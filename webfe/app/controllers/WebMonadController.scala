package controllers

import cats.data.{EitherT, RWST}
import cats.implicits._
import freestyle.free._
import freestyle.free.implicits._
import hmrclt._
import javax.inject._
import play.api._
import play.api.data.{ Form, Mapping }
import play.api.data.Forms._
import play.api.mvc.{ControllerComponents, AbstractController, Result, Request, AnyContent }
import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.mutable.{Map => MMap}
import views.{html => gdspages}
import play.api.http.Writeable

@Singleton class WebMonadController @Inject()(
  cc: ControllerComponents
)(implicit ec: ExecutionContext) extends AbstractController(cc) with i18n.I18nSupport {

  // CrapDB(TM)
  val _data = MMap.empty[(String,String), Any]

  // simulated DB calls, assumed to be expensive
  def dataGet[A](session: String, id: String): Future[Option[A]] =
    _data.get((session, id)).map{_.asInstanceOf[A]}.pure[Future]

  def dataPut[A](session: String, id: String)(elem: A): Future[Unit] = {
    _data((session, id)) = elem
  }.pure[Future]

  // add in State to avoid having to repetitively read/write to the DB
  case class DbState(
    record: Map[String, String]
  )

  // // write out Pages (path state).
  type Path = List[String]

  type WebInner[A] = RWST[Future, Request[AnyContent], Path, DbState, A]
  type WebMonad[A] = EitherT[WebInner, Result, A]

  implicit val interactionHandler = new GDS.Handler[WebMonad] {

    private def askA[A, B: Writeable](id: String)(mapping: Mapping[A])(
      render: (String, Form[A], Request[AnyContent]) => B
    ): WebMonad[A] = {
      val form = Form(single(id -> mapping))

      EitherT[WebInner, Result, A] {
        RWST { (r, st) =>

          implicit val request: Request[AnyContent] = r
          val session = request.session("uuid")

          dataGet[A](session, id).map { x =>
            x match {
              case Some(v: A) => (List.empty[String], st, v.asRight[Result])
              case _ => {
                val e = if (request.method.toLowerCase == "post") {
                  form.bindFromRequest.fold(
                    formWithErrors => {
                      BadRequest(render(id, formWithErrors, implicitly))
                    },
                    formData => {
                      dataPut(session, id)(formData)
                      Redirect(request.uri)
                    }
                  )
                } else {
                  Ok(render(id, form, implicitly))
                }

                (List.empty[String], st, e.asLeft[A])
              }
            }
          }
        }
      }
    }

    override def askBoolean(id: String): WebMonad[Boolean] = {
      val booleanMapping: Mapping[Boolean] = optional(boolean).
        verifying("error.missingvalue", {_.isDefined}).
        transform(_.getOrElse(false), Some(_))

      askA(id){booleanMapping}{ (a,b,r) =>
        implicit val request = r
          gdspages.boolean(a,b)
      }
    }

    override def askString(id: String): WebMonad[String] = askA(id)(text){
      (a,b,r) =>
      implicit val request = r
      gdspages.string(a,b)
    }

    override def askInt(id: String): WebMonad[Int] = askA(id)(number){
      (a,b,r) =>
      implicit val request = r
      gdspages.int(a,b)
    }

    override def tell(id: String, msg: String): WebMonad[Unit] = ???

    override def askAddress(id: String): WebMonad[List[String]] = ???
  }

  implicit val interactionHandler2 = new SDILInteraction.Handler[WebMonad] {
    override def askLitres(id: String): WebMonad[(Long, Long)] = ???
  }  

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index = Action.async { request =>

    if (request.session.get("uuid").isDefined) {
      SDIL.instance.program[GDS.Op]
        .interpret[WebMonad].value
        .runA(request, DbState(Map.empty))
        .map { _.fold(
          identity,
          _ => Ok("Fin")
        ) }
    } else {
      Redirect(".").withSession{
        request.session + ( "uuid" -> java.util.UUID.randomUUID.toString )
      }.pure[Future]
    }
  }

}
