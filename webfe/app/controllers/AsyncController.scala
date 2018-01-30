package controllers



import cats.data.{Kleisli, EitherT, StateT, WriterT}
import cats.implicits._
import freestyle.free._
import freestyle.free.implicits._
import hmrclt._
import javax.inject._
import play.api._
import play.api.data.{ Form, Mapping }
import play.api.data.Forms._
import play.api.libs.json.JsValue
import play.api.mvc.{ControllerComponents, AbstractController, Result, Request, AnyContent }
import scala.concurrent.{ ExecutionContext, Future }
import scala.collection.mutable.{Map => MMap}
import views.{html => gdspages}
import play.api.http.Writeable

@Singleton class AsyncController @Inject()(
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

  // core data type - either terminate computation with a Left[Result] or
  // continue with a Right[A]
  type FutureOpt[A] = EitherT[Future, Result, A] 

  // add in State to avoid having to repetitively read/write to the DB
  case class State(
    record: Map[String, JsValue]
  )
  
  type WithState[A] = StateT[FutureOpt, State, A]

  // // write out Pages (path state).
  type Pages = List[String]
  type WithPathState[A] = WriterT[WithState, Pages, A]

  // thread the immutable request through the transaction
  type ActionK[A] = Kleisli[FutureOpt, Request[AnyContent], A]

  implicit val interactionHandler = new GDS.Handler[ActionK] {

    private def askA[A, B: Writeable](id: String)(mapping: Mapping[A])(
      render: (String, Form[A], Request[AnyContent]) => B
    ): ActionK[A] = {
      val form = Form(single(id -> mapping))

      Kleisli[FutureOpt, Request[AnyContent], A] { implicit request =>
        val session = request.session("uuid")

          EitherT {
            dataGet[A](session, id).map { x =>
              x match {
                case Some(v: A) => v.asRight[Result]
                case _ =>
                  {if (request.method.toLowerCase == "post") {
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
                  } }.asLeft[A]
              } } }
      }
    }

    override def askBoolean(id: String): ActionK[Boolean] = {
      val booleanMapping: Mapping[Boolean] = optional(boolean).
        verifying("error.missingvalue", {_.isDefined}).
        transform(_.getOrElse(false), Some(_))

      askA(id){booleanMapping}{ (a,b,r) =>
        implicit val request = r
          gdspages.boolean(a,b)
      }
    }

    override def askString(id: String): ActionK[String] = askA(id)(text){
      (a,b,r) =>
      implicit val request = r
      gdspages.string(a,b)
    }

    override def askInt(id: String): ActionK[Int] = askA(id)(number){
      (a,b,r) =>
      implicit val request = r
      gdspages.int(a,b)
    }

    override def tell(id: String, msg: String): ActionK[Unit] = {
      Kleisli[FutureOpt, Request[AnyContent], Unit]{ implicit request =>
        val session = request.session("uuid")

        EitherT[Future, Result, Unit] {
          dataGet[Unit](session, id).flatMap { x =>
            x match {
              case Some(_) =>
                ().asRight[Result].pure[Future]
              case None if (request.method.toLowerCase == "post") =>
                dataPut(session, id)(()).map{ _ => ().asRight[Result] }
              case None => 
                  Ok(gdspages.tell(id, msg)).asLeft[Unit].pure[Future]
            }
          }
        }
      }
    }

    override def askAddress(id: String): ActionK[List[String]] = ???
  }

  implicit val interactionHandler2 = new SDILInteraction.Handler[ActionK] {
    override def askLitres(id: String): ActionK[(Long, Long)] = ???
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
        .interpret[ActionK]
        .run(request)
        .fold(
          identity,
          _ => Ok("Fin")
        )
    } else {
      Redirect(".").withSession{
        request.session + ( "uuid" -> java.util.UUID.randomUUID.toString )
      }.pure[Future]
    }
  }

}
