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
  val _data = MMap.empty[String, DbState]

  // simulated DB calls, assumed to be expensive
  def dataGet(session: String): Future[DbState] = {
    println("Db read")
    _data.getOrElse(session, Map.empty).pure[Future]
  }

  def dataPut[A](session: String, newData: DbState): Future[Unit] = {
    println("Db write")    
    _data(session) = newData
  }.pure[Future]

  // add in State to avoid having to repetitively read/write to the DB
  type DbState = Map[String,Any]

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

          { st.get(id) match {
              case Some(v: A) => (List.empty[String], st, v.asRight[Result])
              case _ => {
                if (request.method.toLowerCase == "post") {
                  form.bindFromRequest.fold(
                    formWithErrors => {
                      (
                        List.empty[String],
                        st,
                        BadRequest(render(id, formWithErrors, implicitly)).asLeft[A]
                      )
                    },
                    formData => {
                      //dataPut(session, id)(formData)
                      (
                        List.empty[String],
                        st + (id -> formData),
                        Redirect(request.uri).asLeft[A]
                      )
                    }
                  )
                } else {
                  (
                    List.empty[String],
                    st,
                    Ok(render(id, form, implicitly)).asLeft[A]
                  )
                }


              }
            
          }}.pure[Future]
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
  def index(id: String) = Action.async { request =>

    request.session.get("uuid").fold {
      Redirect(".").withSession{
        request.session + ( "uuid" -> java.util.UUID.randomUUID.toString )
      }.pure[Future]
    }{ sessionUUID =>
      dataGet(sessionUUID).flatMap { initialData => 

      SDIL.instance.program[GDS.Op]
        .interpret[WebMonad].value
        .run(request, initialData)
        .flatMap {case (path,state,a) =>
          {
            if (state != initialData)
              dataPut(sessionUUID, state)
            else
              ().pure[Future]
          }.map { _ =>
            a.fold(
              identity,
              _ => Ok("Fin")
            )
          }
        }
      }
    }
  }
}
