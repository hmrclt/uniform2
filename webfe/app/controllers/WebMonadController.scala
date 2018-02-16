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
import play.api.libs.json.{ Format, JsNull, JsObject, JsValue, Json }
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
  def dataGet[A](session: String): Future[DbState] = {
    println("Db read")
    _data.getOrElse(session, Map.empty).pure[Future]
  }

  def dataPut[A](session: String, newData: DbState): Future[Unit] = {
    println("Db write")    
    _data(session) = newData
  }.pure[Future]

  // add in State to avoid having to repetitively read/write to the DB
  type DbState = Map[String,JsValue]

  // // write out Pages (path state).
  type Path = List[String]

  type WebInner[A] = RWST[Future, (String, Request[AnyContent]), Path, (Path, DbState), A]
  type WebMonad[A] = EitherT[WebInner, Result, A]

  implicit val interactionHandler = new GDS.Handler[WebMonad] {

    private def askA[A, B: Writeable](id: String)(mapping: Mapping[A])(
      render: (String, Form[A], Request[AnyContent]) => B
    )(implicit f: Format[A]): WebMonad[A] = {
      val form = Form(single(id -> mapping))

      EitherT[WebInner, Result, A] {
        RWST { case ((targetId, r), (path,st)) =>

          implicit val request: Request[AnyContent] = r

          val post = request.method.toLowerCase == "post"
          val method = request.method.toLowerCase
          val data = st.get(id)

          {

            println((method, data, targetId, id))
            (method, data, targetId) match {
              case ("get", None, `id`) => (
                id.pure[List],
                (path, st),
                Ok(render(id, form, implicitly)).asLeft[A]
              )
              case ("get", Some(json), `id`) => (
                id.pure[List],
                (path, st),
                Ok(render(id, form.fill(json.as[A]), implicitly)).asLeft[A]
              )
              case ("get", Some(json), _) => (
                id.pure[List],
                (id :: path, st),
                json.as[A].asRight[Result]
              )
              case ("post", Some(json), _) => (
                id.pure[List],
                (id :: path, st),
                json.as[A].asRight[Result]
              )
              case ("post", _, `id`) => form.bindFromRequest.fold(
                formWithErrors => {
                  (
                    id.pure[List],
                    (path, st),
                    BadRequest(render(id, formWithErrors, implicitly)).asLeft[A]
                  )
                },
                formData => {
                  (
                    id.pure[List],
                    (path, st + (id -> Json.toJson(formData))),
                    formData.asRight[Result]
                  )
                }
              )
              case ("post", _, _) => (
                id.pure[List],
                (path, st),
                Redirect(s"/$id").asLeft[A]
              )
            }


          }.pure[Future]
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

    override def tell(id: String, msg: String): WebMonad[Unit] = {

      EitherT[WebInner, Result, Unit] {
        RWST { case ((targetId, r), (path, st)) =>

          implicit val request: Request[AnyContent] = r

          st.get(id) match {
            case Some(_) =>
              (
                id.pure[List],
                (id :: path, st),
                ().asRight[Result]
              ).pure[Future]
            case None if (request.method.toLowerCase == "post") =>
              (
                id.pure[List],
                (id :: path, st + (id -> JsNull)), 
                ().asRight[Result]
              ).pure[Future]
            case None =>
              (
                id.pure[List],
                (path, st),
                Ok(gdspages.tell(id, msg)).asLeft[Unit]
              ).pure[Future]
          }
        }
      }
    }

    override def askAddress(id: String): WebMonad[List[String]] = ???
  }

  implicit val interactionHandler2 = new SDILInteraction.Handler[WebMonad] {
    override def askLitres(id: String): WebMonad[(Long, Long)] = ???
  }  

  def index(id: String) = Action.async { request =>

    request.session.get("uuid").fold {
      Redirect(".").withSession{
        request.session + ( "uuid" -> java.util.UUID.randomUUID.toString )
      }.pure[Future]
    }{ sessionUUID =>
      dataGet(sessionUUID).flatMap { initialData => 

        def parse(in: String): Map[String, JsValue] =
          Json.parse(in) match { 
            case JsObject(v) => v.toList.toMap
            case _ => throw new IllegalArgumentException
          }

        val data = request.getQueryString("restoreState")
          .fold(initialData)(parse)

        SDIL.instance.program[GDS.Op]
          .interpret[WebMonad].value
          .run((id, request), (List.empty[String], data))
          .flatMap {case (_,(path,state),a) =>
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
