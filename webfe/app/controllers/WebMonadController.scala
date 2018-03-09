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

import ltbs.web.webmonad._

@Singleton class SDILController @Inject()(
  cc: ControllerComponents
)(implicit val ec: ExecutionContext) extends AbstractController(cc) with i18n.I18nSupport with WebMonadController {

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

  implicit val interactionHandler = new GDS.Handler[WebMonad] {

    override def askBoolean(id: String): WebMonad[Boolean] = {
      val booleanMapping: Mapping[Boolean] = optional(boolean).
        verifying("error.missingvalue", {_.isDefined}).
        transform(_.getOrElse(false), Some(_))

      formPage(id){booleanMapping}{ (a,b,r) =>
        implicit val request = r
          gdspages.boolean(a,b)
      }
    }

    override def askString(id: String): WebMonad[String] = formPage(id)(text){
      (a,b,r) =>
      implicit val request = r
      gdspages.string(a,b)
    }

    override def askInt(id: String): WebMonad[Int] = formPage(id)(number){
      (a,b,r) =>
      implicit val request = r
      gdspages.int(a,b)
    }

    override def tell(id: String, msg: String): WebMonad[Unit] = {

      EitherT[WebInner, Result, Unit] {
        RWST { case ((targetId, r), (path, st)) =>

          implicit val request: Request[AnyContent] = r

          st.get(id) match {
            case Some(_) => (
              id.pure[List],
              (id :: path, st),
              ().asRight[Result]
            ).pure[Future]
            case None if (request.method.toLowerCase == "post") => (
              id.pure[List],
              (id :: path, st + (id -> JsNull)),
              ().asRight[Result]
            ).pure[Future]
            case None => (
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

  def index(id: String) = run(
    SDIL.instance.program[GDS.Op].interpret[WebMonad].flatMap{_ => Ok("Fin")}
  )(id)(dataGet,dataPut)

}
