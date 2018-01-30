package controllers

import javax.inject._

import play.api._
import play.api.mvc._
import hmrclt._
import freestyle.free._
import freestyle.free.implicits._
import scala.collection.mutable.{Map => MMap}
import cats.implicits._
import play.api.data.{ Form, Mapping }
import play.api.data.Forms._
import scala.concurrent.Future
import views.{html => gdspages}
import play.api.http.Writeable

@Singleton class HomeController @Inject()(
  cc: ControllerComponents
) extends AbstractController(cc) with i18n.I18nSupport {

  type ActionResponse[A] = Either[Action[AnyContent], A]

  val data = MMap.empty[String, Any]

  implicit val interactionHandler = new GDS.Handler[ActionResponse] {

    override def askBoolean(id: String): ActionResponse[Boolean] = {

      val booleanMapping: Mapping[Boolean] = optional(boolean).
        verifying("error.missingvalue", {_.isDefined}).
        transform(_.getOrElse(false), Some(_))

      val form = Form(single(id -> booleanMapping))

      data.get(id) match {
        case Some(v: Boolean) => v.asRight
        case _ => Action { implicit request =>
          if (request.method.toLowerCase == "post") {
            form.bindFromRequest.fold(
              formWithErrors => BadRequest(gdspages.boolean(id, formWithErrors)),
              formData => {
                data.put(id, formData)
                Redirect(request.uri)
              }
            )
          } else {
            Ok(gdspages.boolean(id, form))
          }
        }.asLeft
      }
    }

    override def askString(id: String): ActionResponse[String] = {
      val form = Form(single(id -> text))

      data.get(id) match {
        case Some(v: String) => v.asRight
        case _ => Action { implicit request =>
          if (request.method.toLowerCase == "post") {
            form.bindFromRequest.fold(
              formWithErrors => BadRequest(gdspages.string(id, formWithErrors)),
              formData => {
                data.put(id, formData)
                Redirect(request.uri)
              }
            )
          } else {
            Ok(gdspages.string(id, form))
          }
        }.asLeft
      }
    }

    override def askInt(id: String): ActionResponse[Int] = {
      val form = Form(single(id -> number))

      data.get(id) match {
        case Some(v: Int) => v.asRight
        case _ => Action { implicit request =>
          if (request.method.toLowerCase == "post") {
            form.bindFromRequest.fold(
              formWithErrors => BadRequest(gdspages.int(id, formWithErrors)),
              formData => {
                data.put(id, formData)
                Redirect(request.uri)                
              }
            )
          } else {
            Ok(gdspages.int(id, form))
          }
        }.asLeft
      }
    }

    override def tell(id: String, msg: String): ActionResponse[Unit] = {
      data.get(id) match {
        case Some(_) => ().asRight
        case _ => Action { implicit request =>
          data.put(id, ())
          Ok(gdspages.tell(id, msg))
        }.asLeft
      }
    }
    override def askAddress(id: String): ActionResponse[List[String]] = ???
  }

  implicit val interactionHandler2 = new SDILInteraction.Handler[ActionResponse] {
    override def askLitres(id: String): ActionResponse[(Long, Long)] = ???
  }


  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index(): Action[AnyContent] =

    SDIL.instance.program[GDS.Op].interpret[ActionResponse] match {
      case Left(a) => a
      case Right(b) => Action { implicit request =>
        data.clear()
        Redirect(request.uri)
      }
    }

}
