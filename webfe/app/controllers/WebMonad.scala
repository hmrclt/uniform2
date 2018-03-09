package ltbs.web

import cats.data.{EitherT, RWST}
import cats.implicits._
import play.api._
import play.api.data.Forms._
import play.api.data.{ Form, Mapping }
import play.api.http.Writeable
import play.api.libs.json.{ Format, JsNull, JsObject, JsValue, Json }
import play.api.mvc.{ Result, Request, AnyContent, AbstractController }
import scala.concurrent.{ Future, ExecutionContext }

package object webmonad {

  // add in State to avoid having to repetitively read/write to the DB
  type DbState = Map[String, JsValue]

  // write out Pages (path state)
  type Path = List[String]

  type WebInner[A] = RWST[Future, (String, Request[AnyContent]), Path, (Path, DbState), A]
  type WebMonad[A] = EitherT[WebInner, Result, A]

}

package webmonad {
  trait WebMonadController extends AbstractController with i18n.I18nSupport {

    implicit def ec: ExecutionContext

    implicit def resultToWebMonad(result: Result): WebMonad[Result] =

    EitherT[WebInner, Result, Result] { RWST { case ((_, r), (path,st)) =>
      (
        List.empty[String],
        (path, st),
        result.asRight[Result]
      ).pure[Future]
    } }

    def run(program: WebMonad[Result])(id: String)(
      load: String => Future[Map[String, JsValue]],
      save: (String, Map[String, JsValue]) => Future[Unit]
    ) = Action.async {
      request => request.session.get("uuid").fold {
        Redirect(".").withSession{
          request.session + ( "uuid" -> java.util.UUID.randomUUID.toString )
        }.pure[Future]
      }{ sessionUUID =>
        load(sessionUUID).flatMap { initialData =>

        def parse(in: String): Map[String, JsValue] =
          Json.parse(in) match { 
            case JsObject(v) => v.toList.toMap
            case _ => throw new IllegalArgumentException
          }

          val data = request.getQueryString("restoreState")
            .fold(initialData)(parse)

          program.value
            .run((id, request), (List.empty[String], data))
            .flatMap {case (_,(path,state),a) =>
              {
                if (state != initialData)
                  save(sessionUUID, state)
                else
                  ().pure[Future]
              }.map { _ =>
                a.fold(identity, identity)
              }
            }
        }
      }
    }

    def formPage[A, B: Writeable](id: String)(mapping: Mapping[A])(
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
              (method, data, targetId) match {
              // nothing in database, step in URI, render empty form
              case ("get", None, `id`) => (
                id.pure[List],
                (path, st),
                Ok(render(id, form, implicitly)).asLeft[A]
              )
              // something in database, step in URI, user revisting old page, render filled in form
              case ("get", Some(json), `id`) => (
                id.pure[List],
                (path, st),
                Ok(render(id, form.fill(json.as[A]), implicitly)).asLeft[A]
              )
              // something in database, not step in URI, pass through
              case ("get", Some(json), _) => (
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
                    (id :: path, st + (id -> Json.toJson(formData))),
                    formData.asRight[Result]
                  )
                }
              )
              // something in database, previous page submitted
              case ("post", Some(json), _) if path.contains(targetId) => (
                id.pure[List],
                (id :: path, st),
                Redirect(s"/$id").asLeft[A]
              )                                 
              // something in database, posting, not step in URI nor previous page -> pass through
              case ("post", Some(json), _) => (
                id.pure[List],
                (id :: path, st),
                json.as[A].asRight[Result]
              )               
              case ("post", _, _) | ("get", _, _) => (
                id.pure[List],
                (path, st),
                Redirect(s"/$id").asLeft[A]
              )                
            }
          }.pure[Future]
        }
      }
    }

  }
}
