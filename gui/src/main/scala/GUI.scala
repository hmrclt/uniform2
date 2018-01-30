import hmrclt._
import freestyle.free._
import freestyle.free.implicits._

import cats.effect.IO

object GUI extends App {
  implicit val interactionHandler = new GDS.Handler[IO] {

    import javax.swing.JOptionPane._

    override def askBoolean(id: String): IO[Boolean] = IO.pure(
      showConfirmDialog(null, id, id, YES_NO_OPTION) == 0
    )

    override def askString(id: String): IO[String] = IO.pure {
      showInputDialog(id)
    }

    override def askInt(id: String): IO[Int] = IO.pure {
      showInputDialog(id).toInt
    }

    override def tell(id: String, msg: String): IO[Unit] = IO.pure {
      showMessageDialog(null, msg)
    }

    override def askAddress(id: String): IO[List[String]] = IO.pure {
      showInputDialog(id).split(",").toList.map{_.trim}
    }
  }

  SDIL.instance.program[GDS.Op].interpret[IO].unsafeRunSync()

}
