import hmrclt._
import freestyle.free._
import freestyle.free.implicits._

import cats.implicits._
import cats.data.Writer

object LogicTable extends App {

  type TableState[A] = cats.data.WriterT[List, List[String], A]
  //type TableState[A] = Writer[List[String], List[A]]

  implicit class RichList[A](l: List[A]) {

    def picks(id: String): TableState[A] =
      l.map{x =>
        x.pure[TableState]
          .tell(List(s"Pick $id: $x"))
      }.combineAll
  }

  implicit val interactionHandler = new GDS.Handler[TableState] {

    override def askBoolean(id: String): TableState[Boolean] = {
      List(true, false).picks(id)
    }

    override def askString(id: String): TableState[String] =
      List("a", "b", "c").picks(id)

    override def askInt(id: String): TableState[Int] =
      List(0,250000,500000,750000,1000000).picks(id)

    override def tell(id: String, msg: String): TableState[Unit] =
      List(())
        .map{x => x.pure[TableState].tell(List(s"Told $msg"))}.combineAll

    override def askAddress(id: String): TableState[List[String]] =
      //Writer.apply(
        List(
          List("12 The Street", "Genericford"),
          List("13 The Street", "Genericford")
        ).picks(id)
      //)
  }

  val out = SDIL.instance.program[GDS.Op].interpret[TableState]
  out.written.zipWithIndex.map { case (steps, i) =>
    println(s"\nScenario ${i+1}")
    steps.foreach{ a => 
      println(s"  $a")
    }

  }



}
