package hmrclt 

import cats.{Applicative, Monad}
import cats.kernel.Group
import freestyle.free._

import language.higherKinds


case class Company(utr: String, address: List[String])

@free trait GDS {
  def askBoolean(id: String): FS[Boolean]
  def askString(id: String): FS[String]
  def askAddress(id: String): FS[List[String]]
  def askInt(id: String): FS[Int]
  def tell(id: String, msg: String): FS[Unit]
  //def companyLookup(id: String): FS[Company]
  //def ask[A](id: String): FS[A]
}

@free trait SDILInteraction {
  def askLitres(id: String): FS[(Long, Long)]
}

@module trait SDIL {
  val gds: GDS
//  val sdil: SDILInteraction
  import cats.implicits._

  // def askOptLitres[F[_]](id: String) = for {
  //   yn <- gds.askBoolean(s"${id}-yn")
  //   qtyl <- if (yn) sdil.askLitres(s"${id}-litres") else (0L,0L).pure[FS]
  // } yield (qtyl)

  /**
    * p = CSD produced (Litres)
    * b = CSD copacked by others (Litres)
    * c = copacks for others (T/F)
    * i = is an importer (T/F)
    * 
    * Mandatory := p + b ≥ 1M ∨ c ∨ i
    * Voluntary := p + b < 1M ∧ b ≠ 0
    */  
  def liabilityCalc(p: Int, b: Int, c: Boolean, i: Boolean): Set[String] = Set(
    if (p + b >= 1000000 || c || i) Some("Mandatory") else None,
    if (p + b < 1000000 && b != 0) Some("Voluntary") else None
  ).flatten

  def program[F[_]]= for {
    p <- gds.askInt(     "produced")
    b <- gds.askInt(     "copackedByOthers")
    c <- gds.askBoolean( "copacks")
    i <- gds.askBoolean( "imports")
    _ <- gds.tell(       "output", liabilityCalc(p,b,c,i).toString)
  } yield ()

//   def registration[F[_]]= for {
// //    company <- gds.companyLookup("producer")
//     manufactures <- askOptLitres("manufactures")
//     imports <- askOptLitres("imports")
//     copacks <- askOptLitres("copacks-yn")
//     _ <- gds.tell("boo", "Boo!")
//   } yield ()

}
