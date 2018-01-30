import hmrclt._
import freestyle.free._
import freestyle.free.implicits._

import cats.effect.IO
import org.openqa.selenium._
import java.io.File

object RunTest extends App {

  System.setProperty("webdriver.gecko.driver", "selenium/driver/geckodriver")

  lazy val d = new org.openqa.selenium.firefox.FirefoxDriver
  
  implicit val interactionHandler = new GDS.Handler[IO] {

    import javax.swing.JOptionPane._

    override def askBoolean(id: String): IO[Boolean] = IO.pure{
      Thread.sleep(1000)
      val field = d.findElementById(s"${id}_true")
      field.click
      d.getScreenshotAs(OutputType.FILE).renameTo(new File(s"selenium/$id.png"))
      field.submit
      true      
    }

    override def askString(id: String): IO[String] = IO.pure {
      Thread.sleep(1000)
      val field = d.findElementByName(id)
      field.sendKeys("lorum ipsum")
      d.getScreenshotAs(OutputType.FILE).renameTo(new File(s"selenium/$id.png"))
      field.submit
      "lorum ipsum"
    }

    override def askInt(id: String): IO[Int] = IO.pure {
      Thread.sleep(1000)    
      val field = d.findElementByName(id)
      field.sendKeys("123")
      d.getScreenshotAs(OutputType.FILE).renameTo(new File(s"selenium/$id.png"))
      field.submit
      123
    }

    override def tell(id: String, msg: String): IO[Unit] = IO.pure {
      Thread.sleep(1000)    
      d.getScreenshotAs(OutputType.FILE).renameTo(new File(s"selenium/$id.png"))
      d.findElementById("continue-button").click
      ()
    }

    override def askAddress(id: String): IO[List[String]] = IO.pure {
      ???
    }
  }

  IO.pure {
    d.get("http://localhost:9000/")
  }.flatMap { _ =>
    SDIL.instance.program[GDS.Op].interpret[IO]
  }.flatMap { _ =>
    IO.pure {
      d.close
    }
  }.unsafeRunSync()
}