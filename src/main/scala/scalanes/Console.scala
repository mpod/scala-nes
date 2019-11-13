package scalanes
import java.io.File
import java.nio.file.Path

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO}
import fs2.Stream
import javafx.collections.ObservableList
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.PixelFormat
import scalafx.scene.layout.{Region, VBox}
import scalafx.scene.text.Text
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter

import scala.concurrent.ExecutionContext

object Console extends JFXApp {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val controller: Ref[IO, UInt8] = Ref.of[IO, UInt8](0x00).unsafeRunSync()
  val screenCanvas: Canvas = new Canvas(256 * 2, 240 * 2)
  val fileChooser: FileChooser = new FileChooser {
    title = "Select NES image"
  }
  val hSpacer = new Region
  val vSpacer = new Region

  hSpacer.setPrefWidth(16)
  vSpacer.setPrefHeight(16)

  def runNesImage(file: Path): Unit = {
    val loop: Stream[IO, NesState] = for {
      loaded <- NesState.fromFile2[IO](file, controller).take(1)
      initial <- Stream.eval(NesState.reset.runS(loaded))
      next <- Stream.unfoldEval(initial) { s =>
        val start = System.currentTimeMillis()
        NesState.executeFrame.runS(s).map { nextState =>
          val diff = System.currentTimeMillis() - start
          println(s"Frame generated in ${diff / 1000}s and ${diff % 1000}ms")
          Option(nextState, nextState)
        }
      }
      _ <- Stream.eval(IO.async[Unit] { cb =>
        drawScreen(next, screenCanvas)
        cb(Right(()))
      })
    } yield next

    loop.drain.compile.toVector.unsafeRunAsyncAndForget()
  }

  def drawScreen(nesState: NesState, canvas: Canvas): Unit = {
    val gc = canvas.graphicsContext2D
    val pw = gc.pixelWriter
    val pixelFormat = PixelFormat.getIntArgbInstance

    pw.setPixels(0, 0, 256 * 2, 240 * 2, pixelFormat, nesState.ppuState.pixels, 0, 256 * 2)
  }

  stage = new PrimaryStage {
    title = "ScalaNES console"
    scene = new Scene(600, 400) {
      onKeyPressed = { event =>
        event.getCode match {
          case KeyCode.X =>
            controller.modify(x => (x | 0x80, x)).unsafeRunSync()  // A
          case KeyCode.Z =>
            controller.modify(x => (x | 0x40, x)).unsafeRunSync()  // B
          case KeyCode.A =>
            controller.modify(x => (x | 0x20, x)).unsafeRunSync()  // Select
          case KeyCode.S =>
            controller.modify(x => (x | 0x10, x)).unsafeRunSync()  // Start
          case KeyCode.UP =>
            controller.modify(x => (x | 0x08, x)).unsafeRunSync()  // Up
          case KeyCode.DOWN =>
            controller.modify(x => (x | 0x04, x)).unsafeRunSync()  // Down
          case KeyCode.LEFT =>
            controller.modify(x => (x | 0x02, x)).unsafeRunSync()  // Left
          case KeyCode.RIGHT =>
            controller.modify(x => (x | 0x01, x)).unsafeRunSync()  // Right
          case _ =>
        }
      }
      root = new VBox {
        style =
          """-fx-font-size: 16pt;
            |-fx-font-family: monospace;
            |-fx-padding: 1em;
            |""".stripMargin
        children = Seq(
          screenCanvas,
          vSpacer,
          new Text {
            text =
              """X - A                       Up
                |Z - B                Left        Right
                |A - Select                 Down
                |S - Start
                |""".stripMargin
          }
        )
      }
    }
  }

  val file: File = fileChooser.showOpenDialog(stage)
  if (file != null) {
    runNesImage(file.toPath)
  } else
    System.exit(0)
}
