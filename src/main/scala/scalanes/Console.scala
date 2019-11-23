package scalanes
import java.io.File
import java.nio.file.Path

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO, Timer}
import fs2.Stream
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.image.PixelFormat
import scalafx.scene.layout.{Region, VBox}
import scalafx.scene.text.Text
import scalafx.stage.FileChooser

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

object Console extends JFXApp {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

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
    var frameStart = System.currentTimeMillis()
    NesState
      .fromFile[IO](file, controller)
      .head
      .evalMap(NesState.reset.runS)
      .flatMap { initial =>
        Stream.unfoldEval(initial) { s =>
          NesState.executeFrame.runS(s).map { next =>
            Option(next, next)
          }
        }
      }
      .parEvalMap(2) { next =>
        IO {
          drawScreen(next, screenCanvas)
          val diff = System.currentTimeMillis() - frameStart
          println(s"Frame generated in $diff ms")
          frameStart = System.currentTimeMillis()
          next
        }
      }
      .drain
      .compile
      .toVector
      .unsafeRunAsyncAndForget()
  }

  def drawScreen(nesState: NesState, canvas: Canvas): Unit = {
    def asInt(rgb: Rgb): Int = (rgb.b & 0xFF) | ((rgb.g & 0xFF) << 8) | ((rgb.r & 0xFF) << 16) | (0xFF << 24)
    val gc = canvas.graphicsContext2D
    val pw = gc.pixelWriter
    val pixelFormat = PixelFormat.getIntArgbInstance
    val pixelArray = Array.fill(240 * 2 * 256 * 2)(0)
    val pixelVector = nesState.ppuState.pixels
    for (i <- pixelArray.indices) {
      val row = (i / (256 * 2)) / 2
      val col = (i % (256 * 2)) / 2
      val color = asInt(pixelVector(row * 256 + col))
      pixelArray(i) = color
    }
    pw.setPixels(0, 0, 256 * 2, 240 * 2, pixelFormat, pixelArray, 0, 256 * 2)
  }

  stage = new PrimaryStage {
    title = "ScalaNES console"
    scene = new Scene(256 * 2, 240 * 2 + 4 * 12) {
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
        alignment = Pos.Center
        style =
          """-fx-font-size: 12px;
            |-fx-font-family: monospace;
            |""".stripMargin
        children = Seq(
          screenCanvas,
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
  if (file != null)
    runNesImage(file.toPath)
  else
    System.exit(0)
}
