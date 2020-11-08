package scalanes.mutable

import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicInteger

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
  implicit val timer: Timer[IO]               = IO.timer(ExecutionContext.global)

  val controllerRef: ControllerRef = new AtomicInteger(0)
  val screenCanvas: Canvas         = new Canvas(256 * 2, 240 * 2)
  val fileChooser: FileChooser = new FileChooser {
    title = "Select NES image"
  }
  val hSpacer = new Region
  val vSpacer = new Region

  hSpacer.setPrefWidth(16)
  vSpacer.setPrefHeight(16)

  def runNesImage2(file: Path): Unit = {
    var frameStart = System.currentTimeMillis()
    var nes = NesState
      .fromFile[IO](file, controllerRef)
      .head
      .map(NesState.reset)
      .compile
      .toList
      .unsafeRunSync()
      .head

    while (true) {
      nes = NesState.executeFrame(nes)
      val diff = System.currentTimeMillis() - frameStart
      println(s"Frame generated in $diff ms")
      frameStart = System.currentTimeMillis()
    }
  }

  def runNesImage(file: Path): Unit = {
    var frameStart = System.currentTimeMillis()
    NesState
      .fromFile[IO](file, controllerRef)
      .head
      .map(NesState.reset)
      .flatMap { initial =>
        Stream.unfoldEval(initial) { s =>
          val next = NesState.executeFrame(s)
          IO(Option(next, next))
        }
      }
      .parEvalMap(1) { next =>
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
    val gc          = canvas.graphicsContext2D
    val pw          = gc.pixelWriter
    val pixelFormat = PixelFormat.getIntArgbInstance
    val pixelArray  = nesState.ppuState.canvas
    pw.setPixels(0, 0, 256 * 2, 240 * 2, pixelFormat, pixelArray, 0, 256 * 2)
  }

  stage = new PrimaryStage {
    title = "ScalaNES console"
    scene = new Scene(256 * 2, 240 * 2 + 4 * 12) {
      onKeyPressed = { event =>
        event.getCode match {
          case KeyCode.X =>
            controllerRef.getAndUpdate(_ | 0x80) // A
          case KeyCode.Z =>
            controllerRef.getAndUpdate(_ | 0x40) // B
          case KeyCode.A =>
            controllerRef.getAndUpdate(_ | 0x20) // Select
          case KeyCode.S =>
            controllerRef.getAndUpdate(_ | 0x10) // Start
          case KeyCode.UP =>
            controllerRef.getAndUpdate(_ | 0x08) // Up
          case KeyCode.DOWN =>
            controllerRef.getAndUpdate(_ | 0x04) // Down
          case KeyCode.LEFT =>
            controllerRef.getAndUpdate(_ | 0x02) // Left
          case KeyCode.RIGHT =>
            controllerRef.getAndUpdate(_ | 0x01) // Right
          case _ =>
        }
      }
      root = new VBox {
        alignment = Pos.Center
        style = """-fx-font-size: 12px;
                  |-fx-font-family: monospace;
                  |""".stripMargin
        children = Seq(
          screenCanvas,
          new Text {
            text = """X - A                       Up
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
