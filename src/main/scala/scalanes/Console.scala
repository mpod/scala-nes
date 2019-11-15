package scalanes
import java.io.File
import java.nio.file.Path

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO, Timer}
import fs2.Stream
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
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
    NesState.fromFile2[IO](file, controller).head.evalMap(NesState.reset.runS)
      .flatMap { initial =>
        Stream.unfoldEval(initial) { s =>
          NesState.executeFrame.runS(s).map { next =>
            Option(next, next)
          }
        }
      }.map { next =>
        drawScreen(next, screenCanvas)
        val diff = System.currentTimeMillis() - frameStart
        println(s"Frame generated in $diff ms")
        frameStart = System.currentTimeMillis()
        next
      }
      .drain
      .compile
      .toVector
      .unsafeRunAsyncAndForget()
  }

  def runNesImagePipeline(file: Path): Unit = {
    var frameStart = System.currentTimeMillis()

    NesState.fromFile2[IO](file, controller)
      .head
      .evalMap(NesState.reset.runS)
      .flatMap { initial =>
         Stream.unfoldEval(initial) { s =>
          val start = System.currentTimeMillis()
          NesState.executeFrameCpu.runS(s).map { nextState =>
            val diff = System.currentTimeMillis() - start
            println(s"CPU frame generated in $diff ms by ${Thread.currentThread().getName}")
            Option(nextState, nextState)
          }
        }
      }
      .parEvalMap(3) { s =>
        val ppuStart = System.currentTimeMillis()
        val next = NesState.executeFramePpu
          .runS(s)
          .map { s =>
            val diff = System.currentTimeMillis() - ppuStart
            println(s"PPU frame generated in $diff ms by ${Thread.currentThread().getName}")
            s
          }
        next
      }
      .parEvalMap(1) { s =>
        IO {
          drawScreen(s, screenCanvas)
          val diff = System.currentTimeMillis() - frameStart
          println(s"Frame generated in $diff ms by ${Thread.currentThread().getName}")
          frameStart = System.currentTimeMillis()
          ()
        }
      }
      .drain
      .compile
      .toVector
      .unsafeRunAsyncAndForget()

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
    runNesImagePipeline(file.toPath)
  } else
    System.exit(0)
}
