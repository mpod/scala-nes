package scalanes
import java.nio.file.{Path, Paths}

import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO}
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectBinding, StringBinding}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{HBox, Region, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text

import scala.concurrent.ExecutionContext

object Console extends JFXApp {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val nestestRom: Path = Paths.get(getClass.getResource("/nestest.nes").toURI)
  val (controller, loaded) = (
    for {
      controller <- Ref.of[IO, UInt8](0x00)
      loaded <- NesState.fromFile[IO](Paths.get("screen.nes"), controller)
    } yield (controller, loaded)
  ).unsafeRunSync()
  require(loaded.size == 1)
  val nesState: ObjectProperty[NesState] = ObjectProperty(NesState.reset.runS(loaded.head).unsafeRunSync())

  def ramInfo(s: NesState, address: Int, rows: Int, columns: Int): String =
    (0 until rows).map { i =>
      val rowAddress = address + i * columns
      (0 until columns).foldLeft(s"$$${hex(rowAddress, 4)}:") { case (acc, j) =>
        val cellAddress = rowAddress + j
        val cell = if (cellAddress >= 0x6000)
          s.cartridge.prgRead(cellAddress)
        else
          s.ram(cellAddress)
        acc + " " + hex(cell, 2)
      }
    }.mkString("\n")

  def cpuStateInfo(s: NesState): String = {
    val status = s.cpuState.status
    val flagN = if (CpuFlags.N.bit & status) "N" else "n"
    val flagV = if (CpuFlags.V.bit & status) "V" else "v"
    val flagU = "-"
    val flagB = if (CpuFlags.B.bit & status) "B" else "b"
    val flagD = if (CpuFlags.D.bit & status) "D" else "d"
    val flagI = if (CpuFlags.I.bit & status) "I" else "i"
    val flagZ = if (CpuFlags.Z.bit & status) "Z" else "z"
    val flagC = if (CpuFlags.C.bit & status) "C" else "c"
    s"""STATUS:  $flagN $flagV $flagU $flagB $flagD $flagI $flagZ $flagC [$$${hex(status, 2)}]
       |PC:      $$${hex(s.cpuState.pc, 4)}
       |A:       $$${hex(s.cpuState.a, 2)}
       |X:       $$${hex(s.cpuState.x, 2)}
       |Y:       $$${hex(s.cpuState.y, 2)}
       |Stack P: $$${hex(s.cpuState.stkp, 2)}
       |""".stripMargin
  }

  def oamInfo(s: NesState): String = {
    s.ppuState.spritesState.oam.zipWithIndex.map { case (e, i) =>
      s"${hex(i, 2)} (${e.x}, ${e.y}) ID: ${hex(e.id, 2)} AT: ${hex(e.attribute, 2)}"
    }.take(20).mkString("\n")
  }

  val cpuState: StringBinding = Bindings.createStringBinding(
    () => Option(nesState.value).map(cpuStateInfo).getOrElse(""),
    nesState
  )
  val ram: StringBinding = Bindings.createStringBinding(
    () => Option(nesState.value).map(ramInfo(_, 0x6000, 16, 16)).getOrElse(""),
    nesState
  )
  val asmAt: StringBinding = Bindings.createStringBinding(
    () => {
      Option(nesState.value)
        .map(s => Cpu.disassemble.runA(s).unsafeRunSync())
        .map { case (address, instr) => s"$$${hex(address, 4)}: $instr" }
        .getOrElse("")
    },
    nesState
  )
  val asmAfter: StringBinding = Bindings.createStringBinding(
    () => {
      Option(nesState.value).map(s =>
          Cpu.disassemble(20).runA(s).unsafeRunSync().drop(1).map { case (address, instr) => s"$$${hex(address, 4)}: $instr" }.mkString("\n")
      ).getOrElse("")
    },
    nesState
  )

  val oamInfo: StringBinding = Bindings.createStringBinding(
    () => {
      Option(nesState.value).map(s => oamInfo(s)).getOrElse("")
    },
    nesState
  )

  def drawPatterns(nesState: NesState, patternTableIndex: Int, palette: Int, canvas: Canvas): Unit = {
    val gc = canvas.graphicsContext2D
    for {
      i <- 0 until 16
      j <- 0 until 16
      offset = i * 256 + j * 16
      row <- 0 until 8
      tileLsb = Cartridge.ppuRead(patternTableIndex * 0x1000 + offset + row + 0x0000).runA(nesState).unsafeRunSync()
      tileMsb = Cartridge.ppuRead(patternTableIndex * 0x1000 + offset + row + 0x0008).runA(nesState).unsafeRunSync()
      col <- 0 until 8
      shiftedTileLsb = tileLsb << col
      shiftedTileMsb = tileMsb << col
      pixel = ((shiftedTileMsb & 0x80) >> 6) | ((shiftedTileLsb & 0x80) >> 7)
      color = Ppu.getColor(palette, pixel)(nesState.ppuState)
    } yield {
      gc.fill = Color.rgb(color.r, color.g, color.b)
      gc.fillRect(j * 8 + col, i * 8 + row, 1, 1)
    }
  }

  def drawScreen(nesState: NesState, canvas: Canvas): Unit = {
    val gc = canvas.graphicsContext2D

    for {
      (row, i) <- nesState.ppuState.pixels.zipWithIndex
      (color, j) <- row.zipWithIndex
    } yield {
      gc.fill = Color.rgb(color.r, color.g, color.b)
      gc.fillRect(2 * j, 2 * i, 2, 2)
    }
  }

  val screenCanvas = new Canvas(256 * 2, 240 * 2)
  val patternsLeftCanvas = new Canvas(16 * 8, 16 * 8)
  val patternsRightCanvas = new Canvas(16 * 8, 16 * 8)
  nesState.onChange((_, _, state: NesState) => {
    drawPatterns(state, 0, 0, patternsLeftCanvas)
    drawPatterns(state, 1, 0, patternsRightCanvas)
    drawScreen(state, screenCanvas)
  })
  drawPatterns(nesState.value, 0, 0, patternsLeftCanvas)
  drawPatterns(nesState.value, 1, 0, patternsRightCanvas)
  drawScreen(nesState.value, screenCanvas)

  val screen: ObjectBinding[Vector[Vector[Color]]] = Bindings.createObjectBinding(
    () => {
      Option(nesState.value)
        .map(_.ppuState.pixels).map(_.map(_.map(c => Color.rgb(c.r, c.g, c.b))))
        .getOrElse(Vector.empty)
    },
    nesState
  )

  val hSpacer = new Region
  hSpacer.setPrefWidth(16)

  val vSpacer = new Region
  vSpacer.setPrefHeight(16)

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
          case KeyCode.SPACE =>
            val start = System.currentTimeMillis()
            nesState.value = NesState.executeFrame.runS(nesState.value).unsafeRunSync()
            val duration = System.currentTimeMillis() - start
            println(s"Frame generated in ${duration / 1000}s ${duration % 1000}ms")
          case KeyCode.R =>
            nesState.value = Cpu.reset.runS(nesState.value).unsafeRunSync()
          case _ =>
        }
      }
      root = new HBox {
        style =
          """-fx-font-size: 16pt;
            |-fx-font-family: monospace;
            |-fx-padding: 1em;
            |""".stripMargin
        children = Seq(
          new VBox {
            children = Seq(
              screenCanvas,
              vSpacer,
              new HBox {
                children = Seq(patternsLeftCanvas, hSpacer, patternsRightCanvas)
              },
              new Text {
                text = "\nSPACE - next frame, R - reset"
              }
            )
          },
          new VBox {
            style = "-fx-padding: 0 0 0 1em;"
            children = Seq(
              new Text {
                text <== cpuState
              },
              new Text {
                fill = Color.Blue
                text <== asmAt
              },
              new Text {
                text <== asmAfter
              }
            )
          }
        )
      }
    }
  }
}
