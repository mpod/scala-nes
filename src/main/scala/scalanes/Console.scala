package scalanes
import java.nio.file.Paths

import cats.Monad
import cats.data.State
import cats.effect.{ContextShift, IO}
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectBinding, StringBinding}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.{HBox, Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text

import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source}

object Console extends JFXApp {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val source: BufferedSource = Source.fromFile("nestest.log")
  val log: ObjectProperty[List[String]] = ObjectProperty(source.getLines.toList)
  source.close()

  val loaded: List[NesState] = NesState.fromFile[IO](Paths.get("nestest2.nes")).unsafeRunSync()
  require(loaded.size == 1)
  val nesState: ObjectProperty[NesState] = ObjectProperty(NesState.reset.runS(loaded.head).value)

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
    s"""STATUS:  $flagN $flagV $flagU $flagB $flagD $flagI $flagZ $flagC
       |PC:      $$${hex(s.cpuState.pc, 4)}
       |A:       $$${hex(s.cpuState.a, 2)}
       |X:       $$${hex(s.cpuState.x, 2)}
       |Y:       $$${hex(s.cpuState.y, 2)}
       |Stack P: $$${hex(s.cpuState.stkp, 2)}
       |""".stripMargin
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
        .map(s => Cpu.disassemble.runA(s).value)
        .map { case (address, instr) => s"$$${hex(address, 4)}: $instr" }
        .getOrElse("")
    },
    nesState
  )
  val asmAfter: StringBinding = Bindings.createStringBinding(
    () => {
      Option(nesState.value).map(s =>
        Cpu.disassemble(20).runA(s).value.drop(1).map { case (address, instr) => s"$$${hex(address, 4)}: $instr" }.mkString("\n")
      ).getOrElse("")
    },
    nesState
  )

  def extractPatterns(nesState: NesState, patternTableIndex: Int, palette: Int): Iterable[Iterable[Color]] = {
    val colors = for {
      i <- 0 until 16
      j <- 0 until 16
      offset = i * 256 + j * 16
      row <- 0 until 8
      tileLsb = Cartridge.ppuRead(patternTableIndex * 0x1000 + offset + row + 0x0000).runA(nesState).value
      tileMsb = Cartridge.ppuRead(patternTableIndex * 0x1000 + offset + row + 0x0008).runA(nesState).value
      col <- 0 until 8
      shiftedTileLsb = tileLsb >> col
      shiftedTileMsb = tileMsb >> col
      pixel = ((shiftedTileMsb & 0x01) << 1) | (shiftedTileLsb & 0x01)
      color = Ppu.getColor(palette, pixel).runA(nesState.ppuState).value
    } yield Color.rgb(color.r, color.g, color.b)
    colors.grouped(16 * 8).toList
  }

  def drawPatterns(patterns: Iterable[Iterable[Color]], canvas: Canvas): Unit = {
    val gc = canvas.graphicsContext2D
    for {
      (row, i) <- patterns.zipWithIndex
      (color, j) <- row.zipWithIndex
    } yield {
      gc.fill = color
      gc.fillRect(i * 2, j * 2, 2, 2)
    }
  }

  val patternsLeft: ObjectBinding[Iterable[Iterable[Color]]] = Bindings.createObjectBinding(
    () => {
      Option(nesState.value).map(extractPatterns(_, 0, 0)).getOrElse(Vector.empty)
    },
    nesState
  )
  val patternsLeftCanvas = new Canvas(16 * 8 * 2, 16 * 8 * 2)
  patternsLeft.onChange((_, _, patterns: Iterable[Iterable[Color]]) => {
    drawPatterns(patterns, patternsLeftCanvas)
  })
  patternsLeftCanvas.graphicsContext2D.fill = Color.Red
  patternsLeftCanvas.graphicsContext2D.fillRect(0, 0, patternsLeftCanvas.width.get, patternsLeftCanvas.height.get)
  drawPatterns(patternsLeft.value, patternsLeftCanvas)

  val patternsRight: ObjectBinding[Iterable[Iterable[Color]]] = Bindings.createObjectBinding(
    () => {
      Option(nesState.value).map(extractPatterns(_, 1, 0)).getOrElse(Vector.empty)
    },
    nesState
  )
  val patternsRightCanvas = new Canvas(16 * 8 * 2, 16 * 8 * 2)
  patternsRightCanvas.graphicsContext2D.fill = Color.Blue
  patternsRightCanvas.graphicsContext2D.fillRect(0, 0, patternsRightCanvas.width.get, patternsRightCanvas.height.get)
  drawPatterns(patternsRight.value, patternsRightCanvas)

  patternsRight.onChange((_, _, patterns: Iterable[Iterable[Color]]) => {
    for {
      (row, i) <- patterns.zipWithIndex
      (color, j) <- row.zipWithIndex
    } yield patternsRightCanvas.graphicsContext2D.pixelWriter.setColor(i, j, color)
  })

  val screen: ObjectBinding[Vector[Vector[Color]]] = Bindings.createObjectBinding(
    () => {
      Option(nesState.value)
        .map(_.ppuState.pixels).map(_.map(_.map(c => Color.rgb(c.r, c.g, c.b))))
        .getOrElse(Vector.empty)
    },
    nesState
  )

  val screenCanvas = new Canvas(256 * 2, 240 * 2)

  def boxedCanvas(canvas: Canvas): Pane = new Pane {
    style =
      """
        |-fx-border-color: red;
        |""".stripMargin
    children = Seq(canvas)
  }

  def compare(logLine: String, s: NesState): Boolean = {
    val op = Cpu.getPc.flatMap(pc => Cpu.cpuRead(pc)).runA(s).value
    logLine.contains(s"${hex(s.cpuState.pc, 4)}  ${hex(op, 2)} ") &&
    logLine.contains(s" A:${hex(s.cpuState.a, 2)}") &&
    logLine.contains(s" X:${hex(s.cpuState.x, 2)}") &&
    logLine.contains(s" Y:${hex(s.cpuState.y, 2)}") &&
    (logLine.contains(s" P:${hex(s.cpuState.status, 2)}") || logLine.contains(s" P:${hex(s.cpuState.status & 0xEF, 2)}")) &&
    logLine.contains(s" SP:${hex(s.cpuState.stkp, 2)}")
  }

  stage = new PrimaryStage {
    title = "ScalaNES console"
    scene = new Scene(600, 400) {
      onKeyPressed = { event =>
        if (event.getCode == KeyCode.SPACE) {
//          nesState.value = Monad[State[NesState, *]].whileM_(Cpu.getPc.map(_ != 0xC660))(Cpu.executeNextInstr).runS(s).value
//          nesState.value = (0 to 10000).foldLeft(s)((s, _) => Cpu.executeNextInstr.runS(s).value)
          var i = 0
          val logVector = log.value.toVector
          nesState.value = Monad[State[NesState, *]]
            .whileM_(
              State.get.map {s =>
                val logLine = logVector(i)
                i += 1
                compare(logLine, s)
              }
            )(Cpu.executeNextInstr).runS(nesState.value).value
          val s = nesState.value
          val op = Cpu.getPc.flatMap(pc => Cpu.cpuRead(pc)).runA(s).value
          println("Failed!")
          println(s"Line: $i, ${logVector(i)}")
          println(s"${hex(s.cpuState.pc, 4)}  ${hex(op, 2)} ")
          println(s" A:${hex(s.cpuState.a, 2)}")
          println(s" X:${hex(s.cpuState.x, 2)}")
          println(s" Y:${hex(s.cpuState.y, 2)}")
          println(s" P:${hex(s.cpuState.status, 2)}")
          println(s" SP:${hex(s.cpuState.stkp, 2)}")
        } else if (event.getCode == KeyCode.R)
          nesState.value = Cpu.reset.runS(nesState.value).value
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
              new Text {
                text <== ram
              },
              new HBox {
                children = Seq(boxedCanvas(patternsLeftCanvas), boxedCanvas(patternsRightCanvas))
              },
              new Text {
                text = "\nSPACE - next instruction, R - reset"
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
