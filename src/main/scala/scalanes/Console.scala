package scalanes
import javafx.scene.input.KeyCode
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.beans.binding.{Bindings, ObjectBinding, StringBinding}
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene
import scalafx.scene.layout.{HBox, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text

object Console extends JFXApp {

  val program = "A20A8E0000A2038E0100AC0000A900186D010088D0FA8D0200EAEAEA"

  def ramInfo(s: NesState, address: Int, rows: Int, columns: Int): String =
    (0 until rows).map { i =>
      val rowAddress = address + i * columns
      (0 until columns).foldLeft(s"$$${hex(rowAddress, 4)}:") { case (acc, j) =>
        val cellAddress = rowAddress + j
        val cell = if (cellAddress >= 0x8000) {
          val mapped = s.cartridge.mapper.mapCpuAddress(cellAddress)
          s.cartridge.prgMem(mapped)
        } else
          s.ram(cellAddress)
        acc + " " + hex(cell, 2)
      }
    }.mkString("\n")

  def cpuStateInfo(s: NesState): String = {
    val status = s.cpuRegisters.status
    val flagN = if (CpuFlags.N.bit & status) "N" else "n"
    val flagV = if (CpuFlags.V.bit & status) "V" else "v"
    val flagU = "-"
    val flagB = if (CpuFlags.B.bit & status) "B" else "b"
    val flagD = if (CpuFlags.D.bit & status) "D" else "d"
    val flagI = if (CpuFlags.I.bit & status) "I" else "i"
    val flagZ = if (CpuFlags.Z.bit & status) "Z" else "z"
    val flagC = if (CpuFlags.C.bit & status) "C" else "c"
    s"""STATUS:  $flagN $flagV $flagU $flagB $flagD $flagI $flagZ $flagC
       |PC:      $$${hex(s.cpuRegisters.pc, 4)}
       |A:       $$${hex(s.cpuRegisters.a, 2)}
       |X:       $$${hex(s.cpuRegisters.x, 2)}
       |Y:       $$${hex(s.cpuRegisters.y, 2)}
       |Stack P: $$${hex(s.cpuRegisters.stkp, 2)}
       |""".stripMargin
  }

  def codeInfo(s: NesState, lines: Int): Seq[(Int, String)] = {
    val pc = s.cpuRegisters.pc
    val i = asmMap.indexWhere(_._1 == pc)

    ((i - lines / 2) until (i + lines / 2))
      .filter(i => i >= 0 && i < asmMap.length)
      .map(asmMap)
      .map { case (address, instr) => address -> s"$$${hex(address, 4)}: $instr"}
  }

  val asmLines: Int = 26

  val nesState: ObjectProperty[NesState] = ObjectProperty(NesState.fromString(program))

  val asmMap: Vector[(UInt16, String)] = Cpu.disassemble(0x0000, 0xFFFF, nesState.value)

  val cpuState: StringBinding = Bindings.createStringBinding(
    () => Option(nesState.value).map(cpuStateInfo).getOrElse(""),
    nesState
  )
  val ram1: StringBinding = Bindings.createStringBinding(
    () => Option(nesState.value).map(ramInfo(_, 0x0000, 16, 16)).getOrElse(""),
    nesState
  )
  val ram2: StringBinding = Bindings.createStringBinding(
    () => Option(nesState.value).map(ramInfo(_, 0x8000, 16, 16)).getOrElse(""),
    nesState
  )
  val asm: ObjectBinding[Seq[(Int, String)]] = Bindings.createObjectBinding[Seq[(Int, String)]](
    () => Option(nesState.value).map(codeInfo(_, asmLines)).getOrElse(Seq.empty),
    nesState
  )
  val asmBefore: StringBinding = Bindings.createStringBinding(
    () => {
      val s = for {
        pc <- Option(nesState.value).map(_.cpuRegisters.pc)
        lines <- Option(asm.value).map(_.takeWhile { case (address, _) => address < pc }.map(_._2).mkString("\n"))
      } yield lines
      s.getOrElse("")
    },
    asm, nesState
  )
  val asmAt: StringBinding = Bindings.createStringBinding(
    () => {
      val s = for {
        pc <- Option(nesState.value).map(_.cpuRegisters.pc)
        line <- Option(asm.value).flatMap(_.find { case (address, _) => address == pc }).map(_._2)
      } yield line
      s.getOrElse("")
    },
    asm, nesState
  )
  val asmAfter: StringBinding = Bindings.createStringBinding(
    () => {
      val s = for {
        pc <- Option(nesState.value).map(_.cpuRegisters.pc)
        lines <- Option(asm.value).map(_.dropWhile { case (address, _) => address <= pc }.map(_._2).mkString("\n"))
      } yield lines
      s.getOrElse("")
    },
    asm, nesState
  )

  stage = new PrimaryStage {
    title = "ScalaNES console"
    scene = new Scene(600, 400) {
      onKeyPressed = { event =>
        val s = nesState.value
        if (event.getCode == KeyCode.SPACE)
          nesState.value = Cpu.executeNextInstr.runS(s).value
        else if (event.getCode == KeyCode.R)
          nesState.value = Cpu.reset.runS(s).value
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
                text <== ram1
              },
              new Text("------------------------------------------------------"),
              new Text {
                text <== ram2
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
                text <== asmBefore
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
