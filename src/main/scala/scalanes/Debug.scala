package scalanes

import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color

object Debug {

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

}
