package scalanes.mutable.mappers

import scalanes.mutable._

class Mapper001(
  override val prgRom: Vector[UInt8],
  override val chrRom: Vector[UInt8],
  prgRamSize: Int,
  private val registers: Array[UInt8],
  private var shiftReg: UInt8,
  private var writeCounter: Int
) extends Mapper {

  override type Self = Mapper001

  override val prgRam: Array[UInt8] = Array.fill(prgRamSize)(0x00)

  override def prgBankMaps: List[BankMap] = createPrgBankMaps(registers)

  override def chrBankMaps: List[BankMap] = createChrBankMaps(registers)

  override def prgWrite(address: UInt16, d: UInt8): Self = {
    if (address < 0x8000)
      prgRam.update(address - 0x6000, d)
    // Write to any address in 0x8000-0xFFFF
    else if (address & 0x8000) {
      // Bit 7 is set -> reset shift register and write counter, and activate PRG mode 3
      if (d & 0x80) {
        registers.update(0, registers(0) | 0x0C)
        shiftReg = 0x0
        writeCounter = 0
      } else {
        // Write a bit into shift register
        val shiftReg2 = ((d & 1) << 4) | (shiftReg >> 1)
        val writeCounter2 = writeCounter + 1
        // Shift register is completed, update registers
        if (writeCounter2 == 5) {
          val regIndex = (address >> 13) & 0x03
          registers.update(regIndex, shiftReg2)
          shiftReg = 0x0
          writeCounter = 0
          // Continue with writing to shift register
        } else {
          shiftReg = shiftReg2
          writeCounter = writeCounter2
        }
      }
    }
    this
  }

  override def chrWrite(address: UInt16, d: UInt8): Self = this

  override def reset: Self = this

  private def createPrgBankMaps(registers: Array[UInt8]): List[BankMap] = {
    val prgMode = (registers(0) >> 2) & 0x03
    // PRG mode 2 (16KB): fix first bank at $8000 and switch 16 KB bank at $C000
    if (prgMode == 2)
      List(BankMap.map16kB(0), BankMap.map16kB(registers(3) & 0xF))
    // PRG mode 3 (16KB): fix last bank at $C000 and switch 16 KB bank at $8000
    else if (prgMode == 3)
      List(BankMap.map16kB(registers(3) & 0xF), BankMap.map16kB(0xF))
    // PRG mode 0 or 1 (32KB): switch 32 KB at $8000, ignoring low bit of bank number
    else
      List(BankMap.map32kB((registers(3) & 0xF) >> 1))
  }

  private def createChrBankMaps(registers: Array[UInt8]): List[BankMap] =
    // 0: switch 8 KB at a time; 1: switch two separate 4 KB banks
    if (registers(0) & 0x10)
      List(BankMap.map4kB(registers(1) & 0x1F), BankMap.map4kB(registers(2) & 0x1F))
    else
      List(BankMap.map8kB((registers(1) >> 1) & 0xF))
}

object Mapper001 {
}
