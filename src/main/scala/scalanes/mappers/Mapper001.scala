package scalanes.mappers

import scalanes.Mirroring.Mirroring
import scalanes._

/**
 * http://wiki.nesdev.com/w/index.php/INES_Mapper_001
 */
case class Mapper001(prgRom: Vector[UInt8],
                     chrRom: Vector[UInt8],
                     prgRam: Vector[UInt8],
                     prgBankMaps: List[BankMap],
                     chrBankMaps: List[BankMap],
                     registers: Vector[UInt8],
                     shiftReg: UInt8,
                     writeCounter: Int
                    ) extends Mapper {

  import Mapper001._

  type SpecificMapper = Mapper001

  override def prgWrite(address: UInt16, d: UInt8): Mapper001 =
    if (address < 0x8000)
      copy(prgRam = prgRam.updated(address - 0x6000, d))
    // Write to any address in 0x8000-0xFFFF
    else if (address & 0x8000) {
      // Bit 7 is set -> reset shift register and write counter, and activate PRG mode 3
      if (d & 0x80) {
        val registers2 = registers.updated(0, registers(0) | 0x0C)
        Mapper001(prgRom, chrRom, prgRam, createPrgBankMaps(registers2),
          createChrBankMaps(registers2),  registers2, 0x0, 0)
      } else {
        // Write a bit into shift register
        val shiftReg2 = ((d & 1) << 4) | (shiftReg >> 1)
        val writeCounter2 = writeCounter + 1
        // Shift register is completed, update registers
        if (writeCounter2 == 5) {
          val regIndex = (address >> 13) & 0x03
          val registers2 = registers.updated(regIndex, shiftReg2)
          Mapper001(prgRom, chrRom, prgRam, createPrgBankMaps(registers2),
            createChrBankMaps(registers2), registers2, 0x0, 0)
        // Continue with writing to shift register
        } else
          copy(shiftReg = shiftReg2, writeCounter = writeCounter2)
      }
    } else
      this

  override def chrWrite(address: UInt16, d: UInt8): Mapper001 = this

  // TODO: Implement it
  override def reset: Mapper001 = this
}

object Mapper001 {

  def apply(prgRom: Vector[UInt8], chrRom: Vector[UInt8], prgRamSize: Int) = {
    val registers = Vector(0x0C, 0x00, 0x00, 0x00)
    new Mapper001(
      prgRom,
      chrRom,
      Vector.fill(prgRamSize)(0x00),
      createPrgBankMaps(registers),
      createChrBankMaps(registers),
      registers,
      0x00,
      0
    )
  }

  private def extractMirroring(registers: Vector[UInt8]): Mirroring = {
    val mode = registers(0) & 0x03
    if (mode == 0)
      Mirroring.OneScreenLowerBank
    else if (mode == 1)
      Mirroring.OneScreenUpperBank
    else if (mode == 2)
      Mirroring.Vertical
    else
      Mirroring.Horizontal
  }

  private def createPrgBankMaps(registers: Vector[UInt8]): List[BankMap] = {
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

  private def createChrBankMaps(registers: Vector[UInt8]): List[BankMap] =
    // 0: switch 8 KB at a time; 1: switch two separate 4 KB banks
    if (registers(0) & 0x10)
      List(BankMap.map4kB(registers(1) & 0x1F), BankMap.map4kB(registers(2) & 0x1F))
    else
      List(BankMap.map8kB((registers(1) >> 1) & 0xF))

}
