import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.Size
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.W

import scala.language.implicitConversions

package object scalanes {

  type UInt1 = Int
  type UInt2 = Int
  type UInt3 = Int
  type UInt5 = Int
  type UInt8 = Int
  type UInt15 = Int
  type UInt16 = Int

  type Cartridge = Mapper

  // TODO: Explore later...
  type RAM = Vector[UInt8] Refined Size[Equal[W.`65536`.T]]

  implicit def intToBoolean(value: Int): Boolean = value != 0

  def hex(n: Int, d: Int): String = (d - 1 to 0 by -1).map(i => "0123456789ABCDEF"((n >> (i * 4)) & 0xF)).mkString("")
}
