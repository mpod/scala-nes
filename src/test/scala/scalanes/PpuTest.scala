package scalanes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PpuTest extends AnyFlatSpec with Matchers {
  "Loopy" should "extract coarse x" in {
    Loopy.coarseX(0xff) shouldBe 0x1f
  }

  it should "extract coarse y" in {
    Loopy.coarseY(0xff0) shouldBe 0x1f
  }

  it should "extract nametable x" in {
    Loopy.nametableX(0x400) shouldBe 0x1
  }

  it should "extract nametable y" in {
    Loopy.nametableY(0x800) shouldBe 0x1
  }

  it should "extract fine y" in {
    Loopy.fineY(0x7000) shouldBe 0x7
  }

  it should "set coarse x" in {
    (Loopy.setCoarseX(0x17) _ andThen Loopy.coarseX)(0xe5) shouldBe 0x17
  }

  it should "set coarse y" in {
    (Loopy.setCoarseY(0x17) _ andThen Loopy.coarseY)(0xaf) shouldBe 0x17
  }

  it should "set nametable x" in {
    (Loopy.setNametableX(0x1) _ andThen Loopy.nametableX)(0x3c) shouldBe 0x1
  }

  it should "set nametable y" in {
    (Loopy.setNametableY(0x1) _ andThen Loopy.nametableY)(0xffff) shouldBe 0x1
  }

  it should "set fine y" in {
    (Loopy.setFineY(0x7) _ andThen Loopy.fineY)(0xabcd) shouldBe 0x7
  }

}
