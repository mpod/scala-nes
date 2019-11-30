# scala-nes
NES emulator in Scala using functional programming techniques. This means use of immutable data structures and 
libraries like Cats, Cats Effect, FS2, and Monocle. Purpose of the project was to explore how Scala and functional 
programming techniques can be used in implementation of low-level applications.

## Screenshots

<p float="left">
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/nestest.png" width="256"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/donkey_kong.png" width="256"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/smb.png" width="256"/></kbd>
</p>

## Running
Make sure to have Java and SBT in your PATH environment variable.

    $ sbt run
    
Repository contains a test image file at location `src/test/resources/nestest.nes`.
    
## Lessons learnt

The basic design idea is to have an immutable data structure for representing the state of the NES emulator and bunch 
of functions for transforming between the states. Natural way for modeling the state is to use Scala case classes. The 
state of every component of NES console, like CPU, PPU, or cartridge, is represented with dedicated case class. Case 
class `NesState` collects the states of the components. Monocle library is used heavily for updating such tree-like
case class structure.  

```scala
case class NesState(ram: Vector[UInt8],
                    cpuState: CpuState,
                    ppuState: PpuState,
                    cartridge: Cartridge,
                    controllerState: ControllerState)
```

Transitions between states are in most cases functions of type `NesState => NesState`. Exception is memory read 
operation which returns some 8-bit value but also it may change the state of NES emulator (e.g. reading of PPU's 
memory mapped registers can change behaviour of PPU unit). The type of read operation is therefore 
`NesState => (NesState, UInt8)`. Such transitions can be modeled with `State` data structure from Cats library. 
`State[S, A]` is a wrapper around a function of type `S => (S, A)`, where `S` is the type of the state and `A` is 
the type of the result. Basic purpose of the `State` data structure is to provide mechanisms for composing state 
transition functions. Therefore, read operation is in this case represented with `State[NesState, UInt8]` type, while 
other operations are in most cases represented with `State[NesState, Unit]` type.

Here is an example of TAX CPU instruction, which transfers value from the accumulator register to the register X and 
sets some status flags. The basic operations, like reading from accumulator register, are also implemented in terms 
of `State` data structure.

```scala
type Op = State[NesState, Unit]
def getA: State[NesState, UInt8] = ???
def setX(d: UInt8): State[NesState, Unit] = ???
def setFlag(flag: CpuFlags, value: Boolean): State[NesState, Unit] = ???

// Transfer accumulator to X
def TAX: Op = for {
  d <- getA
  _ <- setX(d)
  _ <- setFlag(CpuFlags.Z, d == 0x00)
  _ <- setFlag(CpuFlags.N, d & 0x80)
} yield ()
```   

This approach is elegant, but performances are actually terrible. Instead of desirable 16 ms per frame,
such implementation requires more than 1 s per frame. Following paragraphs explain how the implementation was 
improved to around 45 ms per frame, which is still 3 times slower than desirable 16 ms per frame.

Quick profiling showed that millions of `NesState` objects are created and destroyed for every frame. Profiling also 
showed creation of many objets related to the `State` data structure. In other words, composition of the `State` 
objects is expensive, which can be explained by generic and stack-safe implementation of the `State` data structure.

First and the most obvious optimization step was to use `State` monad only for bigger blocks, like complete CPU 
instructions. The most basic operations in that case are implemented as plain functions, which are composed with 
`andThen` method from the standard Scala library. Implementation of the TAX instruction in that case looks like:

```scala
object NesState {
  val cpuState: Lens[NesState, CpuState] = GenLens[CpuState](_.cpuState)
}

object CpuState {
  val x: Lens[CpuState, UInt8] = GenLens[CpuState](_.x)
  val status: Lens[CpuState, UInt8] = GenLens[CpuState](_.status)
}

def setZnFlags(d: UInt8)(s: UInt8): UInt8 = ???

val TAX: Op = 
  State.modify { 
    NesState.cpuState.modify { cpu =>
      val update =
        CpuState.x.set(cpu.a) andThen
        CpuState.status.modify(setZnFlags(cpu.a))
      update(cpu)
    }
  }
```

`Lens` classes are from Monocle library and they provide a functional way for modifying nested case classes. 

Next optimization step was to minimize number of state transformations. First implementation contained various counters
which needed to be updated on every clock tick. On significant number of clock ticks only those counters were updated. 
These counters were used, for example, for tracking the progress of the frame processing. For PPU unit it is defined in
[PPU timing diagram](http://wiki.nesdev.com/w/images/d/d1/Ntsc_timing.png) what to do on each clock tick. From the 
diagram it is visible that one frame takes exactly 262 * 341 = 89342 ticks. Since that number is a constant, it is
possible to compose in advance the function for frame processing:

```scala
def clock(counter: Int, scanline: Int, cycle: Int): Option[State[NesState, NesState]] = ???

val frameTicks: Seq[(Int, Int, Int)] =
  (
    for {
      scanline <- -1 to 260
      cycle    <-  0 to 340
      if scanline != 0 || cycle != 0
    } yield (scanline, cycle)
  ).zipWithIndex.map { case ((scanline, cycle), counter) =>
    (counter, scanline, cycle)
  }

val executeFrame: State[NesState, NesState] =
  frameTicks
    .flatMap { case (counter, scanline, cycle) =>
      clock(counter, scanline, cycle)
    }
    .reduce(_ *> _)
```
 
Method `clock` returns a `State` object (or state transformation function) for every combination of counter, scanline, 
and cycle number. Counter, scanline, and cycle identify one frame clock tick. If no transformation is needed for some 
clock tick then `clock` method returns `None`. All those transformations are combined into one big state transformation 
using `*>` method of `State` data structure (a variant of the `flatMap` method). As a result, `executeFrame` contains 
a state transformation that generates a complete frame. This approach also removes the need for having counters in 
`NesState` case class, which means less of state updates, and consequently faster frame processing. On the other hand, 
this showed to be significantly faster than use of `Monad.whileM` method from Cats library. `Monad.whileM` executes 
an action as long the given condition returns true. In one of the first emulator implementations, `Monad.whileM` was 
used for executing the frame processing function until frame was completed.

Physical NES console outputs one pixel at the time. Implementation is based on fast shift registers which simulate flow
of pixel data. Mutable objects can easily emulate such behaviour, but in case of immutable objects extra work is needed.
Every shift operation results with the change of the state, which means some objects are created and destroyed. In order 
to minimize state changes PPU calculates 8 pixels at the time.

## Unsuccessful optimization

## Unaccepted optimization
    
## Issues
* Current version is slow, it creates around 25 frames per second
* No audio support
* Only support for mappers 000 and 001

## References