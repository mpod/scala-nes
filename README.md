# scala-nes
NES emulator written in Scala using functional programming practices. This means use of immutable data structures and 
libraries like Cats, Cats Effect, FS2, and Monocle. Purpose of the project is to explore how Scala and functional 
programming can be used in implementation of low-level applications.

## Screenshots

<p float="left">
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/nestest.png" width="266"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/donkey_kong.png" width="266"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/smb.png" width="266"/></kbd>
</p>

## Running
Make sure to have Java and SBT in your PATH environment variable.

    $ sbt run
    
Repository contains a test image file at location `src/test/resources/nestest.nes`.
    
## Lessons learnt

The basic design is to have an immutable data structure for representing the state of the NES emulator and bunch 
of functions for state transformations. Natural way for modeling the state is to use Scala case classes that
are organized in tree-like structure. Root of the structure is `NesState` case class which contains references
to the states of specific modules, like CPU, PPU, cartridge, and input controllers. Monocle library is heavily used 
for updating such case class structure.  

```scala
case class NesState(ram: Vector[UInt8],
                    cpuState: CpuState,
                    ppuState: PpuState,
                    cartridge: Cartridge,
                    controllerState: ControllerState)
```

Transitions between states are in most cases functions of type `NesState => NesState`. Exception is memory read 
operation which returns some 8-bit value but it may also change the state of NES emulator (e.g. reading of PPU's 
memory mapped registers can change the behaviour of PPU). The type of the read operation is therefore 
`NesState => (NesState, UInt8)`. Such transitions can be modeled with `State` data structure from Cats library. 
`State[S, A]` is a wrapper around a function of type `S => (S, A)`, where `S` is the type that represents the state and 
`A` is the type of the result. Basic purpose of the `State` data structure is to provide mechanisms for composition of 
state transitions. Read operation in this case can be represented with `State[NesState, UInt8]` type, while 
other operations can be in most cases represented with `State[NesState, Unit]` type.

Here is an example of TAX CPU instruction, which transfers a value from the accumulator register to the register X and 
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
such implementation took 1.5 s per frame. Following paragraphs explain how those 1.5 s were lowered to around 
45 ms per frame.

Quick profiling showed that millions of objects were created and destroyed for every frame. Most of those instances were
of type `NesState` or they were somehow related to the `State` data structure. Each transformation of `NesState` 
instance is coupled with creation of few `State`-related instances that actually perform the transformation. In other 
words, transformations of immutable data structures may be expensive.

First and the most obvious optimization step was to use `State` monad only for bigger blocks, like complete CPU 
instructions. The most basic operations, like reading from and writing to CPU registers, are implemented as plain 
functions and composed with `andThen` method from the standard Scala library. Implementation of the TAX instruction in 
that case looks like:

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
      val update = CpuState.x.set(cpu.a) andThen CpuState.status.modify(setZnFlags(cpu.a))
      update(cpu)
    }
  }
```

`Lens` classes are from Monocle library and they provide a functional way for modifying nested case classes. 

Next optimization step was to remove not needed state transformations. First implementation used various
counters that were part of the state case classes and needed to be updated on every clock tick. 
[PPU timing diagram](http://wiki.nesdev.com/w/images/d/d1/Ntsc_timing.png) defines actions that PPU needs to do 
on specific clock tick. That diagram shows that on many clock ticks PPU is actually idle, it doesn't change its state.
But, since the `scanline` and `cycle` counters were used for identifying the position within PPU timing diagram, it was 
needed to update the state on every clock tick. The goal in this case was to remove `scanline` and `cycle` counters 
from the state case classes. A possible solution was to create the frame processing function in advance, or in other 
words, to replace a loop with function composition.

```scala
def clock(counter: Int, scanline: Int, cycle: Int): Option[State[NesState, NesState]] = ???

val frameTicks: Seq[(Int, Int, Int)] = ???

val executeFrame: State[NesState, NesState] =
  frameTicks
    .flatMap { case (counter, scanline, cycle) =>
      clock(counter, scanline, cycle)
    }
    .reduce(_ *> _)
```
 
The basic idea is to iterate through all frame clock ticks, create a function for that particular clock tick, and to 
combine all those functions into one big function. A frame clock tick is identified with three numbers: `counter`
increments with each clock tick, `scanline` defines a row, while `cycle` defines a column in PPU timing diagram. Method 
`clock` returns a `State` object (or state transformation function) for specified clock tick. If no transformation is 
needed then `clock` method returns `None`. All those transformations are combined with `*>` method of the `State` data 
structure (a variant of the `flatMap` method). As a result, `executeFrame` contains a state transformation for 
a complete frame. The frame processing function doesn't need counters as part of the `NesState` case class any more,
which means faster frame processing. On the other hand, having a fixed transformation showed to be 
significantly faster than dynamically detecting the end of the frame processing by using `Monad.whileM_` method from the
Cats library.

Physical NES console outputs one pixel at a time. Implementation is based on fast shift registers which simulate flow
of pixel data. Mutable data structures can easily emulate such behaviour, but in case of immutable data structures 
extra work is needed, because every shift operation is actually a state change, which means a new shift register is 
created and old one is destroyed. In order to minimize the number of state transformations, PPU processes in this 
emulator implementation 8 pixels at a time. 

Debugging of Super Mario Bros. revealed that the CPU spends significant time in infinite loop. All useful work is done
inside the interrupt routine which is triggered when PPU sets vertical blank flag. During the vertical blanking period 
it is safe for CPU to access and change PPU's data. Optimization idea was to detect the address of the infinite loop 
and to skip processing of the CPU instructions at that address. 

### Unsuccessful optimization
Immutable data structures are suitable for pipelined processing. The idea here is to separate CPU processing from PPU 
processing, and to execute them in a sequence. The separation could be done, in theory, only on scanline level, because
of the sprite zero hit flag. PPU sets that flag on rendering sprite that is at the index 0 in internal sprite array. 
CPU usually checks the flag in the loop, and when the flag becomes `true`, it changes rendering for the rest of the
screen. Such CPU/PPU communication limits the pipeline approach to the scanline level. 

Pipeline implementation however didn't produce any improvements in frame processing time. It looks like the thread 
management overhead was significant. Since this was an interesting but unsuccessful optimization attempt, the 
code changes are left on `pipeline2` branch.

### Unaccepted optimization
Another idea for optimization is to use `Ref` data structure from Cats Effect library. This data structure provides a 
functional access to the mutable instances. Idea was not explored in detail because it would require significant 
refactoring of current implementation. Another reason is that the use of `Ref` data structure is kind of against the 
initial requirement of the project, which is use of immutable data structures.
    
## Issues
* Current version is slow, it creates around 25 frames per second
* No audio support
* Only support for mappers 000 and 001

## References
* [OneLoneCoder's "NES Emulator From Scratch" YouTube Tutorial](https://www.youtube.com/channel/UC-yuWVUplUJZvieEligKBkA): 
the main source of motivation for starting this project
* [OneLoneCoder/olcNES emulator](https://github.com/OneLoneCoder/olcNES): video tutorial code
* [NesDev](http://nesdev.com/): various NES resources

