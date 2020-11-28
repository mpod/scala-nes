# scala-nes
NES emulator written in Scala.

## Screenshots

<p float="left">
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/nestest.png" width="256"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/donkey_kong.png" width="256"/></kbd>
<kbd><img src="https://raw.github.com/mpod/scala-nes/master/docs/smb.png" width="256"/></kbd>
</p>

## Running
Make sure to have Java and SBT in your PATH environment variable.

    $ sbt assembly
    $ java -jar target/scala-2.12/scala-nes.jar src/test/resources/nestest.nes
    
## Controls

Keyboard controls are:

| Nintendo              | Emulator    |
| --------------------- | ----------- |
| Up, Down, Left, Right | Arrow Keys  |
| Start                 | S           |
| Select                | A           |
| A                     | Z           |
| B                     | X           |
    
## References
* [OneLoneCoder's "NES Emulator From Scratch" YouTube Tutorial](https://www.youtube.com/channel/UC-yuWVUplUJZvieEligKBkA): 
the main source of motivation
* [OneLoneCoder/olcNES emulator](https://github.com/OneLoneCoder/olcNES): video tutorial code
* [NesDev](http://nesdev.com/): various NES resources
* [nestest](http://nickmass.com/images/nestest.nes): famous test ROM 

