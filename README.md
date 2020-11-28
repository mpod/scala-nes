# scala-nes
NES emulator written in Scala using functional programming practices. This means use of immutable data structures and 
libraries like Cats, Cats Effect, FS2, and Monocle. Purpose of the project is to explore how Scala and functional 
programming can be used in implementation of low-level applications.

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
    
## References
* [OneLoneCoder's "NES Emulator From Scratch" YouTube Tutorial](https://www.youtube.com/channel/UC-yuWVUplUJZvieEligKBkA): 
the main source of motivation for starting this project
* [OneLoneCoder/olcNES emulator](https://github.com/OneLoneCoder/olcNES): video tutorial code
* [NesDev](http://nesdev.com/): various NES resources

