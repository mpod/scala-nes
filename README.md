# scala-nes
NES emulator in Scala using functional programming techniques. This means use of immutable data structures and 
libraries like Cats, Cats Effect, FS2, and Monocle. 

## Screenshots

<img src="https://raw.github.com/mpod/scala-nes/master/docs/nestest.png"/>
<img src="https://raw.github.com/mpod/scala-nes/master/docs/donkey_kong.png"/>
<img src="https://raw.github.com/mpod/scala-nes/master/docs/smb.png"/>

## Running
Make sure to have Java and SBT in your PATH environment variable.

    $ sbt run
    
## Issues
Current version is extremely slow, it generates around 10 frames per second. Bad performance is caused by functional 
programming approach, which generates too many intermediate state objects. However, there are many ideas for 
optimisation.
