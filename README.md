Catcher game in Elm
--------------------

Currently, this is more of an elm WebGL test as its just a few cubes moving around.  I started from [this great blog post](http://elm-lang.org/blog/announce/0.12.3.elm), and am building from there.

I'll be adding more interactive functionality as I learn more about Elm.

To run this demo, [install elm](https://github.com/elm-lang/elm-platform/blob/master/README.md#elm-platform), then run these two commands:

    elm-get install
    elm-server

Then go to [http://localhost:8000/Game.elm](http://localhost:8000/Game.elm) to a spinning cube. Click anywhere on the page to see more cubes fall from the sky! On the right is a view of the current state of the game at any point in time, and you'll see as blocks spawn, then append to the state, and then go away after falling off the screen.