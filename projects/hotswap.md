---
title: Hotswap
---
[GitHub project](https://github.com/mikeplus64/hotswap).

[Hotswap](http://hackage.haskell.org/package/hotswap) is a simple, high level interface to [plugins](http://hackage.haskell.org/package/plugins) for hotswapping Haskell code in a simple and automatable manner.

### Demo application: gambling.

#### Main.hs
~~~ {.haskell}
import Control.Monad
import System.Plugins.Hotswap
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    inputHandler <- newPlugin "Plugin.o" [] "inputHandler" :: IO (Plugin (IO Bool))
    forever $ do
        r <- runPlugin inputHandler
        when r $ reloadPlugin inputHandler
~~~

#### Plugin.hs
~~~ {.haskell}
module Plugin where
import System.Random (randomRIO)

inputHandler :: IO Bool
inputHandler = do
    putStrLn "Guess a number between 0 and 10. Guess -1 to reload the plugin."
    g <- fmap read getLine :: IO Int

    if g == -1
        then do
            putStrLn "Reloading ..."
            return True
        else do
            r <- randomRIO (0, 10) :: IO Int
            if g == r
                then putStrLn "Congratulations! You win nothing!"
                else putStrLn "Wrong! You lose nothing, but bring shame to your people."
            return False
~~~

Then, to compile and run this, I run in a shell:

~~~ {.bash}
~/code/projects/hotswap/examples> ghc --make -O2 -threaded Main
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking Main ...
~/code/projects/hotswap/examples> ghc Plugin
[1 of 1] Compiling Plugin           ( Plugin.hs, Plugin.o )
~/code/projects/hotswap/examples> ./Main # example run:
Guess a number between 0 and 10. Guess -1 to reload the plugin.
5
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
4
Congratulations! You win nothing!
Guess a number between 0 and 10. Guess -1 to reload the plugin.
3
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
5
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
6
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
1
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
-1
Reloading ...
Guess a number between 0 and 10. Guess -1 to reload the plugin.
^Z
zsh: suspended  ./Main
~~~

Pretty mundane. But, lets say I want to change what happens when you guess incorrectly.

~~~ {.bash}
~/code/projects/hotswap/examples> sed -i -e "s/else putStrLn.*/else putStrLn \"HAHA, YOU'RE HORRIBLE\"/g" Plugin.hs
~/code/projects/hotswap/examples> ghc Plugin
[1 of 1] Compiling Plugin           ( Plugin.hs, Plugin.o )
~/code/projects/hotswap/examples> fg
[1]  + continued  ./Main
5
Wrong! You lose nothing, but bring shame to your people.
Guess a number between 0 and 10. Guess -1 to reload the plugin.
-1
Reloading ...
Guess a number between 0 and 10. Guess -1 to reload the plugin.
4
HAHA, YOU'RE HORRIBLE
Guess a number between 0 and 10. Guess -1 to reload the plugin.
~~~

And there we have it, hotswapping!

### Todo
* Entirely automatic hotswapping:
    * Specify a source file; it gets compiled for you, and is watched for changes
    * A good, cross platform and fast file watching library.

* More examples!
