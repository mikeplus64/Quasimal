---
title: A look at QuasiQuotation
desc: Looks at the process of creating QuasiQuoters in relation to my library, QuasiText.
date: May 25, 2012
author: Mike Ledger
description: A QuasiQuotation tutorial.
---
A week or so ago I finished [QuasiText](http://hackage.haskell.org/package/QuasiText), a small Text interpolation library to generate at compile-time expressions with interpolated variables and/or Haskell code.

## Building a QuasiQuoter
Trying to figure out how to build a QuasiQuoter wasn't hard at all. Partly because of the excellent documentation, and partly because of my hard work annoying others in #haskell. 

At the core of a TemplateHaskell splice, as far as expressions go, is the *Q Exp*. A *Q Exp* can be evaluated at compile time using the splice syntax <em>$(TemplateHaskell functions evaluating to a *Q Exp* go here)</em>. A QuasiQuoter is simply a function that takes a string and returns a *Q Exp*, *Q Pat*, *Q Type* or *Q [Dec]*.

For example, to define a nice and silly QuasiQuoter that does nothing but return the string "yeah!!!", we can do:

````haskell
{-# LANGUAGE TemplateHaskell #-}
module SimpleQuoter where
import Language.Haskell.TH.Quote -- gives us QuasiQuoter
import Language.Haskell.TH       -- gives us stuff to return 'Q Exp's.
                                 -- ie. stringL, litE

silly :: QuasiQuoter
silly = QuasiQuoter { quoteExp = \_ -> [| "yeah!!!" |] }
````

You will have noticed the use of the big scary *[| ... |]*s. All these are doing in silly's case is "lifting" the String "yeah!!!" into a *Q Exp*. For information about lifting, you can look [at the docs](http://hackage.haskell.org/packages/archive/template-haskell/2.7.0.0/doc/html/Language-Haskell-TH-Syntax.html) for the Lift typeclass. (note that *[| ... |]* is syntactic sugar for *lift ...*)

Using the functions given by Language.Haskell.TH for creating Q Exps from fairy dust, if we were at some point feeling unsatisfied by string literals in Haskell, we could define:

````haskell
string :: QuasiQuoter
string = QuasiQuoter
    { quoteExp = litE . stringL
        -- note that: quoteExp :: String -> Q Exp
        --            stringL :: String -> Lit
        --            litE :: Lit -> Q Exp
        -- so: litE . stringL = \x -> litE (stringL x)
    }
````

Another way of writing it, using lift instead, could be:

````haskell
string :: QuasiQuoter
string = QuasiQuoter
    { quoteExp = \s -> [| s |]
      -- OR, with Language.Haskell.TH.Syntax imported: quoteExp = lift
    }
````

For trivial examples like this, it doesn't matter what "style" you choose.

Lets forget about QuasiQuoters for a moment, and remember that no Haskell tutorial is complete without a fibonacci sequence function. Lets make a compile-time function to return the code to get the nth fibonacci number.

````haskell
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibsQ :: Q Exp
fibsQ = [| fibs |]

fibQ :: Int -> Q Exp
fibQ = [| fibs !! n |]
````

If we fire up GHCi and type in <em>$(fibQ 10)</em>, we'll get the 10th fibonacci number. All we're really doing is lifting the expression <em>fibs \!\! n</em> to be a <em>Q Exp</em>, then evaluating this <em>Q Exp</em> with <em> $(...) </em>.

Now, back to the thing. At the time I wrote QuasiText, I wanted some nice Text (from Data.Text, the string type all the cool people use) interpolation without any weird unpacking and packing done at runtime (Text recently got "real" Text literals, which makes me feel very awkward about having to pack Strings -> Text). 

Lets say that we want the DSL to look like: "[intp|text goes here $funs $go $here |]", to implement this, we should first make a parser to translate strings into our abstract format.

## Building the parser: QFormat.hs

````haskell
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module QFormat where
import qualified Data.Text as T
import Data.Text (Text)
import Data.Attoparsec.Text
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

type Chunk = Either Text String
-- a chunk is either just raw Text, or the String name of a variable

makeChunks :: Text -> [Chunk]
makeChunks ts = case parseOnly parser ts of
    Right x -> x
    _       -> error "malformed text"
  where
    parser = do
        res <- loop []
        return [ rs | rs <- res, rs /= Left "", rs /= Right ""]

    variable = do
        char '$'
        name <- takeTill (notInClass "a-zA-Z0-9_")
        return (Right (T.unpack name))

    loop xs = do
        text <- takeTill (== '$')
        var  <- choice [variable, fmap Left takeText]
        end  <- atEnd
        if end
            then return $ reverse (var : Left text : xs)
            else loop (var : Left text : xs)
````

From this, we can parse Text into [Chunk]:

````haskell
*Main> makeChunks "a b c foobar $doobar $goobar. $asdf"
[Left "a b c foobar ",Right "doobar",Left " ",Right "goobar",Left ". ",Right "asdf"]

````

But, what we obviously want is for all the *Left xs* to be turned into *Text*, and then all the *Right ys* to be turned into their respective definitions in the given source file, and then for the result to be folded together with *T.append* (although in actual code I tend to use *Data.Monoid*'s (<>)).

## Putting it all together

The above sounds like just the job for a QuasiQuoter! Lets give it a try. For convenience I will also define an instance for *Lift Text*, allowing us to lift *Text* values.

````haskell

instance Lift Text where
    lift t = litE (stringL (unpack t))

format :: QuasiQuoter
format = QuasiQuoter
    { quoteExp = \s ->
        let chunks       = makeChunks (T.pack s)
            liftedChunks = flip map chunks $ \c -> case c of
                Left  t -> [| t |]           -- lift raw text
                Right v -> global (mkName v) -- get a global Name from the name given

        -- and now to fold it all together ... 
        in foldr (\l r -> appE [| T.append |] l `appE` r) [| T.empty |] liftedChunks

        -- note that: appE :: Q Exp -> Q Exp -> Q Exp; it acts as function application for Q Exps
        --            [| T.append |] is the Q Exp form of T.append
    }

````

And now the moments of truth: does it work? Let us consult GHCi for the answer. I set *-ddump-splices* to show the intermediate splices that GHC generates. Also note that it needs *OverloadedStrings* to work for it to evaluate to *Text* at all.

````haskell
*QFormat> :l QFormat.hs
*QFormat> :set -XOverloadedStrings 
*QFormat> :set -XQuasiQuotes
*QFormat> :set -ddump-splices
*QFormat> let place = "World" in [format|Hello $place!|]
<interactive>:18:24-46: Splicing expression
    "Hello $place!"
  ======>
    T.append "Hello " (T.append place (T.append "!" T.empty))
"Hello World!"
*QFormat> -- excellent!
*QFormat> :{ 
*QFormat| let a = "yeah" 
*QFormat|     b = "maybe" 
*QFormat|     person = "jarjar" 
*QFormat|     desc   = "best character" 
*QFormat| in [format|$person was $desc, $a, $b, ok maybe not|]
*QFormat| :}
<interactive>:41:4-52: Splicing expression
    "$person was $desc, $a, $b, ok maybe not"
  ======>
    T.append
      person
      (T.append
         " was "
         (T.append
            desc
            (T.append
               ", "
               (T.append
                  a
                  (T.append
                     ", " (T.append b (T.append ", ok maybe not" T.empty)))))))
"jarjar was best character, yeah, maybe, ok maybe not"

````

So there we have it! I hope you found this little tutorial useful and/or enlightening. Making it certainly was a learning process in and of itself. [My actual library](http://hackage.haskell.org/package/QuasiText), which is frighteningly similar to the code we've hacked up here, although you can embed arbitrary Haskell expressions (leveraging the power of *haskell-src-meta*) into *QuasiText* interpolated strings.

## But wait!
Antoine Latter in the comments section pointed out: "You'll probably do less allocation at run-time if you use the 'Data.Text.concat' function." I concur with this statement.

So, lets refactor:

```haskell
format :: QuasiQuoter
format = QuasiQuoter
    { quoteExp = \s ->
        let chunks       = makeChunks (T.pack s)
            liftedChunks = flip map chunks $ \c -> case c of
                Left  t -> [| t |]           -- lift raw text
                Right v -> global (mkName v) -- get a global Name from the name given
        in appE [| T.concat |] (listE liftedChunks)
        -- note that listE :: [Q Exp] -> Q Exp
    }

```

This is decidedly simpler than the example above, but for the sake of illustrating how to use TemplateHaskell, I'll keep the former examples.
