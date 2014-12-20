---
title: Greg
published: 2012-04-12
---

[GitHub project](https://github.com/mikeplus64/Greg)

### Greg, warrior IRC bot princess

A small, fast, extensible IRC bot written in Haskell.

### Features

* it works
* it doesn't crash (much)
* small, simple code base

### Installation / usage

~~~ {.sh}
git clone git://github.com/mikeplus64/Greg.git
cd Greg
cabal install
Greg # make sure ~/.cabal/bin is in $PATH
~~~

From there, you can input text to send to the channel (just by typing it in and pressing enter), or send IRC commands by prefixing your message with "/". Prefix your message with "//" to send a message starting with "/" to the channel the bot is in.

Note: if you try to join multiple channels with the one bot, weird things may happen.

