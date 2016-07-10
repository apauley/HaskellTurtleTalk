% Haskell Shell Scripting with Turtle
% [Andreas Pauley](http://www.meetup.com/lambda-luminaries/events/231686085/)
% July 11, 2016

# What is Turtle?

> &ldquo;Turtle is a reimplementation of the Unix command line environment in Haskell so
> that you can use Haskell as a scripting language or a shell.  Think of `turtle`
> as `coreutils` embedded within the Haskell language.&rdquo;
>
> &mdash; [https://github.com/Gabriel439/Haskell-Turtle-Library](https://github.com/Gabriel439/Haskell-Turtle-Library)
 
 * Written by [Gabriel Gonzalez](http://haskellforall.com/)
 * Essentially a re-design of [Shelly](https://github.com/yesodweb/Shelly.hs)

# Features

* *Batteries included:* Command an extended suite of predefined utilities

* *Interoperability:* You can still run external shell commands

* *Portability:* Works on Windows, OS X, and Linux

* *Exception safety:* Safely acquire and release resources 

* *Streaming:* Transform or fold command output in constant space

* *Patterns:* Use typed regular expressions that can parse structured values

* *Formatting:* Type-safe `printf`-style text formatting

* *Modern:* Supports `text` and `system-filepath`

# Haskell for Shell Scripting? Why?

 * A high-level language that can compile to command-line executables.
 * Scripts can also be interpreted if you don't want to compile them.
 * Bringing the awesomeness of typed functional programming to your scripts

# Installing Turtle

From outside an existing Haskell project directory:

```bash
$ stack setup
...
$ stack install turtle
...
$ stack ghci turtle
...
Prelude> :set -XOverloadedStrings
Prelude> import Turtle
Prelude Turtle> echo "Hello, world!"
Hello, world!"
```

Making Turtle part of a Haskell project:

```bash
$ stack new my-command-line-executable
```

And add `turtle` as a dependency to cabal/stack.

# Hello, world!

Save this to `example.hs`:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}  --
                                    --
import Turtle                       --
                                    --
main = echo "Hello, world!"         -- echo Hello, world!
```

... then run the example script:

```bash
$ chmod u+x example.hs
$ time ./example.hs 
Hello, world!

real	0m1.355s
user	0m0.993s
sys	0m0.282s
```

# Or create a native binary

```bash
$ stack ghc -- -O2 example.hs
$ time ./example
Hello, world!

real	0m0.006s
user	0m0.002s
sys	0m0.003s
```
