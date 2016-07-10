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

Save this to *`example.hs`*

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

# Create a native binary

```bash
$ stack ghc -- -O2 example.hs
$ time ./example
Hello, world!

real	0m0.006s
user	0m0.002s
sys	0m0.003s
```

# Command-line Arguments

## A Boolean switch

```haskell
main = do
  selfContained <- options "Generate the slides as html" parser
  let baseArgs = ["-t", "slidy", "-s", "slides.md", "-o", "slides.html"]
  let args = if (selfContained) then "--self-contained":baseArgs else baseArgs
  proc "pandoc" args empty

parser :: Parser Bool
parser = switch "self-contained" 's' "Include JS/CSS within the HTML file"
```

```bash
$ ./generate -h
Generate the slides as html

Usage: generate [-s|--self-contained]

Available options:
  -h,--help                Show this help text
  -s,--self-contained      Include JS/CSS within the HTML file
```

# Combining Arguments

```haskell
main = do
  (repoPath, maybeSecs) <- options "Runs a `git fetch` continuously for a given repository" parser
  let secs = fromMaybe defaultSleepSeconds $ fmap realToFrac maybeSecs
  fetch repoPath secs

parser :: Parser (FilePath, Maybe Int)
parser = (,) <$> argPath "repo"  "The path to a git repository"
             <*> optional (argInt "sleepSeconds" "The number of seconds to sleep between fetches.")
```

```bash
$ git-fetch-daemon -h
Runs a `git fetch` continuously for a given repository

Usage: git-fetch-daemon REPO [SLEEPSECONDS]

Available options:
  -h,--help                Show this help text
  REPO                     The path to a git repository
  SLEEPSECONDS             The number of seconds to sleep between fetches.
```

# Subcommands

```haskell
main = do
  x <- options "Haskell Shell Helpers" parser
  case x of
    PSGrep g                                -> stdout $ psg g
    OwaspDependencyCheck (project, scandir) -> owaspCheck project scandir

data Command = PSGrep Text | OwaspDependencyCheck (Text, FilePath) deriving (Show)

parser :: Parser Command
parser = fmap PSGrep (subcommand "psg" "Grep for text from a process listing."
                      (argText "text" "Some text to grep for"))
     <|> fmap OwaspDependencyCheck (subcommand "owasp-dependency-check"
                                    "Run the OWASP dependency checker pointing at a directory"
                      ((,) <$> (argText "project" "The project name to be displayed in the report")
                           <*> (argPath "dir" "A directory with files to check")))
```

# Subcommand Help Output

```bash
$ hsh -h
Haskell Shell Helpers

Usage: hsh (psg | owasp-dependency-check)

Available options:
  -h,--help                Show this help text

Available commands:
  psg
  owasp-dependency-check
```

```bash
$ hsh owasp-dependency-check -h
Run the OWASP dependency checker pointing at a directory

Usage: hsh owasp-dependency-check PROJECT DIR

Available options:
  -h,--help                Show this help text
  PROJECT                  The project name to be displayed in the report
  DIR                      A directory with files to check
```
