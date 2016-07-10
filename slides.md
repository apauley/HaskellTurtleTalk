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

# Built-in functions

Turtle supplies a whole lot of typed Haskell equivalents for the usual Unix utilities.

Examples include:

echo, env, cd, pwd, mv, cp, rm, date, touch, sleep, exit, ls, grep, sed, find etc.

Have a look at [Turtle-Prelude](https://hackage.haskell.org/package/turtle-1.2.8/docs/Turtle-Prelude.html) for more details.

# Turtle Types: Text

No `String`, just `Data.Text`

```haskell
echo :: MonadIO io => Text -> io ()
-- Print to stdout
```

Example:

```haskell
Prelude Turtle> echo "Hello, Turtle"
Hello, Turtle
```

# Turtle Types: FilePath

Turtle uses [system-filepath](https://hackage.haskell.org/package/system-filepath),
not the [filepath used by Prelude](https://hackage.haskell.org/package/filepath-1.4.1.0/docs/System-FilePath-Posix.html#t:FilePath)
which is essentially just a `String`.

Unfortunately it seems that the latest maintainer of `system-filepath` has
[deprecated](https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx) the package, although I didn't see any indication that Turtle will stop using it.

```haskell
cd :: MonadIO io => FilePath -> io ()
-- Change the current directory
```

```haskell
pwd :: MonadIO io => io FilePath
-- Get the current directory
```

```haskell
rm :: MonadIO io => FilePath -> io ()
-- Remove a file
```

Example:

```haskell
Prelude Turtle> cd "/tmp"
Prelude Turtle> pwd
FilePath "/tmp"
```

# Turtle Types: The Shell Monad

```haskell
newtype Shell a
-- A (Shell a) is a protected stream of a's with side effects
```

```haskell
ls :: FilePath -> Shell FilePath
-- Stream all immediate children of the given directory, excluding "." and ".."
```

```haskell
input :: FilePath -> Shell Text
-- Read lines of Text from a file
```

```haskell
output :: MonadIO io => FilePath -> Shell Text -> io ()
-- Stream lines of Text to a file
```

Example:

```haskell
Prelude Turtle> output "/tmp/turtle" ("Hello" <|> "Turtle")
Prelude Turtle> stdout $ input "/tmp/turtle"
Hello
Turtle
```

# Turtle Types: Pattern

```haskell
grep :: Pattern a -> Shell Text -> Shell Text
-- Keep all lines that match the given Pattern
```

```haskell
sed :: Pattern Text -> Shell Text -> Shell Text
-- Replace all occurrences of a Pattern with its Text result
```

```haskell
find :: Pattern a -> FilePath -> Shell FilePath
-- Search a directory recursively for all files matching the given Pattern
```

Example:

```haskell
currentBranch :: Shell Text
currentBranch = do
  sed ("* " *> return "") $ grep (prefix "*") (git "branch" ["--list"])
```

```haskell
*Main GitHellLib> stdout currentBranch
master
```

```bash
ghc-mod$ git branch --list
  ghc-8
* master
ghc-mod$ git branch 2>/dev/null | grep '\*' | cut -d'*' -f2-|sed -e 's/^\s*//g'
master
```

# Executing Arbitrary Commands

 * When built-in functions don't cut it anymore

There are two main categories of functions here:

## 1. Those with the word `shell`, e.g. `shellStrict`

```haskell
shellStrict
:: MonadIO io
=> Text	               -- Command line
-> Shell Text          -- Lines of standard input
-> io (ExitCode, Text) -- Exit code and stdout
```

## 2. Those with the word `proc`, e.g. `inprocWithErr`

```haskell
inprocWithErr
:: Text                     -- Command
-> [Text]                   -- Arguments
-> Shell Text               -- Lines of standard input
-> Shell (Either Text Text)	-- Lines of either standard output (Right) or standard error (Left)
```

The `proc`-style functions are safer, the `shell`-style functions are more powerful.

# shell and family

More powerful, but less safe.
You can include any shell command, with shell pipes etc:

```haskell
Prelude Turtle> shell "cat /etc/hosts|grep 127.0.0.1.*localhost" empty
127.0.0.1	localhost
ExitSuccess
Prelude Turtle> shell "cat /etc/hosts|grep NASA" empty
ExitFailure 1
```

```haskell
  let cmd = "cat dependency-check-report.xml|grep '<severity>.*</severity>'|cut -d'>' -f2|cut -d'<' -f1|sort|uniq"
  (exitCode, output) <- shellStrict cmd empty
```

# proc and family

Safer, but less powerful.
You supply arguments as a list of `Text`:

```haskell
exitCode <- proc "pandoc" ["-t", "slidy", "-s", "slides.md", "-o", "slides.html"] empty
```

```haskell
Prelude Turtle> let procEither = inprocWithErr "git" ["status", "--short"] empty
Prelude Turtle> stdout $ fmap (either (format ("turtle-err: "%s)) (format ("turtle-out: "%s))) shellEither
turtle-out:  M slides.html
turtle-out: MM slides.md
Prelude Turtle> cd "/tmp"
Prelude Turtle> stdout $ fmap (either (format ("turtle-err: "%s)) (format ("turtle-out: "%s))) shellEither
turtle-err: fatal: Not a git repository (or any of the parent directories): .git
```
