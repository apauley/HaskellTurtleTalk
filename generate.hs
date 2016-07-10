#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

-- Run as a script using `./genarate.hs`
-- or compile using:
-- stack ghc -- -O2 generate.hs
main = do
  selfContained <- options "Generate the slides as html" parser
  let baseArgs = ["-t", "slidy", "-s", "slides.md", "-o", "slides.html"]
  let args = if (selfContained) then "--self-contained":baseArgs else baseArgs
  proc "pandoc" args empty

parser :: Parser Bool
parser = switch "self-contained" 's' "Include JS/CSS within the HTML file"
