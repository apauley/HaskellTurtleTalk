#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-# LANGUAGE OverloadedStrings #-}

import Turtle

main = do
  proc "pandoc" ["-t", "slidy", "-s", "slides.md", "-o", "slides.html"] empty
