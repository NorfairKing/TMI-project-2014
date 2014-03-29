module Main where

import CrazyParser

naive circles =
  return "Dit algoritme is niet geïmplementeerd."
scanline_quadratic circles =
  return "Dit algoritme is niet geïmplementeerd."
scanline_linearithmic circles =
  return "Dit algoritme is niet geïmplementeerd."

main = do
  algorithm <- parseLine
  nCircles <- parseLine
  circles <- replicateM nCircles parseLine
  
