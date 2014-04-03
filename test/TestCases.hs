module TestCases where

algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [ c*10^e | e <- [0..1], c<- [1..9] ]
