module TestCases where

algorithmNrs :: [Int]
algorithmNrs = [1..3]

circlesNrs :: [Int]
circlesNrs = [ c*10^e | e <- [1..3], c<- [1..10] ]
