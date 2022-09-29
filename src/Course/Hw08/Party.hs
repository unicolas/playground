{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Hw08.Party (glCons, maxFun, moreFun, nextLevel, treeFold, main) where

import Course.Hw08.Employee (Employee (Emp, empName), GuestList(..))
import Data.Tree (Tree (Node))
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ fun) (GL list fun') = GL (list ++ [emp]) (fun' + fun)

instance Semigroup GuestList where
  (<>) :: GuestList -> GuestList -> GuestList
  (<>) (GL l f) (GL l' f') = GL (l <> l') (f + f')

instance Monoid GuestList where
  mempty :: GuestList
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root subtree) = f root $ map (treeFold f) subtree

{-
>>> import Course.Hw08.Employee (testCompany)
>>> treeFold (\(Emp _ fun) lst -> fun + sum lst) testCompany
46
-}

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss best =
  ( foldr ((<>) . snd) (glCons boss mempty) best
  , foldr ((<>) . fst) mempty best
  )

{-
>>> import Course.Hw08.Employee (testCompany)
>>> nextLevel (Emp "Bob" 6) [(GL [Emp "Clair" 3] 3, mempty), (GL [Emp "Tom" 4] 4, mempty)]
(GL [Emp {empName = "Bob", empFun = 6}] 6,GL [Emp {empName = "Clair", empFun = 3},Emp {empName = "Tom", empFun = 4}] 7)
-}

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

{-
>>> import Course.Hw08.Employee (testCompany2)
>>> maxFun testCompany2
GL [Emp {empName = "John", empFun = 1},Emp {empName = "Sue", empFun = 5},Emp {empName = "Bob", empFun = 3},Emp {empName = "Sarah", empFun = 17}] 26
-}

glPrint :: GuestList -> String
glPrint (GL l fun) = "Total fun: " ++ show fun ++ "\n" ++ empListPrint l

empListPrint :: [Employee] -> String
empListPrint = unlines . sort . map empName

main :: IO ()
main = do
  contents <- readFile "src/Course/Hw08/company.txt"
  putStr $ glPrint $ maxFun $ read contents

