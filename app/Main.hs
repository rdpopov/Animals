module Main(
    Animal(Animal),
    attributes,
    hasAttr,
    name,
    attrLeft,
    helper,
    formatTO,
    has,
    main
    ) where

import System.IO
import Data.Typeable
import Data.List
import System.Environment
import System.Exit

data Animal = Animal String [String]
  deriving (Read, Show,Eq)
 
attributes (Animal _ attr) = attr
name (Animal name _) = name

hasAttr :: String -> Animal -> Bool
hasAttr attr anim = attr `elem` attributes anim

attrLeft :: [Animal] -> [String] ->[String]
attrLeft animals traits = do
  (nub( concatMap attributes animals)) \\ traits 

has :: [Animal] -> String -> [Animal] 
has animals trait = filter (hasAttr trait) animals


helper :: [String] -> Animal -> Bool
helper pool item = not $ (name item) `elem` pool

formatTO ani = show ani ++ "\n"

main = do
       ar <- getArgs
       let f = head ar
       hSetBuffering stdout NoBuffering
       content <- readFile f
       let zoo :: [Animal] = map read $ lines content
       let attr =  attrLeft zoo []
       akinator attr zoo []
       tmp <- readFile ".animals.bak"
       let new_entries :: [Animal] = map read $ lines tmp
       let newNames = map name new_entries
       let final_data = filter (helper newNames) zoo
       writeFile f $ concatMap formatTO (final_data ++ new_entries)

trans :: String->Bool
trans "y"   = True
trans "n"  = False
trans "yes"   = True
trans "no"  = False
trans _  = False

akinator :: [String]->[Animal]->[String]->IO()

akinator _ [] traits = newAnimal traits
akinator _ (x:[]) traits = answer traits x
akinator [] _ traits = newAnimal traits
akinator (t:ts) animals traits = do
  putStr ("Is your animal of " ++ t ++ " ? y/n ")
  ans <- getLine

  print traits
  print animals
  print $ animals `has` t
  print (animals \\ (animals `has` t))
  if trans ans
    then let traits_  =t:traits
             animals_ = animals `has` t
             que_     = attrLeft animals_ traits_
          in akinator que_ animals_ traits_
    else  
         let traits_  = traits
             animals_ = animals \\ (animals `has` t)
             que_     = attrLeft animals_ traits_
          in akinator que_ animals_ traits_

answer ::[String]-> Animal->IO()
answer traits animal  = do
  putStr ("Is this your animal " ++ name animal ++ "? y/n ")
  answer <- getLine
  if trans answer
    then return ()
    else almostAnimal traits animal

almostAnimal ::[String]-> Animal->IO()
almostAnimal traits animal = do
  putStrLn "Name of new animal ? "
  newName <- getLine
  putStrLn ("Animal has " ++ show traits )
  putStrLn ("What trait is different from " ++ name animal ++ "? ")
  newTrait <- getLine
  putStrLn ("Does " ++ name animal ++ " bear " ++ newTrait ++" trait? y/n ")
  traitFor <-getLine
  if trans traitFor
     then writeFile ".animals.bak" 
           (show (Animal newName traits) ++ "\n" ++  show (Animal (name animal) (newTrait:traits)) ++ "\n")
     else writeFile ".animals.bak" 
           (show (Animal newName (newTrait:traits)) ++ "\n" ++  show animal ++ "\n")


newAnimal ::[String]->IO()
newAnimal traits = do
  putStr "Name of new animal ? "
  newName <- getLine
  putStrLn ("Animal has " ++ show traits ++ ". Add a new trait?(leave empty for no) ")
  newTrait <- getLine
  if null newTrait
    then writeFile ".animals.bak" (show (Animal newName traits) ++ "\n")
    else writeFile ".animals.bak" (show (Animal newName (newTrait:traits)) ++ "\n")

