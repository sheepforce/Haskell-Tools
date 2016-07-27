import Data.Char
import Data.List
import System.IO
import System.Environment
import Algebra
import Chemistry.XYZ
import System.Console.CmdArgs
import Ch_GeomCut_Opts


{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  -- get input file, output file and list of atoms from command line arguments
  arguments <- cmdArgs ch_GeomCut_Opts

  -- bind input from command line to useful variables
  let inputFile = input arguments
      outputFile = output arguments
      form = shape arguments
      formsize = geomparam arguments
      rawMidpoint = centre arguments
  
  -- reading the trajectory file
  trajHandle <- openFile inputFile ReadMode
  trajRawContents <- hGetContents trajHandle

  let -- make midpoint a vector
      midpointWords = (words rawMidpoint)
      midpointFloats = map (read :: String -> Float) midpointWords
      midpoint = list2vec midpointFloats
      
      -- converting raw contents to handable format
      molecularRawGeometry = getGeometriesFromTraj trajRawContents 1
      molecularGeometry = convertCoordList2CoordVecs (molecularRawGeometry!!0)
      molecularRawElements = [getElement trajRawContents a | a <- [1..(nAtoms trajRawContents)]]
   
  let -- create a new list, where only atoms within a radius of formsize resist
      molecularGeometryCut = sphericalCut molecularGeometry midpoint formsize
      -- make a new list containing only the remaining element symbols
      persistenElementsIndex = sphericalElements molecularGeometry midpoint formsize molecularRawElements
      molecularElementsCut = [getElement trajRawContents a | a <- [1..(nAtoms trajRawContents)], elem a persistenElementsIndex]
  
  print (length molecularElementsCut)
  print (length molecularGeometryCut)
  -- prints the monitored list
  if (outputFile == "stdout")
     then do
       printSepXYZ stdout (length molecularGeometryCut) molecularElementsCut (map vec2list molecularGeometryCut)
     else do
       outputHandle <- openFile outputFile WriteMode
       printSepXYZ outputHandle (length molecularGeometryCut) molecularElementsCut (map vec2list molecularGeometryCut)
       hClose outputHandle


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- takes a string and converts it to a list of integers
intListFromInput :: String -> [Int]
intListFromInput a = map (read :: String -> Int) b
  where b = words a
  
-- prepends an integer to a list of integers
prependIntToList :: (Integral a) => a -> [a] -> [a]
prependIntToList a [] = [a]
prependIntToList a (b:c) = a : (b:c)

-- takes a list of coordinates (itself a list of floats) and converts it to a list of vectors
convertCoordList2CoordVecs :: (Num a) => [[a]] -> [Vector a]
convertCoordList2CoordVecs coordList = map (list2vec) coordList

{- sphericalCut takes a geometry, a point and a radius, and outputs a list, only containing coordinates less 
distant than radius from point -}
sphericalCut :: (Floating a, Ord a) => [Vector a] -> (Vector a) -> a -> [Vector a]
sphericalCut geometry midpoint radius = [a | a <- geometry, lengthVec (subVec a midpoint) < radius]

{- getSphericalEliminations takes the same arguments as sperhicalCut but returns a list of integers,
indexing which atoms are eliminated -}
sphericalElements :: (Floating a, Ord a) => [Vector a] -> (Vector a) -> a -> [String] -> [Int]
sphericalElements geometry midpoint radius elements = [a + 1 | a <- [0..(length geometry - 1)], lengthVec (subVec (geometry!!a) midpoint) < radius]