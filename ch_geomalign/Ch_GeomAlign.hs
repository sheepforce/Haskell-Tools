import Data.Char
import Data.List
import System.IO
import System.Environment
import Algebra
import Chemistry.XYZ
import System.Console.CmdArgs
import Ch_GeomAlign_Opts

{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  -- get input file, output file and list of atoms from command line arguments
  arguments <- cmdArgs ch_GeomAlign_Opts
  
  -- bind input from command line to useful variables
  let inputFile = input arguments
      outputFile = output arguments
      atomList = intListFromInput (atoms arguments)
  
  -- reading the xyz file
  geomHandle <- openFile inputFile ReadMode
  geomRawContents <- hGetContents geomHandle
  
  -- convert the atom coordinates to a list of vectors
  let moleculeGeometryOrig = [list2vec (getCoordFromAtom geomRawContents a) | a <- [1..(nAtoms geomRawContents)]]
      moleculeGeometryRecenter = recenterGeom moleculeGeometryOrig (moleculeGeometryOrig!!(atomList!!0))
  
  putStrLn "This is the original geometry"
  print moleculeGeometryOrig
  
  putStrLn "\n This is the recentered"
  print moleculeGeometryRecenter
  
  
  


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- takes a string and converts it to a list of integers
intListFromInput :: String -> [Int]
intListFromInput a = map (read :: String -> Int) b
  where b = words a