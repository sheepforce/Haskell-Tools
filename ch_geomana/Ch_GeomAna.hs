import Data.Char
import Data.List
import System.IO
import System.Environment
import Algebra
import Chemistry.XYZ
import System.Console.CmdArgs
import Ch_GeomAna_Opts


{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  -- get input file, output file and list of atoms from command line arguments
  arguments <- cmdArgs ch_GeomAna_Opts
  
  -- bind input from command line to useful variables
  let inputFile = input arguments
      outputFile = output arguments
      mdSteps = mdsteps arguments
      atomList = intListFromInput (atoms arguments)
  
  -- tell the user what he did
  if (verbose arguments == True)
     then do
       putStr ("\n will monitor the trajectory in " ++ inputFile ++ " for ")
       case (length atomList) of 0 -> putStrLn "[ERROR]"
	                         1 -> putStrLn "[ERROR]"
	                         2 -> putStrLn "distance \n"
	                         3 -> putStrLn "angle \n"
	                         4 -> putStrLn "dihedral \n"
	                         x -> putStrLn "[WARN] dihedral \n"
     else return ()
  
  -- reading the trajectory file
  trajHandle <- openFile inputFile ReadMode
  trajRawContents <- hGetContents trajHandle
  
  -- creates a nested list (:t [[Vector]]) where outer layer is the atom to be monitored and the inner layer is the MD step
  let geometryMonitorList = [convertCoordList2CoordVecs (trajAtomCoordList trajRawContents a mdSteps) | a <- atomList]
  
  -- execute the real analysis
  let analysedList = analyseGeomList geometryMonitorList
  
  -- prints the monitored list
  if (outputFile == "stdout")
     then do
       printMonitorResult stdout analysedList
     else do
       outputHandle <- openFile outputFile WriteMode
       printMonitorResult outputHandle analysedList
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

-- distance monitoring
monitorDistance :: (Floating a) => [[Vector a]] -> [a]
monitorDistance geometryList = [distPoint (geometryList!!0!!a) (geometryList!!1!!a) | a <- [0..((length (geometryList!!0)) - 1)]]

-- angle monitoring
monitorAngle :: (Floating a) => [[Vector a]] -> [a]
monitorAngle geometryList = [anglePoint (geometryList!!0!!a) (geometryList!!1!!a) (geometryList!!2!!a) | a <- [0..((length (geometryList!!0)) - 1)]]

-- dihedral monitoring
monitorDihedral :: (Floating a) => [[Vector a]] -> [a]
monitorDihedral geometryList = [dihedralPoint (geometryList!!0!!a) (geometryList!!1!!a) (geometryList!!2!!a) (geometryList!!3!!a) | a <- [0..((length (geometryList!!0)) - 1)]]

-- prints a gnuplot readable list
printMonitorResult :: Handle -> [Float] -> IO()
printMonitorResult file [] = return ()
printMonitorResult file [a] = hPrint file a
printMonitorResult file (a:b) = do hPrint file a
			           printMonitorResult file b

analyseGeomList :: (Floating a) => [[Vector a]] -> [a]
analyseGeomList geomList
  | analysisCheck == 1 = [0.0,0.0,0.0]
  | analysisCheck == 2 = monitorDistance geomList
  | analysisCheck == 3 = map radian2deg (monitorAngle geomList)
  | analysisCheck == 4 = map radian2deg (monitorDihedral geomList)
  | otherwise          = map radian2deg (monitorDihedral geomList)
  where analysisCheck = length geomList