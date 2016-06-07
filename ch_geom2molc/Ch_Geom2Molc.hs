import Data.Char
import Data.List
import System.IO
import System.Environment
import Algebra
import Chemistry.XYZ
import System.Console.CmdArgs
import Ch_Geom2Molc_Opts

{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  -- get input file, output file and list of atoms from command line arguments
  arguments <- cmdArgs ch_Geom2Molc_Opts
  
  -- bind input from command line to useful variables
  let inputFile = input arguments
      outputFile = output arguments
  
  -- reading the xyz file
  geomHandle <- openFile inputFile ReadMode
  geomRawContents <- hGetContents geomHandle
  
  let -- get the elements from the original file and the number of atoms
      moleculeElements = [getElement geomRawContents a | a <- [1..(nAtoms geomRawContents)]]
      moleculeAtomNumber = nAtoms geomRawContents
      -- convert the atom coordinates to a list of float lists
      moleculeGeometryOrig = [getCoordFromAtom geomRawContents a | a <- [1..(nAtoms geomRawContents)]]
      -- zip the list and sort it elementwise
      moleculeZippedGeometrySorted = sort (zip moleculeElements moleculeGeometryOrig)
      -- now unzip again and manipulate the elemts to get a number
      moleculeSortedElements = fst (unzip moleculeZippedGeometrySorted)
      moleculeSortedCoordinates = snd (unzip moleculeZippedGeometrySorted)
      -- intermediately make sublists where same elements are grouped
      groupedSortedElements = group moleculeSortedElements
      -- count the length of each sublist
      numberSameElementAppears = [length (groupedSortedElements!!a) | a <- [0..(length groupedSortedElements - 1)]]
      -- use the number of each element to create a list that will be zipped with the elements
      numberingSameElementsList = [[1..(numberSameElementAppears!!a)] | a <- [0..(length groupedSortedElements - 1)]]
      -- zip together the grouped elements and their numbers in a few steps
      numberedElements_1 = zip groupedSortedElements [map show (numberingSameElementsList!!a) | a <- [0..(length groupedSortedElements -1)]]
      numberedElements_2 = [zip (fst (numberedElements_1!!a)) (snd (numberedElements_1!!a)) | a <- [0..(length numberedElements_1 - 1)]]
      numberedElements_3 = concat numberedElements_2
      -- zip numbered elements together with coordinates
      moleculeSortedGeometry = zip numberedElements_3 moleculeSortedCoordinates
      
  if (outputFile == "stdout")
     then do
       printOrderedXYZs stdout moleculeSortedGeometry
     else do
       outputHandle <- openFile outputFile WriteMode
       printOrderedXYZs outputHandle moleculeSortedGeometry
       hClose outputHandle


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

printOrderedXYZs :: Handle -> [((String, String), [Float])] -> IO()
printOrderedXYZs file [] = return ()
printOrderedXYZs file (((a,b),(x:y:z:_)):[]) = hPutStrLn file (a ++ b ++ "    " ++ show x ++ "    " ++ show y ++ "    " ++ show z ++ "    angstrom")
printOrderedXYZs file (a:at) = if ( (fst (fst a)) == (fst (fst (at!!0))))
				  then do printOrderedXYZs file [a]
					  printOrderedXYZs file at
				  else do printOrderedXYZs file [a]
					  hPutStr file "\n"
					  printOrderedXYZs file at