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
      -- recenter the geometry to a chosen origin, here a chose atom (the first one entered)
      moleculeGeometryRecenter = recenterGeom moleculeGeometryOrig (moleculeGeometryOrig!!((atomList!!0) - 1))
      -- convert the recentered geometry to spherical coordinates
      moleculeGeometryRecenter_spher = map (cart2spherical) moleculeGeometryRecenter
      -- get theta of the second atom entered
      rotTheta = (vec2list (moleculeGeometryRecenter_spher!!((atomList!!1) - 1)))!!1
      -- rotate that recentered geometry so that entered atom number 2 is on the z-Axis
      moleculeGeometryToAxis_spher = [subVec a (Vector 0 rotTheta 0) | a <- moleculeGeometryRecenter_spher]
      -- get phi of the third atom entered
      rotPhi = (vec2list (moleculeGeometryRecenter_spher!!((atomList!!2) - 1)))!!2
      -- rotate the axis-aligned geometry so that entered atom number 3 is on the xz-Plane
      moleculeGeometryToPlane_spher = [subVec a (Vector 0 0 rotPhi) | a <- moleculeGeometryToAxis_spher]
      -- reconvert the final structure to cartesian coordinates
      moleculeGeometryToPlane = map (spherical2cart) moleculeGeometryToPlane_spher
      -- convert final structure in cartesian to list type
      moleculeGeometryToPlane_list = map (vec2list) moleculeGeometryToPlane
      -- finally get the elements from the original file and the number of atoms
      moleculeElements = [getElement geomRawContents a | a <- [1..(nAtoms geomRawContents)]]
      moleculeAtomNumber = nAtoms geomRawContents
      
  if (verbose arguments == True)
     then do
       putStrLn " This is the original geometry"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryOrig)
       
       putStrLn "\n This is the recentered geometry"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryRecenter)
       print moleculeGeometryOrig
       
       putStrLn "\n This is the recentered geometry in spherical coordinates"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryRecenter_spher)
       
       putStrLn "\n This is the recentered geometry in spherical coordinates, BACKTRANSFORMED"
       printSepXYZ stdout moleculeElements (map vec2list (map spherical2cart moleculeGeometryRecenter_spher))
       
       putStrLn "\n This is the Theta of the second atom chosen"
       print rotTheta
       
       putStrLn "\n This is the geometry in spherical coordinates, rotated so that the second atom is aligned to z-Axis (theta=0)"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryToAxis_spher)
       
       putStrLn "\n This is the geometry in spherical coordinates, rotated so that the second atom is aligned to z-Axis (theta=0), BACKTRANSFORMED"
       printSepXYZ stdout moleculeElements (map vec2list (map spherical2cart moleculeGeometryToAxis_spher))
       
       putStrLn "\n This is the Phi of the third atom chosen"
       print rotPhi
       
       putStrLn "\n This is the final geometry in spherical coordinates"
       print moleculeGeometryToPlane_spher
       
       putStrLn "\n This is the final geometry in cartesian coordinates"
       print moleculeGeometryToPlane
       
       putStrLn "\n This is the final geometry in cartesian as list"
       print moleculeGeometryToPlane_list
       
       putStrLn "\n This is the list of elements"
       print moleculeElements
       
       putStrLn "\n Geometry has this amount of atoms"
       print moleculeAtomNumber
     else return ()
  
  if (outputFile == "stdout")
     then do
       hPutStrLn stdout ((show moleculeAtomNumber) ++ "\n")
       printSepXYZ stdout moleculeElements moleculeGeometryToPlane_list
     else do
       outputHandle <- openFile outputFile WriteMode
       hPutStrLn outputHandle ((show moleculeAtomNumber) ++ "\n")
       printSepXYZ outputHandle moleculeElements moleculeGeometryToPlane_list
       hClose outputHandle
    


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- takes a string and converts it to a list of integers
intListFromInput :: String -> [Int]
intListFromInput a = map (read :: String -> Int) b
  where b = words a

-- prints a gnuplot readable list
printXYZResult :: Handle -> [Float] -> IO()
printXYZResult file [] = return ()
printXYZResult file [a] = hPrint file a
printXYZResult file (a:b) = do hPrint file a
			       printXYZResult file b