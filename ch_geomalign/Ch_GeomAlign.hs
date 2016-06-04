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
      mdSteps = mdsteps arguments
  
  -- reading the xyz file
  geomHandle <- openFile inputFile ReadMode
  geomRawContents <- hGetContents geomHandle
  
  {-
  let -- get the elements from the original file and the number of atoms
      moleculeElements = [getElement geomRawContents a | a <- [1..(nAtoms geomRawContents)]]
      moleculeAtomNumber = nAtoms geomRawContents
      -- convert the atom coordinates to a list of vectors
      moleculeGeometryOrig = [list2vec (getCoordFromAtom geomRawContents a) | a <- [1..(nAtoms geomRawContents)]]
      -- recenter the geometry to a chosen origin, here a chose atom (the first one entered)
      moleculeGeometryRecenter = recenterGeom moleculeGeometryOrig (moleculeGeometryOrig!!((atomList!!0) - 1))
      -- rotate the geometry that second atom chosen lays in the ZY-plane
      -- therefore get the necessary rotation angle based on second chosen atom ...
      rotationAngle_2toZY = getP2ZYByZAngle (moleculeGeometryRecenter!!((atomList!!1) - 1))
      -- ... and apply the rotation around Z-Axis
      moleculeGeometry2inZY = [rotateAroundZ a rotationAngle_2toZY | a <- moleculeGeometryRecenter]
      -- second rotation for atom 2, so that it lies also in the XZ-plane and therefore on the z-Axis overall
      -- therefore get the necessary rotation angle based on second chosen atom ...
      rotationAngle_2toZX = getP2XZByXAngle (moleculeGeometry2inZY!!((atomList!!1) - 1))
      -- ... and apply the rotation around X-Axis
      moleculeGeometry2onZ = [rotateAroundX a rotationAngle_2toZX | a <- moleculeGeometry2inZY]
      -- rotate the geometry that third atom chosen lays in the ZY-plane
      -- therefore get the necessary rotation angle based on third chosen atom ...
      rotationAngle_3toZY = getP2ZYByZAngle (moleculeGeometry2onZ!!((atomList!!2) - 1))
      -- ... and apply the rotation around X-Axis
      moleculeGeometry2onZ3inZY = [rotateAroundZ a rotationAngle_3toZY | a <- moleculeGeometry2onZ]
      
      
      
  if (verbose arguments == True)
     then do
       putStrLn " This is the original geometry"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryOrig)
       
       putStrLn "\n This is the recentered geometry"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometryRecenter)
       
       putStrLn "\n This is the Vector of the chosen atom"
       print (moleculeGeometryOrig!!((atomList!!1) - 1))
       
       putStrLn "\n This is the rotation angle around Z-axis"
       print rotationAngle_2toZY
       
       putStrLn "\n This is the geometry with second atom in ZY-plane"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometry2inZY)
       
       putStrLn "\n This is the rotation angle around X-axis"
       print rotationAngle_2toZX
       
       putStrLn "\n This is the geometry with second atom on Z-axis"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometry2onZ)
       
       putStrLn "\n This is the rotation angle around Z-axis for third atom"
       print rotationAngle_3toZY
       
       putStrLn "\n This is the geometry with second atom on Z-Axis and third in YZ-plane"
       printSepXYZ stdout moleculeElements (map vec2list moleculeGeometry2onZ3inZY)
     else return ()
     -}
  
  let -- get the elements from the original file and the number of atoms
      moleculeElements = [getElement geomRawContents a | a <- [1..(nAtoms geomRawContents)]]
      moleculeAtomNumber = nAtoms geomRawContents
      -- read the coordinates for mdSteps steps in a trajectory
      moleculeGeometriesOrigList = getGeometriesFromTraj geomRawContents mdSteps
      -- convert this to a [[Vector]]
      moleculeGeometriesOrigVec = [map list2vec (moleculeGeometriesOrigList!!a) | a <- [0..(mdSteps - 1)]]
      -- recenter the geometry to a chosen origin, here a chose atom (the first one entered)
      moleculeGeometriesRecenter = [recenterGeom (moleculeGeometriesOrigVec!!a) (moleculeGeometriesOrigVec!!a!!((atomList!!0) - 1)) | a <- [0..(mdSteps - 1)]]
      -- rotate the geometry that second atom chosen lays in the ZY-plane
      -- therefore get the necessary rotation angle based on second chosen atom ...
      rotationAngles_2toZY = [getP2ZYByZAngle (moleculeGeometriesRecenter!!a!!((atomList!!1) - 1)) | a <- [0..(mdSteps - 1)]]
      -- ... and apply the rotation around Z-Axis
      moleculeGeometry2inZY = [[rotateAroundZ a (rotationAngles_2toZY!!b) | a <- (moleculeGeometriesRecenter!!b)] | b <- [0..(mdSteps - 1)]]
      -- second rotation for atom 2, so that it lies also in the XZ-plane and therefore on the z-Axis overall
      -- therefore get the necessary rotation angle based on second chosen atom ...
      rotationAngles_2toZX = [getP2XZByXAngle (moleculeGeometry2inZY!!a!!((atomList!!1) - 1)) | a <- [0..(mdSteps - 1)]]
      -- ... and apply the rotation around X-Axis
      moleculeGeometry2onZ = [[rotateAroundX a (rotationAngles_2toZX!!b) | a <- (moleculeGeometry2inZY!!b)] | b <- [0..(mdSteps - 1)]]
      -- rotate the geometry that third atom chosen lays in the ZY-plane
      -- therefore get the necessary rotation angle based on third chosen atom ...
      rotationAngles_3toZY = [getP2ZYByZAngle (moleculeGeometry2onZ!!a!!((atomList!!2) - 1)) | a <- [0..(mdSteps - 1)]]
      -- ... and apply the rotation around X-Axis
      moleculeGeometries2onZ3inZY = [[rotateAroundZ a (rotationAngles_3toZY!!b) | a <- (moleculeGeometry2onZ!!b)] | b <- [0..(mdSteps - 1)]]
  
  if (verbose arguments == True)
     then do
       putStrLn " These are the original geometries"
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometriesOrigVec!!a) | a <- [0..(mdSteps - 1)]])
       
       putStrLn "\n These are the recentered geometries"
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometriesRecenter!!a) | a <- [0..(mdSteps - 1)]])
       
       putStrLn "\n These are the geometries with second atom in ZY-plane"
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometry2inZY!!a) | a <- [0..(mdSteps - 1)]])
       
       putStrLn "\n These are the geometries with second atom on Z-axis"
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometry2onZ!!a) | a <- [0..(mdSteps - 1)]])
       
       putStrLn "\n These are the geometries with second atom on Z-Axis and third in YZ-plane"
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometries2onZ3inZY!!a) | a <- [0..(mdSteps - 1)]])
     else return ()

  if (outputFile == "stdout")
     then do
       printMDXYZ stdout moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometries2onZ3inZY!!a) | a <- [0..(mdSteps - 1)]])
     else do
       outputHandle <- openFile outputFile WriteMode
       printMDXYZ outputHandle moleculeAtomNumber (replicate mdSteps moleculeElements) ([map vec2list (moleculeGeometries2onZ3inZY!!a) | a <- [0..(mdSteps - 1)]])
       hClose outputHandle    


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- takes a string and converts it to a list of integers
intListFromInput :: String -> [Int]
intListFromInput a = map (read :: String -> Int) b
  where b = words a