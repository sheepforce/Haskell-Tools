module Chemistry.XYZ
( nAtoms
, getCoordFromLine
, getCoordFromAtom
, trajAtomLines
, trajAtomCoordList
, getElement
, getGeometriesFromTraj
--
, printSepXYZ
, printMDXYZ
) where
import System.IO

{- taking a xyz file and reading the number of atoms -}
nAtoms :: String -> Int
nAtoms content = read a
  where a = b!!0
	b = lines content

{- taking a xyz file and reading coordinates from a given line -}
getCoordFromLine :: String -> Int -> [Float]
getCoordFromLine content line = take 3 (map (read :: String -> Float) a)
  where a = tail b
	b = words c
	c = d!!(line - 1)
	d = lines content

{- taking a xyz file and reading coordinates from a given atom -}
getCoordFromAtom :: String -> Int -> [Float]
getCoordFromAtom content atom = take 3 (map (read :: String -> Float) a)
  where a = tail b
	b = words c
	c = d!!(atom + 1)
	d = lines content

{- taking a trajectory in simple xyz format and getting the line numbers 
from the same atom in every block -}
trajAtomLines :: Int -> Int -> [Int]
trajAtomLines numberOfAtoms atom = [atom + 2 + a | a <- [0,(numberOfAtoms + 2)..]]


{- takes a trajectory, an atom to monitor and the number of steps in the trajectory
and returns a list of coordinates for this atom in each MD step -}
-- old version, SLOW
{-
trajAtomCoordList :: String -> Int -> Int -> [[Float]]
trajAtomCoordList content atom mdSteps = [getCoordFromLine content a | a <- b]
  where b = take mdSteps (trajAtomLines (nAtoms content) atom)
-}
trajAtomCoordList :: String -> Int -> Int -> [[Float]]
trajAtomCoordList content atom mdSteps = map (take 3) d
  where a = lines content
	b = map words [(a!!(x-1)) | x <- (take mdSteps (trajAtomLines (nAtoms content) atom))]
	c = map tail b
	d = map (map (read :: String -> Float)) c

{- takes a xyz file and tells the element symbol of an atom -}
getElement :: String -> Int -> String
getElement content atom = head a
  where a = words b
        b = c!!(atom + 1)
        c = lines content

{- getGeometriesFromTraj IS A COMPLICATED FUNCTION. It takes a simple XYZ trajectory content and a number
of mdSteps as input and returns a [[[Float]]]. The outer layer of this list is the mdStep, the middle layer
is indexing the atoms in this mdStep and the inner layer is the actual coordinate of this atom -}
-- old version, SLOW
{-
getGeometriesFromTraj :: String -> Int -> [[[Float]]]
getGeometriesFromTraj content mdSteps = a
  where a = [[getCoordFromLine content stepLine | stepLine <- (c!!b)] | b <- [0..(mdSteps - 1)]]
	c = take mdSteps [[(1 + 2 + a)..(2 + a + d)] | a <- [0,(d + 2)..]]
	d = nAtoms content
-}
getGeometriesFromTraj :: String -> Int -> [[[Float]]]
getGeometriesFromTraj content mdSteps = map (map (map (read :: String -> Float))) g
  where a = lines content
	b = map words a
	c = [[(b!!(d - 1)) | d <- (y!!e)] | e <- [0..(mdSteps - 1)]]
	f = map (map tail) c
	g = map (map (take 3)) f
	y = take mdSteps [[(1 + 2 + a)..(2 + a + z)] | a <- [0,(z + 2)..]]
	z = nAtoms content


	
{- ############ -}
{- IO functions -}
{- ############ -}

{- takes a file handle, a list of element symbols and a list of coordinates and
prints them out to a xyz file -}
printSepXYZ :: Handle -> Int -> [String] -> [[Float]] -> IO()
printSepXYZ file numberOfAtoms [] [] = return ()
printSepXYZ file numberOfAtoms (e:[]) ((x:y:z:_):[]) = hPutStrLn file (e ++ "     " ++ (show x) ++ "     " ++ (show y) ++ "     " ++ (show z))
printSepXYZ file numberOfAtoms (e:et) (c:ct) = if (length (et) == (numberOfAtoms - 1))
						  then do
						    hPutStrLn file (show numberOfAtoms ++ "\n")
						    printSepXYZ file numberOfAtoms [e] [c]
						    printSepXYZ file numberOfAtoms et ct
						  else do
						    printSepXYZ file numberOfAtoms [e] [c]
						    printSepXYZ file numberOfAtoms et ct

{- printMDXYZ takes a file handle, a list with elements where outer layer is the MDStep, and a list of geometries
an prints them to a XYZ-trajectory-file -}
printMDXYZ :: Handle -> Int -> [[String]] -> [[[Float]]] -> IO()
printMDXYZ file numberOfAtoms [] [] = return ()
printMDXYZ file numberOfAtoms (e:[]) (c:[]) = printSepXYZ file numberOfAtoms e c
printMDXYZ file numberOfAtoms (e:et) (c:ct) = do printSepXYZ file numberOfAtoms e c
						 printMDXYZ file numberOfAtoms et ct
