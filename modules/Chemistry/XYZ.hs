module Chemistry.XYZ
( nAtoms
, getCoordFromLine
, getCoordFromAtom
, trajAtomLines
, trajAtomCoordList
, getElement
, printSepXYZ
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
trajAtomCoordList :: String -> Int -> Int -> [[Float]]
trajAtomCoordList content atom mdSteps = [getCoordFromLine content a | a <- b]
  where b = take mdSteps (trajAtomLines (nAtoms content) atom)

{- takes a xyz file and tells the element symbol of an atom -}
getElement :: String -> Int -> String
getElement content atom = head a
  where a = words b
        b = c!!(atom + 1)
        c = lines content


{- ############ -}
{- IO functions -}
{- ############ -}

{- takes a file handle, a list of element symbols and a list of coordinates and
prints them out to a xyz file -}
printSepXYZ :: Handle -> [String] -> [[Float]] -> IO()
printSepXYZ file [] [] = return ()
printSepXYZ file [e] [(x:y:z:_)] = hPutStrLn file (e ++ "     " ++ (show x) ++ "     " ++ (show y) ++ "     " ++ (show z))
printSepXYZ file (e:et) (c:ct) = do printSepXYZ file [e] [c]
				    printSepXYZ file et ct