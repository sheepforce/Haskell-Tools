module Data.Chemistry.XYZ
( nAtoms
--, getCoordFromLine
--, getCoordFromAtom
--, trajAtomLines
--, trajAtomCoordList
--, getElement
--, getGeometriesFromTraj
--
--, printSepXYZ
--, printMDXYZ
) where
import System.IO
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

data XYZ = XYZ { nAtoms :: Int
               , comment :: String
               , xyzcontent :: [(String,Double,Double,Double)]
               } deriving Show

xyzParser :: Parser XYZ
xyzParser = do
  many' $ char ' '
  nAtoms <- decimal
  many' $ char ' '
  endOfLine
  comment <- manyTill anyChar endOfLine
  coordinates <- many' xyzCoordLineParser
  --many' endOfLine
  endOfInput
  return $ XYZ { nAtoms = nAtoms
               , comment = comment
               , xyzcontent = coordinates
               }
    where
      xyzCoordLineParser :: Parser (String,Double,Double,Double)
      xyzCoordLineParser = do
	many' $ char ' '
	element <- many1 anyChar
	x <- double
	many1 $ char ' '
	y <- double
	many1 $ char ' '
	z <- double
	many' $ char ' '
	many' endOfLine
	return $ (element,x,y,z)
{-
xyzCoordLineParser :: Parser (String,Double,Double,Double)
xyzCoordLineParser = do
	many' $ char ' '
	element <- manyTill anyChar (char ' ')
	many' $ char ' '
	x <- double
	many1 $ char ' '
	y <- double
	many1 $ char ' '
	z <- double
	many' $ char ' '
	endOfLine
	return $ (element,x,y,z)
-}