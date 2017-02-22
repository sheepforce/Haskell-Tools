module Data.Chemistry.XYZ
( xyzParser
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
  -- coordinates <- many' xyzCoordLineParser <* (endOfLine <|> endOfInput)
  coordinates <- many' xyzCoordLineParser
  endOfInput
  return $ XYZ { nAtoms = nAtoms
               , comment = comment
               , xyzcontent = coordinates
               }
    where
      xyzCoordLineParser :: Parser (String,Double,Double,Double)
      xyzCoordLineParser = do
	many' $ char ' '
	element <- manyTill anyChar (char ' ')
	many' $ char ' '
	x <- double
	many' $ char ' '
	y <- double
	many' $ char ' '
	z <- double
	many' $ char ' '
	many' endOfLine
	return $ (element,x,y,z)