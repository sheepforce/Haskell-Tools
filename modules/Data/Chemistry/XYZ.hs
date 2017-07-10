module Data.Chemistry.XYZ
( XYZ(..)
, xyzParser
, getNAtoms
, getElements
, getCoords
, printXYZ
) where
import System.IO
import Data.Attoparsec.Text.Lazy
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Text.Printf


{- ################# -}
{- define Data Types -}
{- ################# -}

data XYZ = XYZ { nAtoms :: Int
               , comment :: String
               , xyzcontent :: [(String,Double,Double,Double)]
               } deriving Show

{- ###### -}
{- Parser -}
{- ###### -}

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


{- ################################ -}
{- Functions to work with XYZ files -}
{- ################################ -}

getNAtoms :: XYZ -> Int
getNAtoms a = nAtoms a

getElement :: (String,Double,Double,Double) -> String
getElement (e,x,y,z) = e

getElements :: XYZ -> [String]
getElements a = map getElement $ xyzcontent a

getCoord :: (String,Double,Double,Double) -> (Double,Double,Double)
getCoord (e,x,y,z) = (x,y,z)

getCoords :: XYZ -> [(Double,Double,Double)]
getCoords a = map getCoord $ xyzcontent a

printXYZ :: Handle -> XYZ ->  IO ()
printXYZ xyzhandle xyz = do
    hPrint xyzhandle $ nAtoms xyz
    hPrint xyzhandle $ comment xyz
    mapM_ (\(e, x, y, z) -> do
        hPrintf xyzhandle "%-4s" e
        hPrintf xyzhandle "%+16.8f    " x
        hPrintf xyzhandle "%+16.8f    " y
        hPrintf xyzhandle "%+16.8f\n" z) (xyzcontent xyz)