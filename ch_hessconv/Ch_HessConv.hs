import Data.Char
import Data.List
import Text.Printf
import System.IO
import System.Environment
import System.Console.CmdArgs
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Numeric.LinearAlgebra as BLAS
import qualified Numeric.LinearAlgebra.Data as BLAS
--import Ch_Incide_ms_Opts


{- ################ -}
{- Type Definitions -}
{- ################ -}

-- Fortran Double Notation, spearate base from exponent
data FortranDouble = 
  FortranDouble { fortranDoubleBase :: Double
                , fortranDoubleExponentSign :: Char
                , fortranDoubleExponent :: Int
                } deriving (Show,Ord,Eq)


{- ########################################### -}
{- Parsers for Output of different QC-software -}
{- ########################################### -}

{- parse the output from NWChem $PREFIX.hess
NWChem writes the lower triangle of the matrix.
output is row wise -}
nwchemHessParser :: Parser [FortranDouble]
nwchemHessParser = do
  nwHessEntry <- many' fortranDoubleParser
  endOfInput
  return $ nwHessEntry

{- parse Fortran-Double notation with D as exponent-sign
read base, skip D and then read exponent -}
fortranDoubleParser :: Parser FortranDouble
fortranDoubleParser = do
      many' $ char ' '
      base <- double
      char 'D'
      sign <- ((char '+' >> return ('+')) <|> (char '-' >> return ('-')))
      expo <- decimal
      many' endOfLine
      return $ FortranDouble { fortranDoubleBase = base
                             , fortranDoubleExponentSign = sign
                             , fortranDoubleExponent = expo
                             }


{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  nwHessFromFile <- B.readFile "nw.hess"
  
  let nwHessLowerOnly = parseOnly nwchemHessParser nwHessFromFile
  
  let initInd = triangularIndexMaker 2
      initVal = [1.0, 2.0 .. 3.0] :: [Double]
      initMat = replicate 2 $ replicate 2 0.0 :: [[Double]]
  
  print $ fillMatrix initInd initVal [initMat]
  
  
  --print nwHessLowerOnly


{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- write FortranDouble to NWChem Double format in the hessian
printFortranDouble2NWNumber :: Handle -> FortranDouble -> IO()
printFortranDouble2NWNumber handle number = do
  hPrintf handle "    %13.10f" (fortranDoubleBase number)
  hPrintf handle "%c%c" 'D' (fortranDoubleExponentSign number)
  hPrintf handle "%02d\n" (fortranDoubleExponent number)
  return ()

-- make data type the parser creates more useful
eliminate :: Either String a -> a
eliminate (Right a) = a

{- get the size n*n of the square-matrix if you give in the number of elements
in a single triangle including the diagonal elements obviously -}
triangularMatrixSize :: Int -> Int
triangularMatrixSize nSingleTriangle
  | solution1 >= 0 = round solution1
  | solution2 >= 0 = round solution2
  | otherwise = 0
  where
    solution1 = -(sqrt (8 * (fromIntegral nSingleTriangle) + 1) + 1) / 2
    solution2 = (sqrt (8 * (fromIntegral nSingleTriangle) + 1) - 1) / 2

-- gives a tuple which are the indizes of the values in a lower triangular square matrix
triangularIndexMaker :: Int -> [(Int,Int)]
triangularIndexMaker dim = [(a,b) | a <- [0..(dim-1)], b <- [0..(dim-1)], b <= a]

-- give the index of the matrix (i rows, j columns), the valuen to insert and the matrix
setValueInMatrix :: (Int,Int) -> a -> [[a]] -> [[a]]
setValueInMatrix (i,j) value matrix = rowsHead ++ [rowSubs] ++ rowsTail
  where 
    rowsHead = Data.List.take i matrix
    rowsTail = drop (i + 1) matrix
    rowToSubs = matrix!!i
    colHead = Data.List.take j rowToSubs
    colTail = drop (j + 1) rowToSubs
    rowSubs = colHead ++ [value] ++ colTail

{-
expandLowerTriangularToSquare :: [Double] -> [[Double]]
expandLowerTriangularToSquare lowerValues = fillMatrix inputIndizes lowerValues initialMatrix
-}

{- THIS IS A STRANGE FUNCTION WHICH I DO NOT UNDERSTAND FULLY BUT IT WORKS!
give a list of indices, the corresponding list of values (has obviously to be
of the same length) and a initialMatrix (which has to be put in a containing list. 
It will fill the values in the matrix at the positions given by the indices. 
It will return the the completed matrix and original matrix -}
fillMatrix :: [(Int,Int)] -> [a] -> [[[a]]] -> [[[a]]]
fillMatrix [ind] [val] mat = (setValueInMatrix ind val (head mat)) : mat
fillMatrix (indF:indR) (valF:valR) [mat] = (head nextItMats) : [mat]
  where 
    thisItMat = setValueInMatrix indF valF mat
    nextItMats = fillMatrix indR valR [thisItMat]
fillMatrix (indF:indR) (valF:valR) mat = (head nextItMats) : thisItMat : mat
  where 
    thisItMat = setValueInMatrix indF valF (head mat)
    nextItMats = fillMatrix indR valR [thisItMat]

invertIndices :: (Int,Int) -> (Int,Int)
invertIndices (a,b) = (b,a)

{-
-- take the lower triangular matrix and expand to complete matrix
expandLowerTriangularToSquare :: [FortranDouble] -> Int -> [[FortranDouble]]
expandLowerTriangularToSquare =

{- take a list of lowerValues and a dimension of the matrix to be created and it will 
output a single row of the new matrix with upper right filled with zeros -}
createZeroFilledTriangularMatrixLine :: [Double] -> Int -> [Double]
createZeroFilledTriangularMatrixLine [a] dim = a : (replicate (dim - 1) 0.0)
createZeroFilledTriangularMatrixLine lowerValues dim = (drop ((length lowerValues) - consumedValuesinThisLine) lowerValues) ++ (replicate (dim - consumedValuesinThisLine) 0.0)
  where
    consumedValuesinThisLine = triangularMatrixSize (length lowerValues)


createZeroFilledTriangularMatrix :: [Double] -> Int -> [[Double]] -> [[Double]]
createZeroFilledTriangularMatrix [a] dim outputMatrix = (createZeroFilledTriangularMatrixLine [a] dim) : outputMatrix
createZeroFilledTriangularMatrix lowerValues dim outputMatrix = b : a : outputMatrix
  where
    a = createZeroFilledTriangularMatrixLine lowerValues dim
    b = createZeroFilledTriangularMatrix (Data.List.take ((length lowerValues) - consumedValuesinThisIteration) lowerValues) dim outputMatrix
    consumedValuesinThisIteration = triangularMatrixSize (length lowerValues)
-}