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
import Ch_HessConv_Opts
import Data.Chemistry.XYZ

{- ################ -}
{- Type Definitions -}
{- ################ -}

-- Fortran Double Notation, spearate base from exponent
data FortranDouble = 
  FortranDouble { fortranDoubleBase :: Double
                , fortranDoubleExponentSign :: Char
                , fortranDoubleExponent :: Int
                } deriving (Show,Ord,Eq)

data DaltonHess = 
  DaltonHess { daltonHessDimension :: Int
             , daltonHessValues :: [Double]
             , daltonHessGeom :: [Double]
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

{- parse the output from Dalton DALTON.HES 
Dalton writes the whole hessian in blocks of the same
column -}
daltonHessParser :: Parser DaltonHess
daltonHessParser = do
  many' $ char ' ' -- whitespace before the dimension
  dalHessDim <- decimal -- this is the dimension of the square hessian as well as the number of blocks
  many' $ char ' '
  many' endOfLine
  dalHess <- count (dalHessDim^2) fortranFloatParser -- the blocks with the hessian
  dalGeom <- count dalHessDim fortranFloatParser -- the block with the geometry
  endOfInput
  return $ DaltonHess { daltonHessDimension = dalHessDim
                      , daltonHessValues = dalHess
                      , daltonHessGeom = dalGeom
                      }

{- parse Fortran-Double notation with D as exponent-sign
read base, skip D and then read exponent -}
fortranDoubleParser :: Parser FortranDouble
fortranDoubleParser = do
      many' $ char ' '
      base <- double
      (char 'D') <|> (char 'E') <|> (char 'e')
      sign <- ((char '+') <|> (char '-'))
      expo <- decimal
      many' $ char ' '
      many' endOfLine
      return $ FortranDouble { fortranDoubleBase = base
                             , fortranDoubleExponentSign = sign
                             , fortranDoubleExponent = expo
                             }

-- parser for Fortran floats with E notation                             
fortranFloatParser :: Parser Double
fortranFloatParser = do
  many' $ char ' '
  number <- double
  many' $ char ' '
  many' endOfLine
  return $ number




{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  
  -- get the arguments from the command line
  arguments <- cmdArgs ch_hessConv_Opts
  
  -- bind input from command line to useful variables
  let inputfile = input arguments
      outputfile = output arguments
      addinputfile = addInput arguments
      inFormat = inputFormat arguments
      outFormat = outputFormat arguments
  
  -- give a fortran notation zero for creation of dummy matrices
  let fortranZero = FortranDouble { fortranDoubleBase = 0.0
                                  , fortranDoubleExponentSign = '+'
                                  , fortranDoubleExponent = 0
                                  }
  
  -- read the additional input
  addInput <- readFile "nw.xyz"
  -- decide what to do with the additional file based on input and outFormat
  
  -- read coordinates from additional Input
  let nwcoordinates = getGeometriesFromTraj addInput 1

  print nwcoordinates
  
  -- if Input is NWChem, do the following
  -- read the hessian from the file
  nwHessFromFile <- B.readFile "nw.hess"
  let -- parse the file and store it in nwHessLowerOnly
      -- this contains just the lower left triangle of the full Hessian
      nwHessLowerOnly = eliminate $ parseOnly nwchemHessParser nwHessFromFile
      -- dimensions the square hessian matrix will have (n * n) from the number of elements in the lower triangle
      nwHessDim = triangularMatrixSize (length nwHessLowerOnly)
      -- create a zero-filled dummy matrix
      nwInitMat = replicate nwHessDim $ replicate nwHessDim fortranZero
      -- make the list of indices of the lower triangle in NWChem input/output order
      nwLowerInd = triangularIndexMaker nwHessDim
      -- the full hessian in internal format for printing
      fullNWHessianFortran = expandLowerTriangularToSquare nwHessLowerOnly nwInitMat
      fullNWHessianHaskell = map (map fD2hD) fullNWHessianFortran
  
  
  dalHessFromFile <- B.readFile "dal.hes"
  let -- parse the file and store it in dalHessian
      -- this contains the full Hessian
      dalHessian = eliminate $ parseOnly daltonHessParser dalHessFromFile
      -- dimensions the square hessian matrix will have (n * n) from the number of elements in the lower triangle
      dalHessDim = daltonHessDimension dalHessian
      -- create a zero-filled dummy matrix
      dalInitMat = replicate dalHessDim $ replicate dalHessDim 0.0 :: [[Double]]
      -- make the list of indices of the lower triangle in NWChem input/output order
      dalInd = squareIndexMaker dalHessDim
      -- the full hessian in internal format for printing
      fullDalHessianHaskell = head $ fillMatrix dalInd (daltonHessValues dalHessian) [dalInitMat]
      fullDalHessianFortran = map (map hD2fD) fullDalHessianHaskell
  
  
  
  print "NWChem Hessian in NWChem Format"
  writeNWHessian stdout nwLowerInd fullNWHessianFortran
  print "NWChem Hessian in Dalton Format"
  writeDaltonHessian stdout dalInd fullNWHessianFortran dalHessian

  print "Dalton Hessian in NWChem Format"
  writeNWHessian stdout nwLowerInd fullDalHessianFortran
  print "Dalton Hessian in Dalton Format"
  writeDaltonHessian stdout dalInd fullDalHessianFortran dalHessian
  

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

{- takes a file handle, a list of indices  of the lower triangle and the full
hessian in the internal format and writes it out in the NWChem format -}
writeNWHessian :: Handle -> [(Int,Int)] -> [[FortranDouble]] -> IO()
writeNWHessian handle [] hessian = return ()
writeNWHessian handle [a] hessian = printFortranDouble2NWNumber handle hessVal
  where
    hessVal = getValueFromMatrix a hessian
writeNWHessian handle (a:b) hessian = do
  printFortranDouble2NWNumber handle hessVal
  writeNWHessian handle b hessian
    where
      hessVal = getValueFromMatrix a hessian

-- write FortranDouble to Dalton Double format in the hessian
printHaskellDouble2DaltonNumber :: Handle -> FortranDouble -> IO()
printHaskellDouble2DaltonNumber handle number = do
  hPrintf handle "  %19.16f" (fortranDoubleBase number)
  hPrintf handle "%c%c" 'E' (fortranDoubleExponentSign number)
  hPrintf handle "%03d\n" (fortranDoubleExponent number)

{- takes a file handle. a list of indices for the whole matrix, the full hessian in internal
format and a hessian in internal dalton format (with dimension and geometry). Writes the hessian
to DALTON.HES format -}
writeDaltonHessian :: Handle -> [(Int,Int)] -> [[FortranDouble]] -> DaltonHess -> IO()
writeDaltonHessian handle [] dalHess linDalHess = return ()
writeDaltonHessian handle [a] dalHess linDalHess = do
  printHaskellDouble2DaltonNumber handle hessVal
  hPrintf handle "\n"
  sequence_ [hPrintf handle "%20s\n" "cart geom be here (au)!" | x <- [1 .. (daltonHessDimension linDalHess)]]
    where
      hessVal = getValueFromMatrix a dalHess
writeDaltonHessian handle (a:b) dalHess linDalHess
  -- is this the first value to be written? then write the dimension to the header
  | ((length dalHess)^2) == (length (a:b)) = do
    hPrintf handle "%12d\n\n" (daltonHessDimension linDalHess)
    -- is this the last value in one column? If yes insert newline
    if (fst a == ((daltonHessDimension linDalHess) - 1))
       then do
	 printHaskellDouble2DaltonNumber handle hessVal 
	 hPrintf handle "\n"
	 writeDaltonHessian handle b dalHess linDalHess
       else do
	 printHaskellDouble2DaltonNumber handle hessVal
	 writeDaltonHessian handle b dalHess linDalHess
  | otherwise= do
    if (fst a == ((daltonHessDimension linDalHess) - 1))
       then do
	 printHaskellDouble2DaltonNumber handle hessVal 
	 hPrintf handle "\n"
	 writeDaltonHessian handle b dalHess linDalHess
       else do
	 printHaskellDouble2DaltonNumber handle hessVal
	 writeDaltonHessian handle b dalHess linDalHess
  where
    hessVal = getValueFromMatrix a dalHess

-- make data type the parser creates more useful
eliminate :: Either String a -> a
eliminate (Right a) = a

-- eliminate Justs
eliminateJust :: Maybe Int -> Int
eliminateJust (Just a) = a

{- get the size n*n of the square-matrix if you give in the number of elements
in a single triangle including the diagonal elements obviously -}
triangularMatrixSize :: Int -> Int
triangularMatrixSize nElemSingleTriangle = 1 + (eliminateJust $ elemIndex nElemSingleTriangle [round (((1 + x) * x) / 2) | x <- [1 .. ]])


-- gives a tuple which are the indizes of the values in a lower triangular square matrix
triangularIndexMaker :: Int -> [(Int,Int)]
triangularIndexMaker dim = [(a,b) | a <- [0..(dim-1)], b <- [0..(dim-1)], b <= a]

-- gives a tuple which are the indices in a column-first matrix
squareIndexMaker :: Int -> [(Int,Int)]
squareIndexMaker dim = [(a,b) | b <- [0..(dim-1)], a <- [0..(dim-1)]]

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

-- give the function an index and a matrix and you get the corresponding value from the matrix
getValueFromMatrix :: (Int,Int) -> [[a]] -> a
getValueFromMatrix (i,j) matrix = matrix!!i!!j

{- give in initial Values of the lower Triangle of a triangular Matrix and a intialMatrix of the expected size.
It will return the fully expanded hessian matrix -}
expandLowerTriangularToSquare :: [a] -> [[a]] -> [[a]]
expandLowerTriangularToSquare lowerValues initMat = head $ fillMatrix (map invertIndices initInd) lowerValues $ fillMatrix initInd lowerValues [initMat]
  where
    initInd = triangularIndexMaker dim
    dim = triangularMatrixSize (length lowerValues)


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

fD2hD :: FortranDouble -> Double
fD2hD a
  | (fortranDoubleExponentSign a) == '+' = (fortranDoubleBase a) * 10 ** (fromIntegral (fortranDoubleExponent a))
  | (fortranDoubleExponentSign a) == '-' = (fortranDoubleBase a) * 10 ** (- fromIntegral (fortranDoubleExponent a))

hD2fD :: Double -> FortranDouble
hD2fD a  
  | a == 0.0       = FortranDouble { fortranDoubleBase = 0.0
                                   , fortranDoubleExponentSign = '+'
                                   , fortranDoubleExponent = 0
                                   }
  | (abs a) < 1.0 = FortranDouble { fortranDoubleBase = a / (10.0 ** (fromIntegral $ floor $ logBase 10.0 (abs a)))
                                   , fortranDoubleExponentSign = '-'
                                   , fortranDoubleExponent = abs $ floor $ logBase 10.0 (abs a)
                                   }
  | (abs a) >= 1.0  = FortranDouble { fortranDoubleBase = a / (10.0 ** (fromIntegral $ floor $ logBase 10.0 (abs a)))
                                   , fortranDoubleExponentSign = '+'
                                   , fortranDoubleExponent = abs $ floor $ logBase 10.0 (abs a)
                                   }
