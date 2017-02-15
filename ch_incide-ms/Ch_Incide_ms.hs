import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import Data.Time
import System.IO
import System.Environment
import System.Console.CmdArgs
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Ch_Incide_ms_Opts


{- ################ -}
{- Type Definitions -}
{- ################ -}
-- date is defined by Data.Time

-- one point in the MS (single line)
data MsLine = 
  MsLine { measuringTime :: LocalTime
         , mass :: Double
         , intensity :: Double
         } deriving Show

-- type of measurement the ms did
data Measurement = MassSpectrum | Trend deriving (Show, Eq)

-- type for the whole file
data MS = 
  MS { masstype :: Measurement
     , massdata :: [MsLine]
     } deriving Show


{- ####### -}
{- Parsers -}
{- ####### -}
-- parse the time
timeParser :: Parser LocalTime
timeParser = do
    year <- count 4 digit
    char '/'
    month <- count 2 digit
    char '/'
    day <- count 2 digit
    char ' '
    hour <- count 2 digit
    char ':'
    minute <- count 2 digit
    char ':'
    second <- count 2 digit
    char '.'
    milisecond <- count 3 digit
    char ','
    return $ LocalTime { localDay = fromGregorian (read year) (read month) (read day)
                       , localTimeOfDay = TimeOfDay (read hour) (read minute) (read $ second ++ "." ++ milisecond) }

-- get the operation type from the xml-Header
xmlMeasurementParser :: Parser Measurement
xmlMeasurementParser = do
  manyTill anyChar (string $ B.pack "Mode=\"")
  ((string (B.pack "Trend\"") >> return Trend) <|> (string (B.pack "Mass sweep\"") >> return MassSpectrum))

-- parse a line of MS data
msLineParser :: Parser MsLine
msLineParser = do
  time <- timeParser
  many' (char ' ')
  lmass <- double
  char ','
  many' (char ' ')
  lintensity <- double
  char ','
  return $ MsLine { measuringTime = time
                  , mass = lmass
		  , intensity = lintensity
                  }

msParser :: Parser MS
msParser = do
  temp_type <- xmlMeasurementParser
  manyTill anyChar (string $ B.pack "</ConfigurationData>")
  endOfLine
  temp_data <- many $ msLineParser <* (endOfLine <|> endOfInput)
  return $ MS { masstype = temp_type
              , massdata = temp_data
              }
  

{- ############################################################## -}
{- "main" module, handling the IO and calling all other Functions -}
{- ############################################################## -}

main = do
  -- get the arguments from the command line
  arguments <- cmdArgs ch_Incide_ms_Opts
  
  -- bind input from command line to useful variables
  let inputfile = input arguments
      outputfile = output arguments
      completespec = complete arguments
  
  -- read the input file
  spectrumRaw <- B.readFile inputfile
  
  -- parse the ms data
  let msdata = eliminate $ parseOnly msParser spectrumRaw
  
  writeData msdata outputfile completespec

  
{- ############################## -}
{- Functions used in this program -}
{- ############################## -}

-- make data type the parser creates more useful
eliminate :: Either String MS -> MS
eliminate (Right a) = a

-- print gnuplot and spreadsheet compatible output
writeMs :: Handle -> [MsLine] -> IO()
writeMs file [] = return ()
writeMs file [a] = writeMsLine file a
writeMs file (a:b) = do writeMsLine file a
			writeMs file b

-- write out a single line of the ms
writeMsLine :: Handle -> MsLine -> IO()
writeMsLine file a = do
  hPrintf file "%10s , " (show $ localDay $ measuringTime a)
  hPrintf file "%12s , " (show $ localTimeOfDay $ measuringTime a)
  hPrintf file "%+8.3f , " (mass a)
  hPrintf file "%+10.4e\n" (intensity a)


-- write a single line of trend type ms
writeTrend :: Handle -> MS -> IO()
writeTrend file ms = writeTrend file (massdata ms) (getTrendMasses ms)
  where
    getNTrends :: MS -> Int
    getNTrends ms = length $ sort $ nub $ map mass $ massdata ms
    getTrendMasses :: MS -> [Double]
    getTrendMasses ms = sort $ nub $ map mass $ massdata ms
    writeTrend :: Handle -> [MsLine] -> [Double] ->IO()
    writeTrend file [] scannedMasses = return ()
    writeTrend file [a] scannedMasses = writeTrendLine file a scannedMasses
    writeTrend file (a:b) scannedMasses = do
      writeTrendLine file a scannedMasses
      writeTrend file b scannedMasses
    writeTrendLine :: Handle -> MsLine -> [Double] -> IO ()
    writeTrendLine file msline scannedMasses = do
      hPrintf file "%10s , " (show $ localDay $ measuringTime msline)
      hPrintf file "%12s , " (show $ localTimeOfDay $ measuringTime msline)
      listIntensityPrint file (createTrendIntensitiesForPrint msline scannedMasses)
      
      

 -- takes a ms line and creates a list that can be printed with listIntensityPrint for trend output
createTrendIntensitiesForPrint :: MsLine -> [Double] -> [Double]
createTrendIntensitiesForPrint msline allmasses = dummyValStart ++ ((intensity msline) : dummyValEnd)
  where linemass = mass msline
	indexOfMassinAllMasses = fromJust $ elemIndex linemass allmasses
	dummyValStart = replicate indexOfMassinAllMasses (-1.0e6 :: Double)
	dummyValEnd = replicate ((length allmasses) - indexOfMassinAllMasses -1) (-1.0e6 :: Double)

-- print a list Doubles so that i gives the Trend mass output
listIntensityPrint :: Handle -> [Double] -> IO ()
listIntensityPrint handle [] = return ()
listIntensityPrint handle [a] = 
  if (a == -1.0e6)
     then hPrintf handle "%+11s , \n" "NaN"
     else hPrintf handle "%+10.4e , \n" a
listIntensityPrint handle (a:b) = 
  if (a == -1.0e6)
     then do 
       hPrintf handle "%+11s , " "NaN"
       listIntensityPrint handle b
     else do
       hPrintf handle "%+10.4e , " a
       listIntensityPrint handle b

-- takes all masses in the spectrum and prints them
trendMassesPrint :: Handle -> MS -> IO()
trendMassesPrint file ms = do
  hPrintf file "%-24s  , " "#"
  printMasses file trendmasses
  where
    trendmasses = sort $ nub $ map mass $ massdata ms
    printMasses :: Handle -> [Double] -> IO()
    printMasses file [] = return ()
    printMasses file [a] = hPrintf file "%11.3f , \n" a
    printMasses file (a:b) = do 
      hPrintf file "%11.3f , " a
      printMasses file b
    
  

-- write out the data depending on type of measurement, completeness parameter and output
writeData :: MS -> FilePath -> Bool -> IO()
writeData ms path completeness
  | (masstype ms) == MassSpectrum && path == "stdout" && completeness == True = writeMs stdout (massdata ms)
  | (masstype ms) == MassSpectrum && path == "stdout" && completeness == False = writeMs stdout (massdata $ reduceMs ms)
  | (masstype ms) == MassSpectrum && path /= "stdout" && completeness == True = do
    outputHandle <- openFile path WriteMode
    writeMs outputHandle (massdata ms)
    hClose outputHandle
  | (masstype ms) == MassSpectrum && path /= "stdout" && completeness == False = do
    outputHandle <- openFile path WriteMode
    writeMs outputHandle (massdata $ reduceMs ms)
    hClose outputHandle
  | (masstype ms) == Trend && path == "stdout" = do    
    trendMassesPrint stdout ms
    writeTrend stdout ms
  | (masstype ms) == Trend && path /= "stdout" = do
    outputHandle <- openFile path WriteMode
    trendMassesPrint outputHandle ms
    writeTrend outputHandle ms
    hClose outputHandle
  | otherwise = print "Schafe"
  

-- reduceMs removes suceeding iterations and only prints the firts of the ms
reduceMs :: MS -> MS
reduceMs ms = c
  where a = elemIndices (mass $ (massdata ms)!!0) (map mass $ massdata ms)
	b = [((massdata ms)!!x) | x <- [0..((a!!1) - 1)]]
	c = MS { masstype = (masstype ms)
	       , massdata = b
	       }

-- replace element in List by Index
replaceByIndex :: [a] -> a -> Int -> [a]
replaceByIndex list val2ins ind = lhead ++ [val2ins] ++ ltail
  where lhead = Data.List.take (ind - 1) list
	ltail = [ list!!x | x <- [ind..(length list - 1)] ]
	
