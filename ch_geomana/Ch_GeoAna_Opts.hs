{-# LANGUAGE DeriveDataTypeable #-}
module Ch_GeoAna_Opts where
import System.Console.CmdArgs

data Ch_GeoAna_Opts = Ch_GeoAna_Opts
  { input :: FilePath
  , output :: FilePath
  , atoms :: String
  , mdsteps :: Int
  , verbose :: Bool
  }
  deriving (Show,Data,Typeable)

ch_GeoAna_Opts = Ch_GeoAna_Opts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  , atoms = def &= help "list of atoms to monitor, space separated" &= typ "ATOMS"
  , mdsteps = 1 &= help "number of steps to be analysed" &= typ "STEPS"
  , verbose = False &= help "print additional output"
  }

mode = cmdArgsMode ch_GeoAna_Opts
