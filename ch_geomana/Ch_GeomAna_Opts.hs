{-# LANGUAGE DeriveDataTypeable #-}
module Ch_GeomAna_Opts where
import System.Console.CmdArgs

data Ch_GeomAna_Opts = Ch_GeomAna_Opts
  { input :: FilePath
  , output :: FilePath
  , atoms :: String
  , mdsteps :: Int
  , verbose :: Bool
  }
  deriving (Show,Data,Typeable)

ch_GeomAna_Opts = Ch_GeomAna_Opts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  , atoms = def &= help "list of atoms to monitor, space separated" &= typ "ATOMS"
  , mdsteps = 1 &= help "number of steps to be analysed" &= typ "STEPS"
  , verbose = False &= help "print additional output"
  }

mode = cmdArgsMode ch_GeomAna_Opts
