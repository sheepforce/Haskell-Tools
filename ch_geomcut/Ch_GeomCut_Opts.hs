{-# LANGUAGE DeriveDataTypeable #-}
module Ch_GeomCut_Opts where
import System.Console.CmdArgs
import Algebra

data Ch_GeomCut_Opts = Ch_GeomCut_Opts
  { input :: FilePath
  , output :: FilePath
  , shape :: String
  , geomparam :: Float
  , centre :: String
  , verbose :: Bool
  }
  deriving (Show,Data,Typeable)

ch_GeomCut_Opts = Ch_GeomCut_Opts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  , shape = "s" &= help "shape of remaining cut body"
  , geomparam = def &= help "radius or similiar parameter"
  , centre = def &= help "coordinates of the centre"
  , verbose = False &= help "print additional output"
  }

mode = cmdArgsMode ch_GeomCut_Opts
