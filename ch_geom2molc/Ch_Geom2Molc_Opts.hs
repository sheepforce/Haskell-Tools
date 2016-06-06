{-# LANGUAGE DeriveDataTypeable #-}
module Ch_Geom2Molc_Opts where
import System.Console.CmdArgs

data Ch_Geom2Molc_Opts = Ch_Geom2Molc_Opts
  { input :: FilePath
  , output :: FilePath
  }
  deriving (Show,Data,Typeable)

ch_Geom2Molc_Opts = Ch_Geom2Molc_Opts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  }

mode = cmdArgsMode ch_Geom2Molc_Opts
