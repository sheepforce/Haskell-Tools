{-# LANGUAGE DeriveDataTypeable #-}
module Ch_HessConv_Opts where
import System.Console.CmdArgs

data Ch_HessConv_Opts = Ch_HessConv_Opts
  { input :: FilePath
  , output :: FilePath
  , addInput :: FilePath
  , inputFormat :: String
  , outputFormat :: String
  }
  deriving (Show,Data,Typeable)

ch_hessConv_Opts = Ch_HessConv_Opts
  { input = def &= help "file with input hessian" &= typ "INPUT"
  , output = "stdout" &= help "file for output hessian" &= typ "OUTPUT"
  , addInput = def &= help "additional input needed, e.g. xyz-file with coordinates" &= typ "INPUT"
  , inputFormat = def &= help "format of the input hessian [nwchem,dalton,(orca)]"
  , outputFormat = def &= help "format of the output hessian [nwchem,dalton,(orca)]"
  }

mode = cmdArgsMode ch_hessConv_Opts
