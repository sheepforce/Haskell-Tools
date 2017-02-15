{-# LANGUAGE DeriveDataTypeable #-}
module Ch_Incide_ms_Opts where
import System.Console.CmdArgs

data Ch_Incide_ms_Opts = Ch_Incide_ms_Opts
  { input :: FilePath
  , output :: FilePath
  , complete :: Bool
  }
  deriving (Show,Data,Typeable)

ch_Incide_ms_Opts = Ch_Incide_ms_Opts
  { input = def &= help "INCIDE mass spectrum or trend" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  , complete = False &= help "print the complete spectrum or only first iteration"
  }

mode = cmdArgsMode ch_Incide_ms_Opts
