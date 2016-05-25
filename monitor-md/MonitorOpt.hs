{-# LANGUAGE DeriveDataTypeable #-}
module MonitorOpt where
import System.Console.CmdArgs

data MonitorOpts = MonitorOpts
  { input :: FilePath
  , output :: FilePath
  , atoms :: String
  , mdsteps :: Int
  }
  deriving (Show,Data,Typeable)

monitorOpts = MonitorOpts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = def &= help "output file" &= typ "OUTPUT"
  , atoms = def &= help "list of atoms to monitor, space separated" &= typ "ATOMS"
  , mdsteps = def &= help "number of steps to be analysed" &= typ "STEPS"
  }

mode = cmdArgsMode monitorOpts