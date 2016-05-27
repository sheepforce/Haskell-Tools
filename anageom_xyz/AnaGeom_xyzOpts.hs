{-# LANGUAGE DeriveDataTypeable #-}
module AnaGeom_xyzOpts where
import System.Console.CmdArgs

data AnaGeom_xyzOpts = MonitorOpts
  { input :: FilePath
  , output :: FilePath
  , atoms :: String
  , mdsteps :: Int
  , verbose :: Bool
  }
  deriving (Show,Data,Typeable)

anaGeom_xyzOpts = MonitorOpts
  { input = def &= help "trajectory in XYZ format" &= typ "INPUT"
  , output = "stdout" &= help "output file" &= typ "OUTPUT"
  , atoms = def &= help "list of atoms to monitor, space separated" &= typ "ATOMS"
  , mdsteps = 1 &= help "number of steps to be analysed" &= typ "STEPS"
  , verbose = False &= help "print additional output"
  }

mode = cmdArgsMode anaGeom_xyzOpts
