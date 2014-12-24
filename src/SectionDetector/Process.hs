{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module SectionDetector.Process(
    ProcessOutput
  , processInHandle
  , processOutHandle
  , processErrHandle
  , processHandle
  , createProcessPretty
  ) where

import           Control.Applicative
import           Control.Lens
import           System.IO           (Handle)
import           System.Process

data ProcessOutput = ProcessOutput {
    _processInHandle  :: Maybe Handle
  , _processOutHandle :: Maybe Handle
  , _processErrHandle :: Maybe Handle
  , _processHandle    :: ProcessHandle
  }

convertOutput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> ProcessOutput
convertOutput (ih,oh,eh,ph) = ProcessOutput ih oh eh ph

$(makeLenses ''ProcessOutput)

createProcessPretty :: CreateProcess -> IO ProcessOutput
createProcessPretty cp = convertOutput <$> createProcess cp

