{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.IO               (hGetLine)
import           Data.Text.Read             (decimal)
import           Prelude                    hiding (putStrLn, read)
import           SectionDetector.Exception  (tryAndDoNothing)
import           SectionDetector.Lifted     (liftPutStrLn)
import           SectionDetector.Process    (createProcessPretty,
                                             processErrHandle)
import           System.Environment         (getArgs)
import           System.IO                  (BufferMode (..), Handle,
                                             hSetBuffering)
import           System.Process

playerCommand :: FilePath -> CmdSpec
playerCommand fp = ShellCommand $ "mpv -volume 0 --no-msg-color --term-status-msg='${=time-pos}' '" <> fp <> "'"

type TimeStamp = Int

data Section = Section {
      _sectionBegin :: Int
    , _sectionEnd   :: Int
  } deriving(Show)

sectionDuration :: Getter Section Int
sectionDuration = to (\(Section b e) -> e - b)

$(makeLenses ''Section)

data PlayState = PlayState {
    _lastSeconds         :: TimeStamp
  , _currentSectionBegin :: TimeStamp
  , _sections            :: [Section]
  , _streamHandle        :: Handle
  }

$(makeLenses ''PlayState)

type PlayStateMonad = StateT PlayState IO

nextLine :: PlayStateMonad (Maybe Text)
nextLine = do
  h <- use streamHandle
  tryAndDoNothing (hGetLine h)

parseLine :: Text -> Maybe TimeStamp
parseLine t = case decimal t of
  Left _ -> Nothing
  Right (a,_) -> Just a

-- TODO: Use something more intelligent here, like CPS, Cont, FRP?
mainLoop :: PlayStateMonad ()
mainLoop = do
  line' <- nextLine
  case line' of
    Nothing -> return ()
    Just line -> case parseLine line of
      Nothing -> liftPutStrLn ("unparsed: \"" <> line <> "\"") >> mainLoop
      Just timeStamp -> do
        ls <- use lastSeconds
        when (timeStamp - ls > 1) $ do
          liftPutStrLn "Seeked!"
          csb <- use currentSectionBegin
          sections <>= [Section csb ls]
          currentSectionBegin .= timeStamp
        lastSeconds .= timeStamp
        mainLoop

filterSections :: TimeStamp -> [Section] -> [Section]
filterSections ts = filter ((> ts) . (^. sectionDuration))

myProcess :: CmdSpec -> CreateProcess
myProcess fp = CreateProcess {
    cmdspec = fp
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = CreatePipe
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  }

main :: IO ()
main = do
  (filename:_) <- getArgs
  output <- createProcessPretty (myProcess (playerCommand filename))
  let ehandle = output ^?! (processErrHandle . _Just)
  hSetBuffering ehandle NoBuffering
  let ps = PlayState {
      _lastSeconds = 0
    , _currentSectionBegin = 0
    , _sections = []
    , _streamHandle = ehandle
    }
  finalState <- execStateT mainLoop ps
  print (filterSections 5 (finalState ^. sections))

