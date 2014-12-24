{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

import           Control.Applicative
import           Control.Exception          (SomeException, try)
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.IO               (hGetLine, putStrLn)
import Data.Text.Read(decimal)
import           Prelude                    hiding (putStrLn,read)
import System.Environment(getArgs)
import           System.IO                  (BufferMode (..), Handle,
                                             hSetBuffering)
import           System.Process
-- import           System.IO.Strict

myProcess :: FilePath -> CreateProcess
myProcess fp = CreateProcess {
    cmdspec = ShellCommand $ "mpv -volume 0 --no-msg-color --term-status-msg='${=time-pos}' '" <> fp <> "'"
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = CreatePipe
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  }

data ProcessOutput = ProcessOutput {
    _inHandle      :: Maybe Handle
  , _outHandle     :: Maybe Handle
  , _errHandle     :: Maybe Handle
  , _processHandle :: ProcessHandle
  }

convertOutput :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> ProcessOutput
convertOutput (ih,oh,eh,ph) = ProcessOutput ih oh eh ph

$(makeLenses ''ProcessOutput)

createProcessPretty :: CreateProcess -> IO ProcessOutput
createProcessPretty cp = convertOutput <$> createProcess cp

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

tryAndDoNothing :: forall a m.MonadIO m => IO a -> m (Maybe a)
tryAndDoNothing a = do
  r <- liftIO (try a :: IO (Either SomeException a))
  case r of
    Left _ -> return Nothing
    Right l -> return $ Just l

nextLine :: PlayStateMonad (Maybe Text)
nextLine = do
  h <- use streamHandle
  tryAndDoNothing (hGetLine h)

parseLine :: Text -> Maybe TimeStamp
parseLine t = case decimal t of
  Left _ -> Nothing
  Right (a,_) -> Just a

liftPutStrLn :: MonadIO m => Text -> m ()
liftPutStrLn = liftIO . putStrLn

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

main :: IO ()
main = do
  (filename:_) <- getArgs
  output <- createProcessPretty (myProcess filename)
  let ehandle = output ^?! (errHandle . _Just)
  hSetBuffering ehandle NoBuffering
  let ps = PlayState {
      _lastSeconds = 0
    , _currentSectionBegin = 0
    , _sections = []
    , _streamHandle = ehandle
    }
  finalState <- execStateT mainLoop ps
  print (filterSections 5 (finalState ^. sections))

