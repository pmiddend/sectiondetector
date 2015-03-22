{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module SectionDetector.DetectSections(
    detectSections
  , Section
  , sectionBegin
  , sectionEnd
  , sectionDuration
  , filterSections
  ) where

import Control.Applicative
import Data.Aeson(decode,FromJSON(..),Value(..),(.:))
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Monoid                ((<>),mempty)
import           Data.Text                  (Text,pack)
import           Data.Text.IO               (hGetLine)
import           Data.Text.Read             (decimal)
import           Prelude                    hiding (putStrLn, read)
import           SectionDetector.Exception  (tryAndDoNothing)
import           SectionDetector.Lifted     (liftPutStrLn)
import           SectionDetector.Process    (createProcessPretty,
                                             processErrHandle)
import           System.IO                  (BufferMode (..), Handle,
                                             hSetBuffering)
import           System.Process
import Data.Conduit(await,yield,Conduit,(=$=),(=$),($=),($$))
import qualified Data.Conduit.List as CL
import Data.Conduit.Network.Unix
import qualified Data.Conduit.Text as CT
import Data.Text.Encoding(decodeUtf8',encodeUtf8)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sectionDetectorSocketFile = "/tmp/sectiondetector"

playerCommand :: String -> Int -> FilePath -> CmdSpec
playerCommand aspect start fp = ShellCommand $ "mpv --start="<> show start <>" --video-aspect " <> aspect <> " -volume 0 --input-unix-socket=" <> sectionDetectorSocketFile <> " '" <> fp <> "'"

type TimeStamp = Int

data Section = Section {
      _sectionBegin :: Int
    , _sectionEnd   :: Int
  } deriving(Show)

sectionDuration :: Getter Section Int
sectionDuration = to (\(Section b e) -> e - b)

$(makeLenses ''Section)

myProcess :: CmdSpec -> CreateProcess
myProcess fp = CreateProcess {
    cmdspec = fp
  , cwd = Nothing
  , env = Nothing
  , std_in = Inherit
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  }

data MpvJson = MpvJson {
    mpvEventType :: T.Text
  , mpvEventName :: T.Text
  , mpvEventData :: T.Text 
  }

instance FromJSON MpvJson where
  parseJSON (Object v) = MpvJson <$> (v .: "event") <*> (v .: "name") <*> (v .: "data")
  parseJSON _ = mzero

decodeJsonOrFilter = do
  v <- await
  case v of
    Nothing -> return ()
    Just v' -> case decode v' of
      Nothing -> decodeJsonOrFilter
      Just v'' -> yield v'' >> decodeJsonOrFilter

data PlayState = PlayState {
    _lastSeconds         :: TimeStamp
  , _currentSectionBegin :: TimeStamp
  , _sections            :: [Section]
  , _streamHandle        :: Handle
  }

$(makeLenses ''PlayState)

mainLoop :: Conduit String (State PlayState) Section
mainLoop = do
  timepos' <- await
  case timepos' of
    Nothing -> return ()
    Just timeStamp -> do
      ls <- use lastSeconds
      when (timeStamp - ls > 1) $ do
      --  liftPutStrLn "Seeked!"
        csb <- use currentSectionBegin
        sections <>= [Section csb ls]
        currentSectionBegin .= timeStamp
      lastSeconds .= timeStamp
      mainLoop

toTimepos e =
  case mpvEventName e of
    Just "time-pos" -> join mpvEventData
    _ -> Nothing

detectSections :: String -> Int -> FilePath -> IO [Section]
detectSections aspect start filename = do
  output <- createProcessPretty (myProcess (playerCommand aspect start filename))
  runUnixClient (clientSettings sectionDetectorSocketFile) $ \app -> do
    yield (encodeUtf8 "{ \"command\": [\"observe_property\", 1, \"time-pos\"]}") $$ appSink app
    appSource app $= CT.decode CT.utf8 =$= CT.lines =$= CL.map decode =$= CL.catMaybes =$= toTimepos =$= CL.catMaybes

filterSections :: TimeStamp -> [Section] -> [Section]
filterSections ts = filter ((> ts) . (^. sectionDuration))

{-

type PlayStateMonad = StateT PlayState IO

nextLine :: PlayStateMonad (Maybe Text)
nextLine = do
  h <- use streamHandle
--   Just <$> liftIO (hGetLine h)
  tryAndDoNothing (hGetLine h)

parseLine :: Text -> Maybe TimeStamp
parseLine t = case decimal t of
  Left _ -> Nothing
  Right (a,_) -> Just a

mainLoop :: PlayStateMonad ()
mainLoop = do
  line' <- nextLine
--   liftPutStrLn (pack (show line'))
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
  , std_err = Inherit
  , close_fds = False
  , create_group = False
  , delegate_ctlc = False
  }

detectSections :: String -> Int -> FilePath -> IO [Section]
detectSections aspect start filename = do
  output <- createProcessPretty (myProcess (playerCommand aspect start filename))
  --let ehandle = output ^?! (processErrHandle . _Just)
  --hSetBuffering ehandle NoBuffering
  let ps = PlayState {
      _lastSeconds = 0
    , _currentSectionBegin = 0
    , _sections = []
    , _streamHandle = ehandle
    }
  finalState <- execStateT mainLoop ps
  return (finalState ^. sections)

-}
