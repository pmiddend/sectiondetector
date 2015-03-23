{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module SectionDetector.DetectSections(
    detectSections
  , Section
  , sectionBegin
  , sectionEnd
  , sectionDuration
  , filterSections
  ) where

import Control.Concurrent(threadDelay)
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.State.Strict
import           Data.Aeson                 (FromJSON (..), Value (..), decode,
                                             (.:))
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Conduit               (Conduit, await, yield, ($$), ($=),
                                             (=$), (=$=))
import qualified Data.Conduit.List          as CL
import           Data.Conduit.Network.Unix
import qualified Data.Conduit.Text          as CT
import           Data.Monoid                (mempty, (<>))
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8', encodeUtf8)
import           Data.Text.IO               (hGetLine)
import qualified Data.Text.IO               as TIO
import           Data.Text.Read             (decimal, double)
import           Prelude                    hiding (putStrLn, read)
import           SectionDetector.Exception  (tryAndDoNothing)
import           SectionDetector.Lifted     (liftPutStrLn)
import           SectionDetector.Process    (createProcessPretty,
                                             processErrHandle)
import           System.IO                  (BufferMode (..), Handle,
                                             hSetBuffering)
import           System.Process

sectionDetectorSocketFile :: String
sectionDetectorSocketFile = "/tmp/sectiondetector"

playerCommandStr :: String -> Int -> FilePath -> String
playerCommandStr aspect start fp = "mpv --quiet --start="<> show start <>" --video-aspect " <> aspect <> " -volume 0 --input-unix-socket=" <> sectionDetectorSocketFile <> " '" <> fp <> "'"

playerCommand :: String -> Int -> FilePath -> CmdSpec
playerCommand aspect start fp = ShellCommand $ playerCommandStr aspect start fp

type TimeStamp = Double

parseTimeStamp :: T.Text -> Maybe TimeStamp
parseTimeStamp s = case double s of
  Left _ -> Nothing
  Right (x,_) -> Just x


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

data PlayState = PlayState {
    _lastSeconds         :: TimeStamp
  , _currentSectionBegin :: TimeStamp
  }

$(makeLenses ''PlayState)

initialPlayState :: PlayState
initialPlayState = PlayState {
      _lastSeconds = 0
    , _currentSectionBegin = 0
    }

mainLoop :: (MonadState PlayState m,MonadIO m) => Conduit TimeStamp m Section
mainLoop = do
  timeStamp' <- await
  case timeStamp' of
    Nothing -> return ()
    Just timeStamp -> do
      liftIO $ TIO.putStrLn $ "Got " <> ( T.pack . show $ timeStamp )
      ls <- use lastSeconds
      when (timeStamp - ls > 1) $ do
        liftIO $ TIO.putStrLn $ "Seeked!"
        csb <- use currentSectionBegin
        yield (Section (floor csb) (floor ls))
        --sections <>= [Section csb ls]
        currentSectionBegin .= timeStamp
      lastSeconds .= timeStamp
      mainLoop

toTimepos :: MpvJson -> Maybe Double
toTimepos e =
  case mpvEventName e of
    "time-pos" -> parseTimeStamp ( mpvEventData e )
    _ -> Nothing

detectSections :: String -> Int -> FilePath -> IO [Section]
detectSections aspect start filename = do
  TIO.putStrLn $ T.pack $ "Running " <> (playerCommandStr aspect start filename)
  output <- createProcessPretty (myProcess (playerCommand aspect start filename))
  threadDelay ( 1000*1000 )
  runUnixClient (clientSettings sectionDetectorSocketFile) $ \app -> do
    TIO.putStrLn "Yielding command"
    yield (encodeUtf8 "{ \"command\": [\"observe_property\", 1, \"time-pos\"]}\n") $$ appSink app
    TIO.putStrLn "Done, reading"
    evalStateT ( appSource app $= CT.decode CT.utf8 =$= CT.lines =$= CL.map ( decode . BSL.fromStrict . encodeUtf8 ) =$= CL.catMaybes =$= CL.map toTimepos =$= CL.catMaybes =$= mainLoop $$ CL.consume ) initialPlayState

filterSections :: Int -> [Section] -> [Section]
--filterSections ts = filter ((> ts) . (^. sectionDuration))
filterSections ts = id

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
