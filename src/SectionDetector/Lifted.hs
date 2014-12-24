module SectionDetector.Lifted(
  liftPutStrLn
  ) where

import           Control.Monad.State.Strict
import           Data.Text                  (Text)
import           Data.Text.IO               ( putStrLn)
import           Prelude                    hiding (putStrLn, read)

liftPutStrLn :: MonadIO m => Text -> m ()
liftPutStrLn = liftIO . putStrLn
