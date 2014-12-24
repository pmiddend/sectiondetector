{-# LANGUAGE ScopedTypeVariables #-}
module SectionDetector.Exception(
    tryAndDoNothing
  ) where

import           Control.Exception          (SomeException, try)
import           Control.Monad.State.Strict
import           Prelude                    hiding (putStrLn, read)

tryAndDoNothing :: forall a m.MonadIO m => IO a -> m (Maybe a)
tryAndDoNothing a = do
  r <- liftIO (try a :: IO (Either SomeException a))
  case r of
    Left _ -> return Nothing
    Right l -> return $ Just l


