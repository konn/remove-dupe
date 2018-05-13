{-# LANGUAGE FlexibleContexts, TypeApplications #-}
module Main where
import           Conduit
import           Control.Monad.State       (MonadState (..), gets, modify)
import           Control.Monad.Trans.State (evalStateT)
import           Crypto.Hash               (SHA256)
import           Crypto.Hash.Conduit       (hashFile)
import           Data.Byteable             (toBytes)
import qualified Data.ByteString           as BS
import qualified Data.Conduit.Combinators  as C
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as HS
import           System.Directory          (removeFile)
import           System.Environment        (getArgs)

type Env = HashSet BS.ByteString

main :: IO ()
main = do
  dir : _ <- getArgs
  runResourceT $ flip evalStateT HS.empty $
    runConduit $
      C.sourceDirectory dir .| C.mapM (\fp -> (,) fp . toBytes <$> hashFile @_ @SHA256 fp)
                            .| C.mapM_ process

process :: (MonadState Env m, MonadIO m) => (FilePath, BS.ByteString) -> m ()
process (fp, d) = do
  b <- gets $ HS.member d
  if b
    then liftIO $ do putStrLn $ "removed: " ++ fp ; removeFile fp
    else modify $ HS.insert d
