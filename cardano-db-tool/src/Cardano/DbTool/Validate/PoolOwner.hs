{-# LANGUAGE TypeApplications #-}
module Cardano.DbTool.Validate.PoolOwner
  ( validateAllPoolsHaveOwners
  ) where

import           Cardano.DbTool.Validate.Util

import           Cardano.Db

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Reader (ReaderT)

import           Database.Esqueleto.Experimental (SqlBackend, Value (..), countRows, from,
                   notExists, select, table, unValue, where_, (==.), (^.))

validateAllPoolsHaveOwners :: IO ()
validateAllPoolsHaveOwners = do
  putStrF "All pools have owners : "
  count <- runDbNoLogging queryPoolsWithoutOwners
  if count == 0
    then putStrLn $ greenText "ok"
    else putStrLn $ redText ("Failed, " ++ show count ++ " pools are without owners.")

-- -----------------------------------------------------------------------------

-- select * from pool_hash
--  where not exists (select * from pool_owner where pool_owner.pool_hash_id = pool_hash.id) ;

queryPoolsWithoutOwners :: MonadIO m => ReaderT SqlBackend m Int
queryPoolsWithoutOwners = do
    res <- select $ do
      phash <- from $ table @PoolHash
      where_ . notExists $ do
        powner <- from (table @PoolOwner)
        where_ (phash ^. PoolHashId ==. powner ^. PoolOwnerPoolHashId)
      pure countRows
    pure $ maybe 0 unValue (listToMaybe res)
