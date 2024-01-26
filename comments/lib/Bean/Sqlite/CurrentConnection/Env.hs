module Bean.Sqlite.CurrentConnection.Env (make) where

import Bean.Sqlite.CurrentConnection
import Control.Exception
import Control.Monad.Reader.Class
import Sqlite

-- | It might seem dumb to dedicate an entire bean to store the function that extracts
-- a 'Connection' from the Reader environment. Why not simply use 'asks' directly?
--
-- Maybe it's dumb. On the other hand, it's more flexible and more in line with
-- the "don't rely on the monad, accept records-of-functions instead" approach
-- used in this app.
make :: (MonadReader env m) => (env -> Connection) -> CurrentConnection m
make f =
  CurrentConnection
    { askCurrentConnection = do
        conn <- asks f
        pure conn
    }
