module Bean.Sqlite.CurrentConnection.Env where

import Sqlite
import Control.Monad.Reader.Class
import  Bean.Sqlite.CurrentConnection
import Control.Exception

class HasConnection env where
    getConnection :: env -> Connection

make :: (MonadReader env m, HasConnection env) => CurrentConnection m
make = CurrentConnection {
    askCurrentConnection = do
        conn <- asks getConnection
        pure conn

}

data ConnectionMissing = ConnectionMissing deriving Show

instance Exception ConnectionMissing