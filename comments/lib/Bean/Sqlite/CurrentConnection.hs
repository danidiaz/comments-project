module Bean.Sqlite.CurrentConnection (CurrentConnection (..)) where

import Sqlite

newtype CurrentConnection m = CurrentConnection {askCurrentConnection :: m Connection}
