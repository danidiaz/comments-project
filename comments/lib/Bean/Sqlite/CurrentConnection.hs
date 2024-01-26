module Bean.Sqlite.CurrentConnection where

import Sqlite

newtype CurrentConnection m = CurrentConnection { askCurrentConnection :: m Connection }