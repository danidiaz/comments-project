{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-} -- For some convenience with the types in Managed

module Comments.Root
  ( cauldron,
    main,
  )
where

import Cauldron
import Cauldron.Managed
import Comments.Api (CommentsLinks, makeLinks)
import Comments.Api.Runner
import Comments.Api.Server
import Comments.Sqlite
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Data.Pool.Introspection.Bean (PoolConf)
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Functor ((<&>))
import JsonConf
import JsonConf.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Sqlite (Connection)
import ThreadLocal
import Comments.Api.WholeServer 
import Network.Wai.Bean

cauldron :: Cauldron Managed
cauldron =
  [ 
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in recipe @JsonConf $ pure makeJsonConf & ioEff,
    recipe @Logger $ pure withStdOutLogger <&> managed & eff,
    recipe @SqlitePoolConf $ JsonConf.lookupSection @SqlitePoolConf "sqlite" <$> arg & ioEff,
    recipe @PoolConf $ JsonConf.lookupSection @PoolConf "sqlite" <$> arg & ioEff,
    -- recipe @SqlitePool $ Comments.Sqlite.makeSqlitePool <$> arg <*> arg <&> managed & eff,
    recipe @SqlitePool $ Comments.Sqlite.makeSqlitePool & wire <&> managed & eff,
    recipe @(ThreadLocal Connection) $ pure makeThreadLocal & ioEff,
    recipe @(IO Connection) $ readThreadLocal @Connection <$> arg & val,
    recipe @CommentsRepository $ Comments.Repository.Sqlite.make <$> arg <*> arg & val,
    recipe @CommentsLinks $ pure makeLinks & ioEff,
    recipe @CommentsServer $ Recipe {
      bean = makeCommentsServer <$> arg <*> arg <*> arg & val,
      -- bean = makeCommentsServer & wire & val,
      decos = [
        -- Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer <$> arg <*> arg <*> arg & val
        Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer & wire & val
      ]
    },
    recipe @StaticServeConf $ JsonConf.lookupSection @StaticServeConf "runner" & wire & ioEff,
    -- recipe @Application_ $ val $ wire $ \(CommentsServer { server }) -> Comments.Api.WholeServer.makeApplication_ server,
    recipe @Application_ $ Comments.Api.WholeServer.makeApplication_ <$> fmap Comments.Api.Server.unwrap arg <*> arg & val,
    recipe @RunnerConf $ JsonConf.lookupSection @RunnerConf "runner" & wire & ioEff,
    recipe @Runner $ makeRunner & wire & val
  ]

main :: IO ()
main = do
  cook @Runner forbidDepCycles cauldron & either throwIO \action ->
    with action \(Runner {runServer}) -> do
      runServer
