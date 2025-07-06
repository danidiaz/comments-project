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
-- import Cauldron.Args
import Cauldron.Managed
import Comments.Api (CommentsLinks, makeLinks)
import Comments.Api.Runner
import Comments.Api.Server
import Comments.Sqlite
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Data.Pool.Introspection.Bean (PoolConf)
import Control.Exception (throwIO)
-- import Control.Monad.IO.Class
import Data.Function ((&))
-- import Data.Functor ((<&>))
import JsonConf
import JsonConf.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Sqlite (Connection)
import ThreadLocal
import Comments.Api.WholeServer 
import Network.Wai.Bean
-- import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

cauldron :: Cauldron Managed
cauldron =
  [ 
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in recipe @JsonConf $ ioEff_ $ pure makeJsonConf,
    -- recipe @Logger $ eff_ $ pure withStdOutLogger <&> managed,
    recipe @Logger $ eff_ $ managed <$> pure withStdOutLogger,
    recipe @SqlitePoolConf $ ioEff_ $ JsonConf.lookupSection @SqlitePoolConf "sqlite" <$> arg,
    recipe @PoolConf $ ioEff_ $ JsonConf.lookupSection @PoolConf "sqlite" <$> arg,
    -- recipe @SqlitePool $ Comments.Sqlite.makeSqlitePool <$> arg <*> arg <&> managed & eff,
    recipe @SqlitePool $ eff_ $ managed <$> wire Comments.Sqlite.makeSqlitePool,
    recipe @(ThreadLocal Connection) $ ioEff_ $ pure makeThreadLocal,
    recipe @(IO Connection) $ val_ $ readThreadLocal @Connection <$> arg,
    -- recipe @CommentsRepository $ val_ $ Comments.Repository.Sqlite.make <$> arg <*> arg,
    recipe @CommentsRepository $ val_ $ Comments.Repository.Sqlite.make <$> arg <*> arg,
    recipe @CommentsLinks $ ioEff_ $ pure makeLinks,
    recipe @CommentsServer $ Recipe {
      bean = val_ $ makeCommentsServer <$> arg <*> arg <*> arg,
      -- bean = makeCommentsServer & wire & val,
      decos = [
        -- Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer <$> arg <*> arg <*> arg & val
        val_ $ wire $ Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer
      ]
    },
    recipe @StaticServeConf $ ioEff_ $ wire $ JsonConf.lookupSection @StaticServeConf "runner",
    -- recipe @Application_ $ val $ wire $ \(CommentsServer { server }) -> Comments.Api.WholeServer.makeApplication_ server,
    recipe @Application_ $ val_ $ Comments.Api.WholeServer.makeApplication_ <$> fmap Comments.Api.Server.unwrap arg <*> arg,
    recipe @RunnerConf $ ioEff_ $ wire $ JsonConf.lookupSection @RunnerConf "runner",
    recipe @Runner $ val_ $ wire makeRunner
  ]

main :: IO ()
main = do
  cook @Runner forbidDepCycles cauldron & either throwIO \action ->
    with action \(Runner {runServer}) -> do
      runServer

-- managedEff_ :: forall bean . (HasCallStack) => Args (forall x . (bean -> IO x) -> IO x) -> Constructor Managed bean
-- managedEff_ args = withFrozenCallStack (eff_ $ managed <$> args)

