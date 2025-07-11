{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}

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
-- import qualified Comments.Repository.Memory
-- import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

cauldron :: Cauldron Managed
cauldron = mconcat [ 
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in singleton @JsonConf $ ioEff_ $ pure makeJsonConf,
    singleton @Logger $ eff_ $ pure $ managed withStdOutLogger,
    singleton @SqlitePoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @SqlitePoolConf "sqlite",
    singleton @PoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @PoolConf "sqlite",
    singleton @SqlitePool $ eff_ $ wire $ \sconf pconf -> managed $ Comments.Sqlite.makeSqlitePool sconf pconf,
    singleton @(ThreadLocal Connection) $ ioEff_ $ pure makeThreadLocal,
    -- IO Connection |=| val_ $ readThreadLocal @Connection <$> arg,
    singleton @(IO Connection) $ val_ $ wire $ readThreadLocal @Connection,
    singleton @CommentsRepository $ val_ $ wire $ Comments.Repository.Sqlite.make,
    singleton @CommentsLinks $ ioEff_ $ pure makeLinks,
    singleton @CommentsServer $ Recipe {
      bare = val_ $ wire makeCommentsServer,
      decos = [
        val_ $ wire $ Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer
      ]
    },
    singleton @StaticServeConf $ ioEff_ $ wire $ JsonConf.lookupSection @StaticServeConf "runner",
    singleton @Application_ $ val_ $ Comments.Api.WholeServer.makeApplication_ <$> fmap Comments.Api.Server.unwrap arg <*> arg,
    singleton @RunnerConf $ ioEff_ $ wire $ JsonConf.lookupSection @RunnerConf "runner",
    singleton @Runner $ val_ $ wire makeRunner
    -- (type Runner) |=| val_ $ wire makeRunner
  ]
  -- <> [
  --   singleton @CommentsRepository $ ioEff_ $ pure Comments.Repository.Memory.make
  -- ]

main :: IO ()
main = do
  [cauldron] 
    & cook @Runner forbidDepCycles 
    & either throwIO \action ->
        with action \(Runner {runServer}) -> do
          runServer
