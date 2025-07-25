{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Root
  ( cauldron,
    main,
  )
where

import Cauldron
-- import Cauldron.Args
import Cauldron.Managed
import Comments.Api (CommentsLinks, makeLinks)
import Network.Wai.Handler.Warp.Runner
import Comments.Api.Server
-- import Control.Monad.IO.Class

-- import Data.Functor ((<&>))

import Comments.Api.WholeServer
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Comments.Sqlite
import Control.Exception (throwIO)
import Data.Function ((&))
import Data.Pool.Introspection.Bean (PoolConf)
import JsonConf
import JsonConf.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Network.Wai.Bean
import Sqlite (Connection)
import ThreadLocal

-- import qualified Comments.Repository.Memory
-- import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

cauldron :: Cauldron Managed
cauldron =
  mconcat
    [ let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
       in recipe @JsonConf $ ioEff_ $ pure makeJsonConf,
      recipe @Logger $ eff_ $ pure $ managed withStdOutLogger,
      recipe @SqlitePoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @SqlitePoolConf "sqlite",
      recipe @PoolConf $ ioEff_ $ wire $ JsonConf.lookupSection @PoolConf "sqlite",
      recipe @SqlitePool $ eff_ $ wire $ \sconf pconf -> managed $ Comments.Sqlite.makeSqlitePool sconf pconf,
      recipe @(ThreadLocal Connection) $ ioEff_ $ pure makeThreadLocal,
      -- IO Connection |=| val_ $ readThreadLocal @Connection <$> arg,
      recipe @(IO Connection) $ val_ $ wire $ readThreadLocal @Connection,
      recipe @CommentsRepository $ val_ $ wire $ Comments.Repository.Sqlite.make,
      recipe @CommentsLinks $ ioEff_ $ pure makeLinks,
      recipe @CommentsServer $
        Recipe
          { bare = val_ $ wire makeCommentsServer,
            decos =
              [ val_ $ wire $ Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer
              ]
          },
      recipe @StaticServeConf $ ioEff_ $ wire $ JsonConf.lookupSection @StaticServeConf "runner",
      recipe @Application_ $ val_ $ Comments.Api.WholeServer.makeApplication_ <$> fmap Comments.Api.Server.unwrap arg <*> arg,
      recipe @RunnerConf $ ioEff_ $ wire $ JsonConf.lookupSection @RunnerConf "runner",
      recipe @Runner $ Recipe {
        bare = val_ $ wire makeRunner,
        decos = 
          [
            val_ $ wire $ \logger (conf :: RunnerConf) -> hoistRunner \action -> do
              logInfo "Server started" conf & runLogT "runner" logger defaultLogLevel
              action
          ]
      }
    ]

-- <> [
--   recipe @CommentsRepository $ ioEff_ $ pure Comments.Repository.Memory.make
-- ]

main :: IO ()
main = do
  cauldron
    & cook @Runner forbidDepCycles
    & either throwIO \action ->
      with action runApplication
