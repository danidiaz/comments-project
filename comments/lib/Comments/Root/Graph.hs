{-# LANGUAGE BlockArguments #-}
-- For some convenience with the types in Managed
{-# LANGUAGE ImpredicativeTypes #-}

module Comments.Root.Graph (Comments.Root.Graph.main) where

import Cauldron
import Comments.Root (cauldron)
import Control.Concurrent.Async (forConcurrently_)
import Network.Wai.Handler.Warp.Runner (Runner)
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import System.FilePath (replaceExtension)
import System.Process

main :: IO ()
main = do
  let merr = case cook @Runner forbidDepCycles cauldron of
        Left err -> Just err
        Right _ -> Nothing
  let depGraph = getDependencyGraph cauldron
  qsem <- newQSem 1
  forConcurrently_ @[]
    [ let file = "beans.dot" in (file, writeAsDot (defaultStyle merr) file),
      let file = "beans-decos.dot" in (file, writeAsDot (defaultStyle merr) file . collapseBeans . removeAggregates),
      let file = "beans-simple.dot" in (file, writeAsDot (defaultStyle merr) file . collapseBeans . removeDecos)
    ]
    \(file, action) -> do
      _ <- action depGraph
      let svg = replaceExtension file "svg"
      callCommand $ "dot -Tsvg " ++ file ++ " > " ++ svg
      bracket_
        (waitQSem qsem)
        (signalQSem qsem)
        (putStrLn $ "Refreshed " ++ svg)
