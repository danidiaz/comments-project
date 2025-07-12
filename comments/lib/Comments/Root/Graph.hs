{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImpredicativeTypes #-} -- For some convenience with the types in Managed

module Comments.Root.Graph (Comments.Root.Graph.main) where

import Cauldron
import Comments.Root (cauldron)
import Comments.Api.Runner
import System.Process
import Control.Concurrent.Async (forConcurrently_)
import System.FilePath (replaceExtension)
import Control.Concurrent.QSem
import Control.Exception (bracket_)

main :: IO ()
main = do
  let merr = case cook @Runner forbidDepCycles [cauldron] of
        Left err -> Just err
        Right _ -> Nothing
  let depGraph = getDependencyGraph [cauldron]
  qsem <- newQSem 1
  forConcurrently_ @[] [
    let file = "beans.dot" in (file, writeAsDot (defaultStyle merr) file),
    let file = "beans-decos.dot" in (file, writeAsDot (defaultStyle merr) file . collapseBeans . removeAggregates),
    let file = "beans-simple.dot" in (file, writeAsDot (defaultStyle merr) file . collapseBeans . removeDecos)
    ] \(file, action) -> do 
          _ <- action depGraph
          let svg = replaceExtension file "svg" 
          callCommand $ "dot -Tsvg " ++ file ++ " > " ++ svg
          bracket_
            (waitQSem qsem)
            (signalQSem qsem)
            (putStrLn $ "Refreshed " ++ svg)
