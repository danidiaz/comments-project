{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Comments.Root (
    depDiagramsMain,
    -- manuallyWiredAppMain,
    -- polymorphicallyWiredAppMain',
    -- polymorphicallyWiredAppMain'',
    )

main :: IO ()
main = depDiagramsMain
