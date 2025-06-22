{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Comments.Root (
    dependencyGraphMain,
    -- manuallyWiredAppMain,
    -- polymorphicallyWiredAppMain',
    -- polymorphicallyWiredAppMain'',
    )

main :: IO ()
main = dependencyGraphMain
