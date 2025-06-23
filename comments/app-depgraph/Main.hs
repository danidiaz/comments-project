{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Comments.Root
  ( dependencyGraphMain,
  -- manuallyWiredAppMain,
  -- polymorphicallyWiredAppMain',
  -- polymorphicallyWiredAppMain'',
  )

main :: IO ()
main = dependencyGraphMain
