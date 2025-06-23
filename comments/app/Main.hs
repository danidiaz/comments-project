{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Comments.Root
  ( appMain,
  -- manuallyWiredAppMain,
  -- polymorphicallyWiredAppMain',
  -- polymorphicallyWiredAppMain'',
  )

main :: IO ()
main = appMain
