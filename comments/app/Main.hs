{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Comments (
    appMain,
    -- manuallyWiredAppMain,
    -- polymorphicallyWiredAppMain',
    -- polymorphicallyWiredAppMain'',
    )

main :: IO ()
main = appMain
