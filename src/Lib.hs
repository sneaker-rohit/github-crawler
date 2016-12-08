{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib
	(
startApp
	) where

import Followers (showFollowers)

import qualified GitHub as GH

startApp = do
    -- possibleUser <- GH.executeRequest' $ GH.userInfoForR "torvalds"
    -- print possibleUser
    showFollowers
    