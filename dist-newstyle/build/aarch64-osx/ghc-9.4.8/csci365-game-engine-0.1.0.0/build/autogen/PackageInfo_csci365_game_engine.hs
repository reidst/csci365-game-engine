{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_csci365_game_engine (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "csci365_game_engine"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A text-based game engine and an example dungeon crawler."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
