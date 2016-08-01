{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database where

import Database.Persist.TH

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
Character
    username        String
    UniqueUsername  username
    userID          String
    UniqueUserID    userID
    accessToken     String
    refreshToken    String
    deriving        Show
|]
