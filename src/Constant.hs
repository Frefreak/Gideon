{-# LANGUAGE OverloadedStrings #-}
module Constant where

import System.Directory
import System.FilePath
import Network.Wreq
import Control.Lens.Operators
import Data.Text (pack, Text)

import Network.HTTP.Client.TLS
import Network.HTTP.Client
import Network.Connection
import Data.Default

appName :: String
appName = "Gideon"

getAppRoot :: IO FilePath
getAppRoot = getAppUserDataDirectory appName

gideonOpt :: Options
gideonOpt = defaults & header "User-Agent" .~ ["Gideon!!!"]
                & header "Accept" .~ ["application/vnd.ccp.eve.Api-v5+json"]
                & manager .~ Left insecureSetting

insecureSetting :: ManagerSettings
insecureSetting = mkManagerSettings
    (def { settingDisableCertificateValidation = True }) Nothing

clientID :: String
clientID = "a6de2072ac1c464295e2432827a20101"

urlSSO :: String
urlSSO = "https://login.eveonline.com/oauth/authorize"

urlVerify :: String
urlVerify = "https://login.eveonline.com/oauth/token"

urlCharacterInfo :: String
urlCharacterInfo = "https://login.eveonline.com/oauth/verify"

callbackPort :: Int
callbackPort = 5747

callbackUri :: String
callbackUri = "http://127.0.0.1:" ++ show callbackPort ++ "/callback"

allScope :: [String]
allScope = ["characterAccountRead", "characterAssetsRead",
            "characterBookmarksRead", "characterCalendarRead",
            "characterChatChannelsRead", "characterClonesRead",
            "characterContactsRead", "characterContactsWrite",
            "characterContractsRead", "characterFactionalWarfareRead",
            "characterFittingsRead", "characterFittingsWrite",
            "characterIndustryJobsRead", "characterKillsRead",
            "characterLocationRead", "characterLoyaltyPointsRead",
            "characterMailRead", "characterMarketOrdersRead",
            "characterMedalsRead", "characterNavigationWrite",
            "characterNotificationsRead", "characterOpportunitiesRead",
            "characterResearchRead", "characterSkillsRead",
            "characterStatsRead", "characterWalletRead",
            "corporationAssetRead", "corporationBookmarksRead",
            "corporationContractsRead", "corporationFactionalWarfareRead",
            "corporationIndustryJobsRead", "corporationKillsRead",
            "corporationMarketOrdersRead", "corporationMedalsRead",
            "corporationMembersRead", "corporationShareholdersRead",
            "corporationStructuresRead", "corporationWalletRead",
            "fleetRead", "fleetWrite", "publicData", "remoteClientUI",
            "structureVulnUpdate"]

getSqlUser :: IO Text
getSqlUser = pack . (</> "user.db") <$> getAppRoot

getMetaDataFile :: IO FilePath
getMetaDataFile = (</> ".meta.yaml") <$> getAppRoot

xmlUrl :: String
xmlUrl = "https://api.eveonline.com/"

crestUrl :: String
crestUrl = "https://crest-tq.eveonline.com/"

databasePath :: IO FilePath    -- assume store yaml database in Gideon folder
databasePath = (</> "Gideon/sde") <$> getHomeDirectory
