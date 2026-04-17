

import qualified Data.Map as Map
type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
    ,(2,"KINGinYELLOW")
    ,(3,"dagon1997")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
    ,("KINGinYELLOW",15000)
    ,("dagon1997",300)]

--creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = 

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB


lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username
-- Maybe UserName -> (UserName -> Maybe PlayerCredits) -> Maybe PlayerCredits

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)
-- creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id

 
creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id  >>= lookupCredits     

type WillCoId = Int
gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [(1001,1)
                ,(1002,2)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWillCoId :: WillCoId -> Maybe PlayerCredits
creditsFromWillCoId id = lookupGamerId id >>= lookupUserName >>= lookupCredits