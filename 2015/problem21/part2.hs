import System.Environment
import Data.List
import Data.Maybe
import Data.String.Utils

data Player = Player
    { name :: String
    , hitPoints :: Int
    , damage :: Int
    , armor :: Int
    , equippedCost :: Int
    } deriving (Show)

data Item = Item
    { iname :: String
    , icost :: Int
    , idamage :: Int
    , iarmor :: Int
    } deriving (Show)

me :: Player
me = Player { name = "Me", hitPoints = 100, damage = 0, armor = 0, equippedCost = 0 }

nullItem :: Item
nullItem = Item { iname = "Nothing", icost = 0, idamage = 0, iarmor = 0 }

loadBoss :: [String] -> Player
loadBoss lineList = foldl loadLine baseBoss lineList
    where baseBoss = Player { name = "Boss", hitPoints = 0, damage = 0, armor = 0, equippedCost = 0}
          loadLine boss line = case splitWs line of
              ["Hit", "Points:", hp] -> boss { hitPoints = read hp }
              ["Damage:", d] -> boss { damage = read d }
              ["Armor:", a] -> boss { armor = read a }

parseItemLine :: String -> Item
parseItemLine line = case splitWs line of
    [name, c, d, a] ->
        Item { iname = name
             , icost = read c
             , idamage = read d
             ,  iarmor = read a }
    [name, name2, c, d, a] ->
        Item { iname = name ++ name2
             , icost = read c
             , idamage = read d
             , iarmor = read a }

loadItemsFromString :: String -> [Item]
loadItemsFromString string = map parseItemLine . lines $ string

loadItems :: Player -> [Item] -> Player
loadItems player items = foldl loadItem player items
    where loadItem player item =
              player { damage = damage player + idamage item
                     , armor = armor player + iarmor item
                     , equippedCost = equippedCost player + icost item
                     }

totalCost :: [Item] -> Int
totalCost items = sum . map icost $ items

fight :: Player -> Player -> (Player, Player)
fight boss player = if hitsBoss <= hitsPlayer then (player, boss) else (boss, player)
    where hitsBoss = ceiling $ (fromIntegral . hitPoints $ boss) / (fromIntegral . max 1 $ damage player - armor boss)
          hitsPlayer = ceiling $ (fromIntegral . hitPoints $ player) / (fromIntegral . max 1 $ damage boss - armor player)

solve :: String -> String -> String -> [String] -> Int
solve weaponStr armorStr ringStr bossLineList = maximum . map (equippedCost . snd) . filter ((== "Me") . name . snd) $ results
    where boss = loadBoss bossLineList
          weapons = loadItemsFromString weaponStr
          armors = nullItem:(loadItemsFromString armorStr)
          rings = nullItem:(loadItemsFromString ringStr)
          equipementCombinations = [[w, a, r1, r2] | w <- weapons, a <- armors, r1 <- rings, r2 <- rings]
          results = map (fight boss . loadItems me) $ equipementCombinations

main = do
    [f] <- getArgs
    bossFileContent <- readFile f
    weaponStr <- readFile "weapons.txt"
    armorStr <- readFile "armors.txt"
    ringStr <- readFile "rings.txt"
    putStrLn . show . solve weaponStr armorStr ringStr. lines $ bossFileContent
