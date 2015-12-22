import System.Environment
import Data.String.Utils

data Player = Player
    { name :: String
    , hitPoints :: Int
    , damage :: Int
    , armor :: Int
	, equippedCost :: Int
    }

data Item = Item
    { iname :: String
    , icost :: Int
    , idamage :: Int
    , iarmor :: Int
    }

me :: Player
me = Player { name = "Me", hitPoints = 100, damage = 0, armor = 0, equippedCost = 0 }

nullItem :: Item
nullItem = Item { name = "Nothing", cost = 0, damage = 0, armor = 0 }

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

loadBoss :: [String] -> Player
loadBoss lineList = foldl loadLine baseBoss lineList
    where baseBoss = Player { name = "Boss", hitPoints = 0, damage = 0, armor = 0, equippedCost = 0}
          loadLine boss line = case splitWs line of
              ["Hit", "Points:", hp] -> boss { hitPoints = read hp }
              ["Damage:", d] -> boss { damage = read d }
              ["Armor:", a] -> boss { armor = read a }

loadItems :: Player -> [Item] -> Player
loadItems player items = foldl loadItem player items
    where loadItem player item =
              player { damage = damage player + damage item
        	         , armor = armor player + armor item
			         , equippedCost = equippedCost player + cost item
        	         }

--totalCost :: [Item] -> Int
--totalCost items = sum . map cost $ items
--
--fight :: Player -> Player -> Player
--fight p1 p2 = winner . find someoneWon . iterate singleFight $ (p1, p2)
--    where someoneWon (p1, p2) = hitPoints p1 <= 0 || hitPoints p2 <= 0
--          singleFight (attacker, defender) = (defenderAfterFight, attacker)
--		      where defenderAfterFith = defender { hitPoints = hitPoints defender - (max 1 (damage attacker - armor defender)) }
--	      winner (a, b) = a

solve :: [String] -> Int
solve bossLineList = 
	where boss = loadBoss bossLineList
		  ringCombinations = filter ((< 3) . length) . subsequences $ rings
	      equipementCombinations = [[w, a, r] | w <- weapons, a <- aromrs, r <-ringCombinations]
		  results = map (\items -> (fight boss . loadItems me items, totalCost items)) $ equipementCombinations

main = do
    [f] <- getArgs
    content <- readFile f
    weapons <- readFile "weapons.txt"
    armors <- readFile "armors.txt"
    rings <- readFile "rings.txt"
    putStrLn . show . solve weapons armors rings . lines $ content
