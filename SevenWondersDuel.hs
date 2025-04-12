module SevenWondersDuel where

    import Data.List
    import Data.Maybe (mapMaybe)

    type Name = String                 -- kártya vagy játékos neve
    type Drachma = Int                 -- a pénz a játékban
    type Point = Int                   -- pontszám
    type Shield = Int                  -- pajzsok száma
    type Cost = ([Product], Drachma)   -- a kártya költsége (a `Product` típus később lesz definiálva)
    type Table = [[Maybe Card]]        -- a játék tábla (a `Card` típus később lesz definiálva)
    type IsBuilt = Bool                -- meg van-e építve (a csodáknál lesz használva)

    data Product = Clay Int | Wood Int | Stone Int | Glass Int | Papyrus Int
        deriving (Show, Eq)

    data Symbol = Globe | Wheel | Sundial | Mortar | Pendulum | Quill
        deriving (Show, Eq)

    data Effect = Price Product | Money Drachma | MoneyByCard Card Drachma | PointsByCard (Either Card WonderCard) Point
        deriving (Show, Eq)

    data Card = Materials Name Cost Product | Civilian Name Cost Point | Scientific Name Cost Point Symbol | Military Name Cost Shield | Commercial Name Cost Point Effect | Guilds Name Cost Effect
        deriving (Show, Eq)

    data WonderCard = W Name Cost Point Shield Drachma
        deriving (Show, Eq)

    data Player = P Name [Card] [(WonderCard, IsBuilt)] Drachma
        deriving (Show, Eq)

    blankMaterialsCard = Materials "" ([], 0) (Stone 0)
    blankMilitaryCard = Military "" ([], 0) 0
    blankCivilianCard = Civilian "" ([], 0) 0
    blankScientificCard = Scientific "" ([], 0) 0 Quill
    blankCommercialCard = Commercial "" ([], 0) 0 (Price (Stone 0))
    blankWonderCard = W "" ([], 0) 0 0 0

    -- Alap pontok összeszámolása

    wonderCardPoints :: WonderCard -> Point
    wonderCardPoints (W _ _ points _ _) = points

    wonderCardShields :: WonderCard -> Shield
    wonderCardShields (W _ _ _ shields _) = shields

    wonderCardDrachma :: WonderCard -> Drachma
    wonderCardDrachma (W _ _ _ _ drachma) = 0

    countWonderPoints :: Player -> (Point, Shield, Drachma)
    countWonderPoints (P _ _ wonderCards _) = (sum (map wonderCardPoints builtWonders), sum (map wonderCardShields builtWonders), sum (map wonderCardDrachma builtWonders))
        where
            builtWonders :: [WonderCard]
            builtWonders = [wonderCard | (wonderCard, isBuilt) <- wonderCards, isBuilt]

    cardPoints :: Card -> Point
    cardPoints (Civilian _ _ point) = point
    cardPoints (Scientific _ _ point _) = point
    cardPoints Guilds {} = 1
    cardPoints _ = 0

    cardShields :: Card -> Shield
    cardShields (Military _ _ shields) = shields
    cardShields _ = 0

    cardDrachma :: Card -> Drachma
    cardDrachma _ = 0

    countCardPoints :: Player -> (Point, Shield, Drachma)
    countCardPoints (P _ cards _ _) = (sum (map cardPoints cards), sum (map cardShields cards), sum (map cardDrachma cards))

    shieldPoints :: Shield -> Point
    shieldPoints shields
        | shields >= 6 = 10
        | shields >= 3 = 5
        | shields >= 1 = 2
        | otherwise = 0

    countBasicPoints :: Player -> Point
    countBasicPoints (P _ cards wonderCards n) = points + shieldPoints (wonderShields + cardShields) + drachmaPoints
        where
            (wonderPoints, wonderShields, wonderDrachma) = countWonderPoints (P "" cards wonderCards n)
            (cardPoints, cardShields, cardDrachma) = countCardPoints (P "" cards wonderCards n)
            
            points = wonderPoints + cardPoints

            drachmaPoints = (wonderDrachma + cardDrachma + n) `div` 3

    -- Katonai győzelem

    militaryVictory :: Player -> Player -> Maybe Player
    militaryVictory (P name1 cards1 wonderCards1 n1) (P name2 cards2 wonderCards2 n2)
        | difference >= 9 = Just winningPlayer
        | otherwise = Nothing
        where
            builtWonders1 :: [WonderCard]
            builtWonders1 = [wonderCard | (wonderCard, isBuilt) <- wonderCards1, isBuilt]
            builtWonders2 :: [WonderCard]
            builtWonders2 = [wonderCard | (wonderCard, isBuilt) <- wonderCards2, isBuilt]
            shields1 = sum (map cardShields cards1) + sum (map wonderCardShields builtWonders1)
            shields2 = sum (map cardShields cards2) + sum (map wonderCardShields builtWonders2)
            difference = abs (shields1 - shields2)
            winningPlayer
                | shields1 > shields2 = P name1 cards1 wonderCards1 n1
                | otherwise = P name2 cards2 wonderCards2 n2

    -- Kártya vagy csoda ára

    isMaterial :: Card -> Bool
    isMaterial Materials {} = True
    isMaterial _ = False

    onlyPosSub :: Int -> Int -> Int
    onlyPosSub n m
        |(n - m) > 0 = n - m
        | otherwise = 0

    coveredMaterials :: Product -> [Card] -> Int
    coveredMaterials product [] = 0
    coveredMaterials product (x:cards)
        | hasMat x product = materialAmount x + coveredMaterials product cards
        | otherwise = coveredMaterials product cards

    hasMat :: Card -> Product -> Bool
    hasMat (Materials _ _ (Clay n)) (Clay m) = True
    hasMat (Materials _ _ (Wood n)) (Wood m) = True
    hasMat (Materials _ _ (Stone n)) (Stone m) = True
    hasMat (Materials _ _ (Glass n)) (Glass m) = True
    hasMat (Materials _ _ (Papyrus n)) (Papyrus m) = True
    hasMat _ _ = False

    getProduct :: Card -> Maybe Product
    getProduct (Commercial _ _ _ (Price product)) = Just product
    getProduct _ = Nothing

    isCommercial :: Card -> Bool
    isCommercial Commercial {} = True
    isCommercial _ = False

    isFixPrice :: Maybe Product -> [Card] -> Bool
    isFixPrice Nothing cards = False
    isFixPrice (Just _) [] = False
    isFixPrice prod (x:cards)
        | isCommercial x && compareProduct (getProduct x) prod = True
        | otherwise = isFixPrice prod cards

    compareProduct :: Maybe Product -> Maybe Product -> Bool
    compareProduct (Just (Clay _)) (Just (Clay _)) = True
    compareProduct (Just (Wood _)) (Just (Wood _)) = True
    compareProduct (Just (Stone _)) (Just (Stone _)) = True
    compareProduct (Just (Glass _)) (Just (Glass _)) = True
    compareProduct (Just (Papyrus _)) (Just (Papyrus _)) = True
    compareProduct _ _ = False

    productAmount :: Product -> Int
    productAmount (Clay n) = n
    productAmount (Wood n) = n
    productAmount (Stone n) = n
    productAmount (Glass n) = n
    productAmount (Papyrus n) = n

    materialAmount :: Card -> Int
    materialAmount (Materials _ _ (Clay n)) = n
    materialAmount (Materials _ _ (Wood n)) = n
    materialAmount (Materials _ _ (Stone n)) = n
    materialAmount (Materials _ _ (Glass n)) = n
    materialAmount (Materials _ _ (Papyrus n)) = n
    materialAmount _ = 0

    productToMaybe :: Product -> Maybe Product
    productToMaybe (Clay n) = Just (Clay n)
    productToMaybe (Wood n) = Just (Wood n)
    productToMaybe (Stone n) = Just (Stone n)
    productToMaybe (Glass n) = Just (Glass n)
    productToMaybe (Papyrus n) = Just (Papyrus n)

    costInMoney :: Cost -> [Card] -> Drachma
    costInMoney ([], money) _ = money
    costInMoney (x:products, money) cards
        | isFixPrice (productToMaybe x) cards = onlyPosSub  (productAmount x) (coveredMaterials x cards) + costInMoney (products, money) cards
        | otherwise = (onlyPosSub (productAmount x) (coveredMaterials x cards) * 3) + costInMoney (products, money) cards

    -- Céh kártyák hatása

    compareCards :: Card -> Card -> Bool --Card osszehasonlitasa
    compareCards Materials {} Materials {} = True
    compareCards Civilian {} Civilian {} = True
    compareCards Scientific {} Scientific {} = True
    compareCards Military {} Military {} = True
    compareCards Commercial {} Commercial {} = True
    compareCards Guilds {} Guilds {} = True
    compareCards _ _ = False

    compareWondercards :: WonderCard -> WonderCard -> Bool --WonderCard osszehasonlitasa
    compareWondercards W {} W {} = True

    cardInCardsAmount :: Maybe Card -> [Card] -> Int --adott Card tipus db szama egy Card listaban
    cardInCardsAmount (Just card) (x:cards)
        | compareCards card x = 1 + cardInCardsAmount (Just card) cards
        | otherwise = cardInCardsAmount (Just card) cards
    cardInCardsAmount _ _ = 0

    isBuilt :: (WonderCard, IsBuilt) -> Bool --megepitett
    isBuilt (W {}, True) = True
    isBuilt _ = False

    getWonderCard :: (WonderCard, IsBuilt) -> WonderCard --visszaadja a WonderCard-ot a tuple-bol
    getWonderCard (wondercard, isBuilt) = wondercard

    wondercardInCardsAmount :: Maybe WonderCard -> [(WonderCard, IsBuilt)] -> Int --megepitett WonderCard tipus db szama egy WonderCard listaban
    wondercardInCardsAmount (Just card) (x:cards)
        | isBuilt x && compareWondercards card (getWonderCard x) = 1 + wondercardInCardsAmount (Just card) cards
        | otherwise = wondercardInCardsAmount (Just card) cards
    wondercardInCardsAmount _ _ = 0

    isGuild :: Card -> Bool --Guilds
    isGuild Guilds {} = True
    isGuild _ = False

    getBlankFromCard :: Card -> Card --adott Card visszaad u.o. tipusu blank Card-ot
    getBlankFromCard Materials {} = blankMaterialsCard
    getBlankFromCard Civilian {} = blankCivilianCard
    getBlankFromCard Scientific {} = blankScientificCard
    getBlankFromCard Military {} = blankMilitaryCard
    getBlankFromCard Commercial {} = blankCommercialCard
    getBlankFromCard _ = error "Unsupported card type"

    getBlankFromWondercard :: WonderCard -> WonderCard --adott WonderCard visszaad u.o. tipusu blank Card-ot
    getBlankFromWondercard W {} = blankWonderCard

    guildEffectCard :: Card -> Maybe Card --adott Guilds Card visszaad olyan tipusu blank Card-ot ami van az Effect parametereben
    guildEffectCard (Guilds _ _ (PointsByCard (Left card) _)) = Just (getBlankFromCard card)
    guildEffectCard _ = Nothing

    guildEffectWonderCard :: Card -> Maybe WonderCard --adott Guilds Card visszaad olyan tipusu blank WonderCard-ot ami van az Effect parametereben
    guildEffectWonderCard (Guilds _ _ (PointsByCard (Right wondercard) _)) = Just (getBlankFromWondercard wondercard)
    guildEffectWonderCard _ = Nothing

    guildEffectPoints :: Card -> Int --adott Guilds Card megadja a szamolando Points-ot
    guildEffectPoints (Guilds _ _ (PointsByCard _ points)) = points
    guildEffectPoints _ = error "Unsupported card type"

    whichPlayerHasMoreCard :: Maybe Card -> Player -> Player -> Int --melyik jateksonak van u.o. Card-bol tobb es mennyi
    whichPlayerHasMoreCard card (P _ cards1 _ _) (P _ cards2 _ _)
        | cardInCardsAmount card cards1 >= cardInCardsAmount card cards2 = cardInCardsAmount card cards1
        | otherwise = cardInCardsAmount card cards2

    whichPlayerHasMoreWonderCard :: Maybe WonderCard -> Player -> Player -> Int --melyik jateksonak van WonderCard-bol tobb es mennyi
    whichPlayerHasMoreWonderCard card (P _  _ cards1 _) (P _ _ cards2 _)
        | wondercardInCardsAmount card cards1 >= wondercardInCardsAmount card cards2 = wondercardInCardsAmount card cards1
        | otherwise = wondercardInCardsAmount card cards2

    isWonderGuild :: Card -> Bool --WonderCard-ot szamol-e a Guilds
    isWonderGuild (Guilds _ _ (PointsByCard (Right wondercard) _)) = True
    isWonderGuild _ = False

    wonderGuildPoints :: Player -> Player -> [Card] -> Int
    wonderGuildPoints (P _ cards1 wCards1 _) (P _ cards2 wCards2 _) (x:cards)
        | isGuild x && isWonderGuild x = whichPlayerHasMoreWonderCard (guildEffectWonderCard x) (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) * guildEffectPoints x + wonderGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) cards
        | otherwise = wonderGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) cards
    wonderGuildPoints _ _ _ = 0

    regGuildPoints :: Player -> Player -> [Card] -> Int
    regGuildPoints (P _ cards1 wCards1 _) (P _ cards2 wCards2 _) (x:cards)
        | isGuild x && not (isWonderGuild x) = whichPlayerHasMoreCard (guildEffectCard x) (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) * guildEffectPoints x + regGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) cards
        | otherwise = regGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) cards
    regGuildPoints _ _ _ = 0

    guildCardsList :: [Card] -> [Card]
    guildCardsList = filter isGuild

    --guildPoints :: Player -> Player -> Int
    --guildPoints (P _ (x:cards1) wCards1 _) (P _ cards2 wCards2 _)
    --    | isGuild x && isWonderGuild x = whichPlayerHasMoreWonderCard (guildEffectWonderCard x) (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) * guildEffectPoints x + guildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0)
    --    | isGuild x && not (isWonderGuild x) = whichPlayerHasMoreCard (guildEffectCard x) (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) * guildEffectPoints x + guildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0)
    --    | otherwise = guildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0)
    --guildPoints _ _ = 0

    guildPoints :: Player -> Player -> Int
    guildPoints (P _ cards1 wCards1 _) (P _ cards2 wCards2 _) =  regGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) (guildCardsList cards1) + wonderGuildPoints (P "" cards1 wCards1 0) (P "" cards2 wCards2 0) (guildCardsList cards1)

    -- Tudományos kártyák extra pontjai

    isScientific :: Card -> Bool
    isScientific Scientific {} = True
    isScientific _ = False

    compareSymbol :: Card -> Card -> Bool
    compareSymbol (Scientific _ _ _ Globe) (Scientific _ _ _ Globe) = True
    compareSymbol (Scientific _ _ _ Wheel) (Scientific _ _ _ Wheel) = True
    compareSymbol (Scientific _ _ _ Sundial) (Scientific _ _ _ Sundial) = True
    compareSymbol (Scientific _ _ _ Mortar) (Scientific _ _ _ Mortar) = True
    compareSymbol (Scientific _ _ _ Pendulum) (Scientific _ _ _ Pendulum) = True
    compareSymbol (Scientific _ _ _ Quill) (Scientific _ _ _ Quill) = True
    compareSymbol _ _ = False

    getScientificPoints :: Card -> Int
    getScientificPoints (Scientific _ _ point _) = point
    getScientificPoints _ = error "Unsupported card type"

    hasTwo :: Card -> [Card] -> Int
    hasTwo card (x:cards)
        | not (isScientific x) = hasTwo card cards
        | isScientific x && not (compareSymbol card x) = hasTwo card cards
        | otherwise = (getScientificPoints card + getScientificPoints x) * 2
    hasTwo _ _ = 0

    scientificPlusPoints :: Player -> Point
    scientificPlusPoints (P _ (x:cards) _ _) = hasTwo x cards + scientificPlusPoints (P "" cards [] 0)
    scientificPlusPoints _ = 0

    -- Tábla

    getCardLocation :: Card -> Table -> Maybe (Int, Int)
    getCardLocation card table = 
        case findIndex (elem (Just card)) table of
            Just rowIndex -> 
                case elemIndex (Just card) (table !! rowIndex) of
                    Just colIndex -> Just (rowIndex, colIndex)
                    Nothing -> Nothing
            Nothing -> Nothing

    getCardFromTable :: Table -> (Int, Int) -> Maybe Card
    getCardFromTable table (rowIndex, colIndex) = table !! rowIndex !! colIndex

    getX :: Maybe (Int, Int) -> Int
    getX (Just (x, _)) = x
    getX Nothing = error "Error"
    
    getY :: Maybe (Int, Int) -> Int
    getY (Just (_, y)) = y
    getY Nothing = error "Error"

    isInFirstLine :: Card -> Table -> Bool
    isInFirstLine card table = getX(getCardLocation card table) == 0

    isInTable :: Card -> Table -> Bool
    isInTable card table
        | getCardLocation card table == Nothing = False
        | otherwise = True

    isCardFree :: Card -> Table -> Bool
    isCardFree card [[]] = False
    isCardFree card table
        | not (isInTable card table) = False 
        | isInFirstLine card table = True
        | otherwise = getCardFromTable table (getX (getCardLocation card table) -1, getY (getCardLocation card table)) == Nothing && getCardFromTable table (getX (getCardLocation card table) -1, getY (getCardLocation card table) +1) == Nothing

    -- Kártya felvétele

    updateRow :: Card -> [Maybe Card] -> [Maybe Card]
    updateRow _ [] = []
    updateRow card (Nothing : cards) = Nothing : updateRow card cards
    updateRow card (Just x : cards)
        | x == card = Nothing : cards
        | otherwise = Just x : updateRow card cards

    cardToNothing :: Card -> Table -> Table
    cardToNothing card table
        | isCardFree card table = map (updateRow card) table
        | otherwise = table

    -- Megvásárolható-e

    getCardCost :: Card -> Cost
    getCardCost (Materials _ cost _) = cost
    getCardCost (Civilian _ cost _) = cost
    getCardCost (Scientific _ cost _ _) = cost
    getCardCost (Military _ cost _) = cost
    getCardCost (Commercial _ cost _ _) = cost
    getCardCost (Guilds _ cost _) = cost

    getWonderCardCost :: WonderCard -> Cost
    getWonderCardCost (W _ cost _ _ _) = cost

    getPlayerMoney :: Player -> Drachma
    getPlayerMoney (P _ _ _ money) = money

    canBuyCard :: Card -> Player -> Bool
    canBuyCard card (P _ cards _ money) = getPlayerMoney (P "" cards [] money) - costInMoney (getCardCost card) cards >= 0

    getWonderList :: Player -> [WonderCard]
    getWonderList (P _ _ (x:wonderTuples) _) = getWonderCard x : getWonderList (P "" [] wonderTuples 0)
    getWonderList _ = []

    playerHasWonder :: WonderCard -> Player -> Bool
    playerHasWonder card (P _ _ wonderTuples _) = card `elem` getWonderList (P "" [] wonderTuples 0)

    isBuiltByPlayer :: WonderCard -> Player -> Bool
    isBuiltByPlayer card (P _ _ (x:wonderTuples) _)
        | (card == getWonderCard x) && isBuilt x = True
        | otherwise = isBuiltByPlayer card (P "" [] wonderTuples 0)
    isBuiltByPlayer _ _ = False

    canBuyWonder :: WonderCard -> Player -> Bool
    canBuyWonder wonderCard (P _ cards wonderTuples money)
        | playerHasWonder wonderCard (P "" cards wonderTuples money) && isBuiltByPlayer wonderCard (P "" cards wonderTuples money) = False
        | not (playerHasWonder wonderCard (P "" cards wonderTuples money)) = False
        | otherwise = getPlayerMoney (P "" cards wonderTuples money) - costInMoney (getWonderCardCost wonderCard) cards >= 0