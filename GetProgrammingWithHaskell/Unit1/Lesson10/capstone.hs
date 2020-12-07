cup flOz = \message -> message flOz

coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = cup (flOz - ozDrank)
    where flOz = getOz aCup

afterASip = drink coffeeCup 1

drinkV2 aCup ozDrank =
    if ozDiff >= 0
        then cup ozDiff
        else cup 0
    where
        flOz = getOz aCup
        ozDiff = flOz - ozDrank

afterBigGulp = drinkV2 coffeeCup 20

isEmpty aCup = aCup (\x -> x == 0)
isEmptyV2 aCup = getOz aCup == 0

afterManySips = foldl drink coffeeCup [1,1,1,1,1,1]

robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot("Kill3r", 25, 200)

name (n,_,_) = n
attack (_,a,_) = a
hp (_,_,h) = h

getName aRobot = aRobot name
setName aRobot newName = aRobot (\(n,a,h) -> robot (newName, a, h))

getAttack aRobot = aRobot attack
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot(n,newAttack, h))

getHP aRobot = aRobot hp
setHP aRobot newHP = aRobot (\(n,a,h) -> robot(n, a, newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack: " ++ (show a) ++ " hp: " ++ (show h))

damage aRobot attackDamage = aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

afterHit = damage killerRobot 90

fight aRobot bRobot = damage bRobot attack
    where attack = if getHP aRobot > 10
            then getAttack aRobot
            else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiantRound1 killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound2 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound3 killerRobotRound2

fastRobot = robot ("speedy", 15, 40)
slowRobot = robot ("slowpoke",20,30)

slowRobotRound1 = fight fastRobot slowRobot
fastRobotRound1 = fight slowRobotRound1 fastRobot
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
-- fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- slowRobotRound1 = fight fastRobot slowRobot

-- Q10.1
robotList = [robot("Kill3r", 25, 200), robot ("Mr. Friendly", 10, 300), robot ("speedy", 15, 40), robot ("slowpoke",20,30)]
robotHPs = map getHP robotList

-- Q10.2
threeRoundFight aRobot bRobot = 
    (\(aRobot, bRobot) -> 
--         (\(a, b) -> 
--             (\(a, b) -> 
--                 (\(a, b) ->
--                     (\(a, b) ->
--                         (\(a, b) -> (a, b))
--                         (fight b a, b))
--                     (a, fight a b))
--                 (fight b a, b))
--             (a, fight a, b)
--         (fight b a, b)) 
    (aRobot, (fight aRobot bRobot)))(aRobot, bRobot)

    --Q10.3
    challenger = robot ("Challenger", 34, 222);

    round1Robots = map fight challenger robotList