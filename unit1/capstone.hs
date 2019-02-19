--coffee--
cup flOz = \message -> message flOz

getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank =
  if ozDiff >= 0
    then cup ozDiff
    else cup 0
  where
    flOz = getOz aCup
    ozDiff = flOz - ozDrank

isEmpty aCup = getOz aCup == 0

coffeeCup = cup 12

coffeeCupDrained = foldl drink coffeeCup [1, 1, 3, 1, 2]

--robot--
robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n

attack (_, a, _) = a

hp (_, _, hp) = hp

getName aRobot = aRobot name

getAttack aRobot = aRobot attack

getHP aRobot = aRobot hp

setName aRobot new = aRobot (\(n, a, h) -> robot (new, a, h))

setAttack aRobot new = aRobot (\(n, a, h) -> robot (n, new, h))

setHP aRobot new = aRobot (\(n, a, h) -> robot (n, a, new))

printRobot aRobot =
  aRobot (\(n, a, h) -> n ++ " " ++ (show a) ++ " " ++ (show h))

damage aRobot attackDamage =
  aRobot (\(n, a, h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
  where
    attack =
      if getHP aRobot > 10
        then getAttack aRobot
        else 0

killerRobot = robot ("Killer", 25, 200)

defenderRobot = robot ("Defender", 15, 400)

centristRobot = robot ("Centrist", 20, 300)

robotList = [killerRobot, defenderRobot, centristRobot]

printRobots robotList = map printRobot robotList

partyFight attacker defenders = map doFight defenders
  where
    doFight = fight attacker
    --TODO: threeRoundFight
