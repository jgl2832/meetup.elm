import Keyboard
import Window
import List
import Random

-- Inputs
type Input = { space:Bool, shoot:Bool, hero:Int, delta:Time, random:Float}

delta = inSeconds <~ fps 35

input = sampleOn delta ( Input <~ Keyboard.shift
                              ~ Keyboard.space
                              ~ lift .x Keyboard.wasd
                              ~ delta
                              ~ Random.float delta)


-- Game 
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)
(heroWidth,heroHeight) = (75,53)
(projWidth,projHeight) = (30,24)
(enemWidth,enemHeight) = (25,18)

type Object a = { a | x:Float, y:Float, vx:Float, vy:Float, w:Int, h:Int }

type Hero = Object { score:Int }
type Projectile = Object {}
type Enemy = Object {}

data State = Play | Pause

type Game = { state: State, hero:Hero, projectiles:[Projectile], enemies:[Enemy] }
player y = { x=0, y=y, vx=0, vy=0, score=0, w=heroWidth, h=heroHeight }
projectile x = {x=x, y=0-halfHeight+heroHeight, vx=0, vy=1, w=projWidth, h=projHeight }
enemy x = { x=x, y=halfHeight+50, vx=0,vy=1, w=enemWidth, h=enemHeight }

defaultGame =
  { state = Pause
  , gamehero = player (0-halfHeight+heroHeight/2)
  , projectiles = []
  , enemies = []
  }

-- Update
near n c m = m >= n-c && m <= n+c
within obj1 obj2 = (obj1.x |> near obj2.x (obj2.w/2)) && (obj1.y |> near obj2.y (obj2.w/2))

stepObj t ({x,y,vx,vy} as obj) =
  { obj | x <- x + vx * t
        , y <- y + vy * t }

growObj scalar t ({x,y,vx,vy,h,w} as obj) =
  { obj | w <- w + scalar * t
        , h <- h + (scalar*(h/w)) * t }

stepPlyr t dir score player = 
  let player' = stepObj t { player | vx <- toFloat dir * 200 }
  in { player' | x <- clamp (0-halfWidth+heroWidth/2) (halfWidth-heroHeight/2) player'.x 
               , score <- player.score + score }

stepProj t projectile =
  let projectile' = growObj -30 t (stepObj t { projectile | vy <- 800 })
  in { projectile' | x <- projectile'.x }

stepEnemy t enemy =
  let enemy' = growObj 10 t (stepObj t { enemy | vy <- -100 })
  in { enemy' | x <- enemy'.x }

enemyShotFilter enemy projectiles =
  List.any (\t -> within t enemy) projectiles

randomEnemyStart random =
  (random * (gameWidth - 60)) - (halfWidth - 30)

stepGame {space, shoot, hero, delta, random}
         ({state,gamehero, projectiles, enemies} as game) =
  let state' = if | space -> Play
                  | otherwise -> state


      onscreenProjectiles = List.filter (\t -> t.y < halfHeight) projectiles
      projectiles' = List.map (\t -> (stepProj delta t)) onscreenProjectiles ++ 
                     if | shoot && length onscreenProjectiles < 1 -> [projectile(gamehero.x)]
                        | otherwise -> []


      shotEnemies = List.filter (\t -> enemyShotFilter t projectiles') enemies
      unshotEnemies = List.filter (\t -> not (enemyShotFilter t projectiles')) enemies
      score' = length shotEnemies

      onscreenEnemies = List.filter (\t -> t.y > 0-halfHeight-enemHeight) unshotEnemies
      enemies' = List.map (\t -> (stepEnemy delta t)) onscreenEnemies ++
                     if | state == Play && length onscreenEnemies < 2 -> [enemy(randomEnemyStart random)]
                        | otherwise -> []

      gamehero' = stepPlyr delta hero score' gamehero
  in
      { game | state <- state'
             , gamehero <- gamehero'
             , projectiles <- projectiles'
             , enemies <- enemies' }

gameState = foldp stepGame defaultGame input


pongGreen = rgb 0 0 0
textGreen = rgb 224 57 62
txt f = leftAligned . f . monospace . Text.color textGreen . toText
msg = "SHIFT to start, SPACE to shoot, ad to move"

displayHero hero =
    displayObj hero (toForm (fittedImage hero.w hero.h "meetup_clear.png"))

displayProj proj =
    displayObj proj (toForm (fittedImage proj.w proj.h "meetup_clear.png"))

displayEnemy enemy =
    displayObj enemy (toForm (fittedImage enemy.w enemy.h "meetup_clear.png"))

displayObj obj shape = 
    move (obj.x,obj.y) shape

display (w,h) {state,gamehero,projectiles,enemies} =
  let scores : Element
      scores = txt (Text.height 20) <|
               show gamehero.score ++ " rsvps" 
  in
      container w h middle <|
      collage gameWidth gameHeight
      ([ filled pongGreen    (rect gameWidth gameHeight)
      , if state == Play then (displayHero gamehero) else toForm(spacer 1 1)
      , toForm scores
          |> move (0, gameHeight/2 - 40)
      , toForm (if gamehero.score < 4 then spacer 1 1 else txt (Text.height 20) <| show "Lookin SMUG!")
      , toForm (if state == Play then spacer 1 1 else txt id msg)
          |> move (0, 75 - gameHeight/2)
      , toForm (if state == Play then spacer 1 1 else (fittedImage 400 259 "Meetup-The-Game.gif"))
          |> move (10, 40)
      ] ++ (map (\t -> displayProj t) projectiles) ++ (map (\t -> displayEnemy t) enemies) )

main = lift2 display Window.dimensions gameState
--main = lift asText gameState
