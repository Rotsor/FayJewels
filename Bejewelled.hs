{-# LANGUAGE NoImplicitPrelude #-}

module Bejewelled where

import Prelude
import FFI
import CodeWorld

main = do
  img1 <- mkImage $ "Backgroundl.jpg"
  playInCanvas initial step event (draw img1)

data GemColor = Red | Green | Blue | Yellow | Magenta deriving Show
eqGemColor Red Red = True
eqGemColor Green Green = True
eqGemColor Blue Blue = True
eqGemColor Yellow Yellow = True
eqGemColor Magenta Magenta = True
eqGemColor _ _ = False

allColors = [Red, Green, Blue, Yellow, Magenta]

data TTL = TTL Double
data ColumnStuff = Debris Double TTL GemColor | Freefall Double GemColor

type ColumnState = ([GemColor], [ColumnStuff])
type Score = Int

data RunningGame = RunningGame {
    timeLeft :: Double,
    score :: Score,
    columns :: [ColumnState]
  }

data GameState = GameRunning RunningGame | Lobby (Maybe Score)

data State = State {
    randomGen :: StdGen,
    bestScore :: Score,
    mousePos :: Point,
    gameState :: GameState
  }

columnN = 8
rowN = 8

genListElement :: [a] -> StdGen -> (a, StdGen)
genListElement list g = case randomR (0, fromIntegral $ length list) g of
  (x, g') -> (look list x, g')
 where
    look [x] _ = x
    look (x : xs) d
      | d < 1 = x
      | otherwise = look xs (d-1)

traverseMR :: (a -> RG b) -> [a] -> RG [b]
traverseMR f [] = returnR []
traverseMR f (h : t) = apR (fmapR (:) (f h)) (traverseMR f t)

isFreefall (Freefall _ _) = True
isFreefall _ = False

topUp1 (stable, flying) = flip fmapR (replicateMR needMore genColor) $ 
  \colors -> (stable, flying ++ zipWith (\i c -> Freefall (top + fromIntegral i * gemPeriod) c) [1..] colors) where
 needMore = rowN - length stable - length (filter isFreefall flying)
 top = case reverse flying of
   [] -> rowHeight (length stable - 1)
   (Freefall h _ : _) -> h
   (Debris h _ _ : _) -> h

topUp :: [ColumnState] -> RG [ColumnState]
topUp = traverseMR topUp1

genColor = genListElement allColors

returnR x g = (x, g)

apR f x g = case f g of
  (f', g') -> fmapR f' x g'
bindR x f g = case x g of
  (x', g') -> f x' g'

fmapR f action g = case action g of
  (res, g') -> (f res, g')

type RG a = StdGen -> (a, StdGen)

replicateMR :: Int -> RG a -> RG [a]
replicateMR 0 action = returnR []
replicateMR n action = bindR action (\x -> fmapR (x:) (replicateMR (n-1) action))

genColumn = replicateMR rowN genColor

genColumns :: RG [[GemColor]]
genColumns = replicateMR columnN genColumn

initGame = fmapR (\cols -> RunningGame {
    score = 0
  , columns = (map (\c -> ([], zipWith (\r c -> Freefall (rowHeight r) c) [rowN..] c)) cols)
  , timeLeft = totalGameTime
  }) genColumns

initial g =  State
   { randomGen = g
   , mousePos = (0, 0)
   , gameState = Lobby Nothing
   , bestScore = 0
   }

flyingSpeed = gemPeriod * 10


nagon n = polygon $ [(gemSize * 0.5 * sin a, gemSize * 0.5 * cos a)|i<-[0..n-1], let a = 2*pi*fromIntegral i/fromIntegral n]

drawGem Red = Color (RGBA 1 0.1 0.1 1) $ nagon 6
drawGem Blue = Color (RGBA 0.2 0.2 1 1) $ rectangleSolid (gemSize * 0.85) (gemSize * 0.85)
drawGem Green = Color green $ translate 0 (gemSize * (-0.1)) $ nagon 3
drawGem Yellow = Color yellow $ circleSolid (gemSize / 2 * 0.9)
drawGem Magenta = Color magenta $ polygon [(gemSize*0.5, 0), (0, gemSize*0.5), (-gemSize*0.5, 0), (0, -gemSize*0.5)]

rowHeight row = fromIntegral row * gemPeriod

stepCol :: Double -> ColumnState -> ColumnState
stepCol dt (standing, flying) = (standing', flying'') where
  flying' = map proceed flying where
    proceed (Freefall h c) = Freefall (h - dt*flyingSpeed) c
    proceed (Debris h (TTL x) c) = Debris h (TTL $ x - dt) c

  tmp = go (length standing) flying'
  landed = fst tmp -- WANT LAZY PATTERNS!

  standing' = standing ++ landed

  flying'' = filter (not . dead) . blockMovements (rowHeight (length standing')) $ snd tmp where

    dead (Debris _ (TTL x) _) | x <= 0 = True
    dead _ = False

    blockMovements _ [] = []
    blockMovements minH (Freefall h c : rest) = Freefall h' c : blockMovements (h' + gemPeriod) rest where
      h' = mxx minH h
      mxx a b | a > b = a | otherwise = b
    blockMovements _ (d@(Debris h _ _) : rest) = d : blockMovements (h + gemPeriod) rest


  go n [] = ([], [])
  go n allF@((Freefall h c) : f)
    | h < (rowHeight n) = case go (n+1) f of
      (landed, remaining) -> (c : landed, remaining)
  go n f = ([], f)

zipMay :: [a] -> [b] -> [(Maybe a, Maybe b)]
zipMay as bs = takeWhile (\(a,b) -> isJust a || isJust b) $ zip (extend as) (extend bs) where
  extend = (++repeat Nothing) . map Just

isJust Nothing = False
isJust _ = True

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (r : rs) = zipWith (\x l -> case x of Nothing -> l; Just v -> v : l) (map Just r ++ repeat Nothing) (transpose rs)

colDestruction :: [ColumnState] -> (Int, [ColumnState])
colDestruction cols = case unzip cols of
  (stable, flying) -> case destruction stable of
    (cnt, destroyed) -> (cnt, zipWith f destroyed flying)
 where
  f remains flying = case go remains of
    (stable, detached) -> (stable, detached ++ flying)
   where
     go l = case span snd l of
       (stable, detaching) -> (map fst stable, zipWith g [length stable..] detaching)
      where
       g j (color, False) = Debris (rowHeight j) maxTTL color
       g j (color, True) = Freefall (rowHeight j) color

fromJust (Just x) = x

destructionPossible :: [[GemColor]] -> Bool
destructionPossible x = fst (destruction x) > 0

destruction :: [[GemColor]] -> (Int, [[(GemColor, Bool)]])
destruction original = (count, destroyed) where
  transposed = take rowN $ transpose $ map ((++ repeat Nothing) . map Just) original
  lineDestroy [] = []
  lineDestroy (Nothing : l) = (undefined, False) : lineDestroy l
  lineDestroy (Just x : l) = case span (\v -> case v of Just y -> eqGemColor x y; _ -> False) l of
    (eq, neq)
      | length eq >= 2 -> map (\x -> (fromJust x, False)) (Just x : eq) ++ lineDestroy neq
      | otherwise -> map (\x -> (fromJust x, True)) (Just x : eq) ++ lineDestroy neq
  count = sum $ map (length . filter (not . snd)) destroyed
  destroyed = zipWith (\r1 r2 -> zipWith demandBoth r1 r2) vertical horisontal where
    demandBoth (x, alive1) (y, alive2) = (x, alive1 && alive2)
    vertical = map (lineDestroy . map Just) original
    horisontal = transpose $ map lineDestroy transposed

maxx a b | a > b = a | otherwise = b

step dt w = case gameState w of
  Lobby (Just score) -> w { bestScore = maxx score (bestScore w) }
  Lobby Nothing -> w
  GameRunning game
    | timeLeft game < 0 -> w { gameState = Lobby (Just $ score game) }
    | otherwise -> stepG dt w game

stepG dt w game = case topUp col' (randomGen w) of
  (topped, gen') ->
    w {
     gameState = GameRunning $ RunningGame { timeLeft = (timeLeft game) - dt, score = (score game) + scoreAdd, columns = topped },
     randomGen = gen'
     } 
 where
  destruction = colDestruction $ map (stepCol dt) (columns game)
  scoreAdd = fst destruction
  col' = snd destruction

gemSize = 38
gemSpace = 8
gemPeriod = gemSize + gemSpace
maxTTL = TTL 0.5

divTTL (TTL a) (TTL b) = a / b

drawNode (Freefall h color) = translate 0 h $ drawGem color
drawNode (Debris h ttl color) = translate 0 h $ scale s s $ drawGem color where
  s = divTTL ttl maxTTL

drawColumn (standing, flying) = pictures $ map drawNode (takeWhile ((< rowHeight rowN) . getH) $ zipWith (\i color -> Freefall (i*gemPeriod) color) [0..] standing ++ flying) where
  getH (Freefall h _) = h
  getH (Debris h _ _) = h

nodeCoord (i, j) = (rowHeight i, rowHeight j)

arrowPoly = polygon fullArrow where
  quarterArrow = [(0, 0.5*w1), (l1-l3, 0.5*w1), (l1-l2, w2*0.5), (l1, 0)]
  halfArrow = quarterArrow ++ (map (\(x, y) -> (x, -y)) $ reverse $ tail $ init $ quarterArrow)
  fullArrow = halfArrow ++ map (\(x, y) -> (-x, -y)) halfArrow
  s = gemPeriod
  w1 = s * 0.07
  w2 = s * 0.2
  l1 = w2
  l2 = l1 * 0.66
  l3 = l2 * 0.66

{- case makeMove (map fst $ columns w) move of
   Nothing -> red
   Just _ -> green -}

drawHint w = case pointToMove (mousePos w) of
  Nothing -> pictures []
  Just move -> color white $ uncurry translate (mulSV 0.5 $ uncurry addV ((\(a, b) -> (nodeCoord a, nodeCoord b)) move)) $ (if fst (fst move) /= fst (snd move) then id else rotate 90) arrowPoly 

mulSV s (x, y) = (s*x, s*y)

gridColor = gray 0.15

drawGrid = color gridColor $ scale gemPeriod gemPeriod $ pictures $ map (\i -> line [(t 0, t i), (t columnN, t i)]) [0..rowN] ++ map (\i -> line [(t i, t 0), (t i, t rowN)]) [0..columnN] where
  t i = fromIntegral i - 0.5

totalGameTime = 60

lerpColor p (RGBA r1 g1 b1 a1) (RGBA r2 g2 b2 a2) = RGBA (lerp p r1 r2) (lerp p g1 g2) (lerp p b1 b2) (lerp p a1 a2) where
  lerp p a b = a + (b - a) * p

timerColor timeLeft
  | timeLeft > (grayCutoff * totalGameTime) = baseColor
  | timeLeft < (redCutoff * totalGameTime) = alarmColor
  | otherwise = lerpColor ((timeLeft - 0.1 * totalGameTime) / ((grayCutoff-redCutoff) * totalGameTime)) alarmColor baseColor
 where 
   grayCutoff = 0.3
   redCutoff = 0.1
   baseColor = (gray 0.9)
   alarmColor = red

drawTimer timeLeft = translate ((-250-fromIntegral columnN*0.5*gemPeriod)*0.5) 0 $ pictures
 [ Color (gray 0.2) (rectangleSolid timerWidth timerHeight)
 , Translate 0 (-(distanceSpent/2)) $ Color (timerColor timeLeft) (rectangleSolid 9 distanceLeft)
 , frame timerWidth timerHeight
 ] where
 distanceLeft = timerHeight * timeLeft / totalGameTime
 distanceSpent = timerHeight - distanceLeft
 timerHeight = fromIntegral rowN * gemPeriod
 timerWidth = 15

frame x y = color (gray 0.2) $ rectangleWire (x + 0.15 * gemPeriod) (y + 0.15 * gemPeriod)

draw img1 w =
 pictures $ (Color black (rectangleSolid 500 500) :) $
   case gameState w of
     Lobby Nothing -> [Color white $ translate (-100) 0 $ scale 0.3 0.3 $ text "Click to start a game"]
     Lobby (Just score) -> 
       [ Color white $ translate (-100) 0 $ scale 0.3 0.3 $ pictures
         [ text $ "Your score is " ++ show score
         , translate 0 200 $ text $ "Best score is " ++ show (bestScore w)]]
     GameRunning game -> 
        [ uncurry translate globalShift $ pictures [
            drawGrid
          , pictures (zipWith (\i col -> translate (i * gemPeriod) 0 $ drawColumn col) [0..] (columns game))
          , drawHint w
          ]
          , drawTimer (timeLeft game)
          , translate (75) 210  $ Color white $ scale 0.2 0.2 $ Text $ "Score: " ++ show (score game)
          , frame (rowHeight columnN) (rowHeight rowN)]

addV (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pointToCellPair (x, y) = (p, addV p (deltas 0)) where
  p = (xi, yi)
  xi = round (x / gemPeriod)
  yi = round (y / gemPeriod)
  xd = x - fromIntegral xi * gemPeriod
  yd = y - fromIntegral yi * gemPeriod
  deltas n
    | xd > abs yd = (1, 0)
    | xd < (- abs yd) = (-1, 0)
    | yd > 0 = (0, 1)
    | otherwise = (0, -1)

pointToMove p = case pointToCellPair p of
  cp
    | validMove cp -> Just cp
    | otherwise -> Nothing

validCell (x, y) = x >= 0 && x < columnN && y >= 0 && y < rowN

validMove (a, b) = validCell a && validCell b

type Cell = (Int, Int)
type Move = (Cell, Cell)

swap field (a, b) = case (look a, look b) of
  (Just ca, Just cb) -> Just (write a cb $ write b ca $ field)
  _ -> Nothing
 where
  index _ [] = Nothing
  index i (h : _) | i==0 = Just h
  index i (_ : t) = index (i-1) t

  look (x, y) = case index x field of
    (Just col) -> index y col
    Nothing -> Nothing

  modify 0 f (x : xs) = f x : xs
  modify n f (x : xs) = x : modify (n-1) f xs

  write (x, y) c arr = modify x (modify y (const c)) arr

makeMove :: [[GemColor]] -> Move -> Maybe [[GemColor]]
makeMove colors move
  | destructionPossible colors = Nothing
  | otherwise = case swap colors move of
     Nothing -> Nothing
     Just colors -> if destructionPossible colors then Just colors else Nothing

globalShift = (- fromIntegral (columnN - 1) * 0.5 * gemPeriod, - fromIntegral (rowN - 1) * 0.5 * gemPeriod)

correctMousePos = addV (mulSV (-1) globalShift)

tryMakeMove pt w = case pointToMove pt of
  Nothing -> w
  Just move -> case unzip (columns w) of 
    (standing, flying) -> case makeMove standing move of
      Nothing -> w
      Just standing' -> w {columns = zip standing' flying}


newGame w = case initGame (randomGen w) of
  (game, gen) -> w { gameState = GameRunning game, randomGen = gen }

event (MouseMoveEvent pt) w = w { mousePos = correctMousePos pt }
event (MousePressEvent _ pt) w = case gameState w of
  Lobby _ -> newGame w
  GameRunning game -> w { gameState = GameRunning (tryMakeMove (correctMousePos pt) game) }
event _                         w = w
