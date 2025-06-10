-- App3.hs (Versión final con Ord derivado para Estado)

{-# LANGUAGE TupleSections #-}
module Main where

import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Data.List as L
import Data.Ord (Down(..))
import Data.Bits (setBit, testBit)
import Data.Word (Word64)
import qualified Data.Map.Strict as M
import Data.List (reverse)

-- TIPOS DE DATOS
type Bosque = [[Int]]
type Pos    = (Int, Int)
type Mask   = Word64

type PriorityQueue = [(Down Int, Estado)]

data Estado = Estado
  { posAct  :: Pos
  , energia :: Int
  , mask    :: Mask
  , camino  :: [Pos]
  } deriving (Eq, Ord) -- <<<==== ESTA ES LA LÍNEA AÑADIDA

-- CONSTANTES
costoDiagonal, costoTrampa :: Int
costoDiagonal = 2
costoTrampa   = 3

movimientos :: [Pos]
movimientos = [(0,1),(1,0),(1,1),(-1,0),(0,-1)]

-- FUNCIONES AUXILIARES
valorRuna :: Bosque -> Pos -> Maybe Int
valorRuna b (i,j)
  | i < 0 || j < 0 = Nothing
  | otherwise      = case drop i b of
      (fila:_) | j < length fila -> Just (fila !! j)
      _                           -> Nothing

enRango :: Int -> Pos -> Bool
enRango n (i,j) = i>=0 && i<n && j>=0 && j<n

idx :: Int -> Pos -> Int
idx n (i,j) = i*n + j

expandir :: Bosque -> Int -> Estado -> [Estado]
expandir bosque n st = mapMaybe paso movimientos
  where
    (i,j)    = posAct st
    eAct     = energia st
    visMask  = mask st
    path     = camino st
    paso (di,dj) =
      let np = (i+di, j+dj)
          bit = idx n np
      in if not (enRango n np) || testBit visMask bit
         then Nothing
         else case valorRuna bosque np of
           Nothing -> Nothing
           Just v  ->
             let cd   = if di==1 && dj==1 then costoDiagonal else 0
                 ct   = if v==0            then costoTrampa   else 0
                 eNew = eAct - cd - ct + v
             in if eNew < 0 then Nothing
                else Just $ Estado
                     { posAct  = np
                     , energia = eNew
                     , mask    = setBit visMask bit
                     , camino  = np : path
                     }

heuristic :: Int -> Int -> Pos -> Int
heuristic n maxRuna (i,j) = ((n-1 - i) + (n-1 - j)) * maxRuna

maxRunaValue :: Bosque -> Int
maxRunaValue b
  | null b || null (head b) = 0
  | otherwise = max 0 (maximum (map maximum b))

-- ALGORITMO A* CON LISTA ORDENADA
bestFirst :: Bosque -> Int -> Maybe ([Pos],Int)
bestFirst bosque e0 = do
  let n = length bosque
  if n == 0 || null (head bosque) then Nothing else do
    let maxRuna = maxRunaValue bosque
        h       = heuristic n maxRuna
    v0 <- valorRuna bosque (0,0)
    let c0 = if v0==0 then costoTrampa else 0
        e1 = e0 - c0 + v0
    if e1 < 0 then Nothing else
      let startMask = setBit 0 0
          start = Estado (0,0) e1 startMask [(0,0)]
          priority = e1 + h (0,0)
          pq0 = [(Down priority, start)]
      in search n bosque h pq0 M.empty

search
  :: Int
  -> Bosque
  -> (Pos -> Int)
  -> PriorityQueue
  -> M.Map (Pos,Mask) Int
  -> Maybe ([Pos],Int)
search n bosque h pq bestMap =
  case pq of
    [] -> Nothing
    ((_, st):pq') ->
      let p = posAct st
          m = mask st
          e = energia st
      in if p == (n-1,n-1)
           then Just (reverse (camino st), e)
           else case M.lookup (p,m) bestMap of
             Just eOld | e <= eOld -> search n bosque h pq' bestMap
             _ ->
               let bestMap' = M.insert (p,m) e bestMap
                   hijos    = expandir bosque n st
                   nuevosItems = map (\s -> (Down (energia s + h (posAct s)), s)) hijos
                   pq'' = L.sort (nuevosItems ++ pq')
               in search n bosque h pq'' bestMap'

-- MAIN
mostrarPos :: Pos -> String
mostrarPos (i,j) = "(" ++ show i ++ "," ++ show j ++ ")"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sB,sE] ->
      case (readMaybe sB :: Maybe Bosque, readMaybe sE :: Maybe Int) of
        (Just b, Just e) ->
          case bestFirst b e of
            Nothing -> putStrLn "No existe un camino válido que deje energía ≥ 0 hasta el final."
            Just (cam,ef) -> do
              putStrLn "Camino óptimo (coordenadas):"
              mapM_ (putStrLn . mostrarPos) cam
              putStrLn $ "Energía final máxima: " ++ show ef
        _ -> putStrLn "Error: no se pudo parsear la matriz o la energía."
    _ -> putStrLn "Uso: App3 \"[[...]]\" <energiaInicial>"