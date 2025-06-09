-- App3.hs

{-# LANGUAGE TupleSections #-}
module Main where

import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Heap (Entry(..), Heap)
import qualified Data.Heap as H
import Data.Ord (Down(..))
import Data.Bits (setBit, testBit)
import Data.Word (Word64)
import qualified Data.Map.Strict as M
import Data.List (reverse)

-- | Bosque y posiciones
type Bosque = [[Int]]
type Pos    = (Int, Int)
type Mask   = Word64

-- | Estado en el heap
data Estado = Estado
  { posAct  :: Pos
  , energia :: Int
  , mask    :: Mask
  , camino  :: [Pos]
  }

-- | Costos
costoDiagonal, costoTrampa :: Int
costoDiagonal = 2
costoTrampa   = 3

-- | Movimientos permitidos
movimientos :: [Pos]
movimientos = [(0,1),(1,0),(1,1),(0,-1),(-1,0)]

-- | Lee el valor de la runa (o Nothing si fuera de rango)
valorRuna :: Bosque -> Pos -> Maybe Int
valorRuna b (i,j)
  | i < 0 || j < 0 = Nothing
  | otherwise      = case drop i b of
      (fila:_) | j < length fila -> Just (fila !! j)
      _                           -> Nothing

-- | Comprueba rango en n×n
enRango :: Int -> Pos -> Bool
enRango n (i,j) = i>=0 && i<n && j>=0 && j<n

-- | Índice único para máscara (i*n + j)
idx :: Int -> Pos -> Mask
idx n (i,j) = fromIntegral (i*n + j)

-- | Expande un estado hacia sus vecinos válidos (sin volver a visitar)
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

-- | Best-first search con max-heap, poda de estados dominados
bestFirst :: Bosque -> Int -> Maybe ([Pos],Int)
bestFirst bosque e0 = do
  let n = length bosque
  v0 <- valorRuna bosque (0,0)
  let c0 = if v0==0 then costoTrampa else 0
      e1 = e0 - c0 + v0
  if e1 < 0 then Nothing else
    let startMask = setBit 0 0
        start = Estado (0,0) e1 startMask [(0,0)]
        heap0 = H.singleton (Entry (Down e1) start)
    in search n bosque heap0 M.empty

-- | Bucle principal de búsqueda
search
  :: Int                                      -- ^ tamaño n
  -> Bosque
  -> Heap (Entry (Down Int) Estado)           -- ^ heap ordenado por energía descendente
  -> M.Map (Pos,Mask) Int                     -- ^ mejor energía vista por (pos,mask)
  -> Maybe ([Pos],Int)
search n bosque heap bestMap =
  case H.uncons heap of
    Nothing -> Nothing
    Just (Entry (Down e) st, heap') ->
      let p = posAct st
          m = mask st
      in if p == (n-1,n-1)
           then Just (reverse (camino st), e)
           else case M.lookup (p,m) bestMap of
             Just eOld | e <= eOld -> search n bosque heap' bestMap
             _ ->
               let bestMap' = M.insert (p,m) e bestMap
                   hijos    = expandir bosque n st
                   heap''   = foldr (\st' h -> H.insert (Entry (Down (energia st')) st') h) heap' hijos
               in search n bosque heap'' bestMap'

-- | Formatea una posición para imprimir
mostrarPos :: Pos -> String
mostrarPos (i,j) = "(" ++ show i ++ "," ++ show j ++ ")"

-- | Main: parsea args y llama a bestFirst
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
