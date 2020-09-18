module Effect.Aff.Cache
       ( Cache
       , newCache
       , runCached
       ) where

import Prelude

import Control.Alt ((<|>))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Fiber, forkAff, generalBracket, joinFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar

newtype Cache k a = Cache (AVar (Map k (Entry a)))

data Entry a =
  InFlight (Fiber a)
  | Success a

newCache :: forall k a. Aff (Cache k a)
newCache =  Cache <$> AVar.new Map.empty

runCached ::
  forall k a.
  Ord k =>
  Cache k a ->
  k ->
  Aff a ->
  Aff a
runCached c@(Cache mapVar) k aff = do
  map <- AVar.take mapVar
  case Map.lookup k map of
    Nothing -> do
      fiber <- forkAff (execAff c k aff)
      let map' = Map.insert k (InFlight fiber) map
      -- can this be interrupted?
      AVar.put map' mapVar
      joinFiber fiber
    Just e -> do
      AVar.put map mapVar
      case e of
        InFlight fiber ->
          joinFiber fiber <|> runCached c k aff
        Success a ->
          pure a

execAff ::
  forall k a.
  Ord k =>
  Cache k a ->
  k ->
  Aff a ->
  Aff a
execAff (Cache mapVar) k aff =
  generalBracket (pure unit)
    { killed: \_ _ -> removeEntry
    , failed: \_ _ -> removeEntry
    , completed: \b _ -> putSuccess b
    } (const aff)

  where
    putSuccess b = do
      map <- AVar.take mapVar
      AVar.put (Map.insert k (Success b) map) mapVar

    removeEntry = do
      map <- AVar.take mapVar
      AVar.put (Map.delete k map) mapVar
