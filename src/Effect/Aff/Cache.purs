module Effect.Aff.Cache
       ( Cache
       , newCache
       , newCache'
       , runCached
       , runCached'
       ) where

import Prelude

import Control.Alt ((<|>))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Fiber, generalBracket, joinFiber, launchAff, launchSuspendedAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newtype Cache k a = Cache (Ref (Map k (Entry a)))

data Entry a =
  InFlight (Fiber a)
  | Success a

newCache :: forall k a. Aff (Cache k a)
newCache =  liftEffect newCache'

newCache' :: forall k a. Effect (Cache k a)
newCache' =  Cache <$> Ref.new Map.empty

runCached ::
  forall k a.
  Ord k =>
  Cache k a ->
  k ->
  Aff a ->
  Aff a
runCached c k aff = do
  joinFiber =<< liftEffect (runCached' c k aff)

runCached' ::
  forall k a.
  Ord k =>
  Cache k a ->
  k ->
  Aff a ->
  Effect (Fiber a)
runCached' c@(Cache mapRef) k aff = do
  fiber <- launchSuspendedAff (execAff c k aff)
  prev <- Ref.modify' (modifyMap fiber) mapRef
  case prev of
    Nothing ->
      launchAff (joinFiber fiber)
    Just (Success a) ->
      pure (pure a)
    Just (InFlight a) ->
      launchSuspendedAff (joinFiber a <|> runCached c k aff)

  where
    modifyMap fiber map = case Map.lookup k map of
      Nothing ->
        { state: Map.insert k (InFlight fiber) map, value: Nothing }
      value ->
        { state: map, value }

execAff ::
  forall k a.
  Ord k =>
  Cache k a ->
  k ->
  Aff a ->
  Aff a
execAff (Cache mapRef) k aff =
  generalBracket (pure unit)
    { killed: \_ _ -> removeEntry
    , failed: \_ _ -> removeEntry
    , completed: \b _ -> putSuccess b
    } (const aff)

  where
    putSuccess b =
      liftEffect $ Ref.modify_ (Map.insert k (Success b)) mapRef

    removeEntry =
      liftEffect $ Ref.modify_ (Map.delete k) mapRef
