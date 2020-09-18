module Test.Main where

import Prelude

import Control.Monad.Error.Class (throwError)
import Effect (Effect)
import Effect.Aff (Aff, apathize, forkAff, joinFiber, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Cache (newCache, runCached, runCached')
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "AffCache" cacheSpec

cacheSpec :: Spec Unit
cacheSpec = do
  it "should run initial request" do
    c <- newCache
    (runCached c "foo" (pure "FOO")) `shouldReturn` "FOO"

  it "should run requests of different keys" do
    c <- newCache
    (runCached c "foo" (pure "FOO")) `shouldReturn` "FOO"
    (runCached c "bar" (pure "BAR")) `shouldReturn` "BAR"

  it "should run uncached results eagerly" do
    latch <- AVar.empty
    c <- newCache
    _ <- liftEffect $ runCached' c "foo" (AVar.put unit latch)
    AVar.take latch

  it "should cache successful results" do
    avar <- AVar.new "FOO"
    c <- newCache
    (runCached c "foo" (AVar.take avar)) `shouldReturn` "FOO"
    (runCached c "foo" (AVar.take avar)) `shouldReturn` "FOO"

  it "should share results in flight" do
    avar <- AVar.empty
    c <- newCache
    fiber1 <- forkAff $ runCached c "foo" (AVar.take avar)
    fiber2 <- forkAff $ runCached c "foo" (AVar.take avar)
    AVar.put "FOO" avar
    joinFiber fiber1 `shouldReturn` "FOO"
    joinFiber fiber2 `shouldReturn` "FOO"

  it "should retry on succeeding attempt" do
    c <- newCache
    apathize $ runCached c "foo" err
    runCached c "foo" (pure "FOO") `shouldReturn` "FOO"

  it "should retry on failed in flights" do
    latch <- AVar.empty

    c <- newCache

    fiber1 <- forkAff do
      runCached c "foo" do
        AVar.put unit latch
        err

    AVar.take latch
    runCached c "foo" (pure "FOO") `shouldReturn` "FOO"

err :: forall a. Aff a
err = throwError $ error "error"
