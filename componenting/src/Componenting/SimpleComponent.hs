module Componenting.SimpleComponent
  ( SimpleComponentStopped, SimpleComponent
  , simpleComponent, getComponent
  ) where

import Data.Row
import Data.Kind
import Componenting.Component

data SimpleComponentStopped
    stopped
    started
    (dependenciesConstraint :: Row Type -> Constraint)
  = SimpleComponentStopped {
      simpleComponentStopped :: stopped,
      simpleComponentStart :: forall r. (dependenciesConstraint r)
                          => (Rec r)
                          -> stopped
                          -> IO (SimpleComponent stopped started dependenciesConstraint)
    }

data SimpleComponent
    stopped
    started
    (dependenciesConstraint :: Row Type -> Constraint)
  = SimpleComponent {
    simpleComponentStarted :: started,
    simpleComponentStop :: started -> IO (SimpleComponentStopped stopped started dependenciesConstraint)
  }

instance
  (dependenciesConstraint dependencies) =>
  StartComponent
    (SimpleComponentStopped stopped started dependenciesConstraint)
    (SimpleComponent stopped started dependenciesConstraint)
    ()
    (Rec dependencies)
  where
    start dependencies (SimpleComponentStopped { simpleComponentStopped
                                               , simpleComponentStart })
      = simpleComponentStart dependencies simpleComponentStopped

instance StopComponent
    (SimpleComponentStopped stopped started dependenciesConstraint)
    (SimpleComponent stopped started dependenciesConstraint)
    ()
  where
    stopWithMeta _ SimpleComponent { simpleComponentStarted, simpleComponentStop }
      = simpleComponentStop simpleComponentStarted

simpleComponent :: forall dependenciesConstraint stopped started.
                   (forall r. (dependenciesConstraint r)
                    => (Rec r) -> stopped -> IO started)
                -> (started -> IO stopped)
                -> stopped
                -> SimpleComponentStopped stopped started dependenciesConstraint
simpleComponent doStart doStop initialStoppedValue =
    SimpleComponentStopped {
      simpleComponentStopped = initialStoppedValue,
      simpleComponentStart = startSimple
    }
  where
    startSimple :: forall r. (dependenciesConstraint r)
                   => (Rec r) -> stopped -> IO (SimpleComponent stopped started dependenciesConstraint)
    startSimple deps stopped = do
      started <- doStart deps stopped
      pure $ SimpleComponent {
        simpleComponentStarted = started,
        simpleComponentStop = stopSimple
      }
    stopSimple :: started -> IO (SimpleComponentStopped stopped started dependenciesConstraint)
    stopSimple started = do
      stopped <- doStop started
      pure $ SimpleComponentStopped {
        simpleComponentStopped = stopped,
        simpleComponentStart = startSimple
      }

getComponent :: SimpleComponent stopped started dependencies -> started
getComponent = simpleComponentStarted
