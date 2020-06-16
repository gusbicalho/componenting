module Componenting.Component where

class StartComponent
    stoppedComp
    startedComp
    meta
    dependencies
    | stoppedComp -> startedComp meta
  where
  startWithMeta :: dependencies -> stoppedComp -> IO (meta, startedComp)

  default startWithMeta :: (meta ~ ()) => dependencies -> stoppedComp -> IO (meta, startedComp)
  startWithMeta deps stoppedComp = ((), ) <$> start deps stoppedComp

  start :: dependencies -> stoppedComp -> IO startedComp
  start deps stoppedComp = snd <$> startWithMeta deps stoppedComp

class StopComponent
    stoppedComp
    startedComp
    meta
    | startedComp meta -> stoppedComp
  where
  stopWithMeta :: meta -> startedComp -> IO stoppedComp

stop :: StopComponent stoppedComp startedComp () => startedComp -> IO stoppedComp
stop startedComp = stopWithMeta () startedComp
