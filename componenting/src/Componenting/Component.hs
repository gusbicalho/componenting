module Componenting.Component where

class StartComponent
    stoppedComp
    startedComp
    dependencies
    | stoppedComp -> startedComp
  where
  start :: dependencies -> stoppedComp -> IO startedComp

class StopComponent
    stoppedComp
    startedComp
    | startedComp -> stoppedComp
  where
  stop :: startedComp -> IO stoppedComp
