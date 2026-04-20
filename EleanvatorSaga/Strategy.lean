import EleanvatorSaga.Types

/-! # Player-facing API for controlling elevators -/

namespace EleanvatorSaga

/-- Commands the player's strategy can issue. -/
inductive Command where
  | goToFloor (elevatorIdx : ElevatorId) (floor : FloorNum) (forceNow : Bool := false)
  | stop (elevatorIdx : ElevatorId)
  | setGoingUpIndicator (elevatorIdx : ElevatorId) (value : Bool)
  | setGoingDownIndicator (elevatorIdx : ElevatorId) (value : Bool)
  deriving Repr, BEq

/-- Monad for writing strategy handlers in imperative style. -/
abbrev CommandM := StateM (List Command)

/-- Run a `CommandM` block and collect the commands. -/
def cmds (m : CommandM Unit) : List Command :=
  (m.run []).2

/-- Read-only view of an elevator's state. -/
structure ElevatorView where
  index : ElevatorId
  currentFloor : FloorNum
  destinationQueue : List FloorNum
  pressedFloors : List FloorNum
  goingUpIndicator : Bool
  goingDownIndicator : Bool
  maxPassengerCount : Nat
  passengerCount : Nat
  deriving Repr, Inhabited

def ElevatorView.loadFactor (v : ElevatorView) : Float :=
  if v.maxPassengerCount == 0 then 1.0
  else v.passengerCount.toFloat / v.maxPassengerCount.toFloat

/-- An elevator you can inspect and command.

```
elevator.goToFloor 3
elevator.stop
elevator.currentFloor  -- query state
elevator.loadFactor    -- 0.0 to 1.0
```
-/
structure Elevator where
  view : ElevatorView
  deriving Repr, Inhabited

namespace Elevator

def index (e : Elevator) : ElevatorId := e.view.index
def currentFloor (e : Elevator) : FloorNum := e.view.currentFloor
def destinationQueue (e : Elevator) : List FloorNum := e.view.destinationQueue
def pressedFloors (e : Elevator) : List FloorNum := e.view.pressedFloors
def maxPassengerCount (e : Elevator) : Nat := e.view.maxPassengerCount
def passengerCount (e : Elevator) : Nat := e.view.passengerCount
def loadFactor (e : Elevator) : Float := e.view.loadFactor

/-- The direction the elevator is heading, or `none` if stopped. -/
def destinationDirection (e : Elevator) : Option Direction :=
  match e.view.destinationQueue with
  | [] => none
  | dest :: _ =>
    if dest > e.view.currentFloor then some .up
    else if dest < e.view.currentFloor then some .down
    else none

/-- Queue the elevator to go to the given floor.
    Pass `forceNow := true` to jump to the front of the queue. -/
def goToFloor (e : Elevator) (floor : FloorNum) (forceNow : Bool := false) : CommandM Unit :=
  modify (· ++ [.goToFloor e.view.index floor forceNow])

/-- Clear the destination queue and stop the elevator. -/
def stop (e : Elevator) : CommandM Unit :=
  modify (· ++ [.stop e.view.index])

/-- Set the going-up indicator. Affects which passengers will board. -/
def goingUpIndicator (e : Elevator) (value : Bool) : CommandM Unit :=
  modify (· ++ [.setGoingUpIndicator e.view.index value])

/-- Set the going-down indicator. Affects which passengers will board. -/
def goingDownIndicator (e : Elevator) (value : Bool) : CommandM Unit :=
  modify (· ++ [.setGoingDownIndicator e.view.index value])

end Elevator

/-- A floor in the building. -/
structure FloorView where
  level : FloorNum
  buttonUp : Bool
  buttonDown : Bool
  waitingCount : Nat
  deriving Repr, Inhabited

def FloorView.floorNum (f : FloorView) : FloorNum := f.level

/-- Current state of the game. -/
structure GameView where
  elevators : Array ElevatorView
  floors : Array FloorView
  elapsedTime : Float
  transportedCounter : Nat
  deriving Repr

/-- Get an elevator handle by index. -/
def GameView.elevator (view : GameView) (i : Nat) : Elevator :=
  { view := view.elevators[i]! }

/-- All elevators as handles you can command. -/
def GameView.allElevators (view : GameView) : Array Elevator :=
  view.elevators.map fun v => { view := v }


/-- Your elevator control strategy.

Each handler receives the relevant elevator and game state.
All handlers default to doing nothing — implement the ones you need. -/
class Strategy where
  onIdle : Elevator → GameView → CommandM Unit := fun _ _ => pure ()
  onFloorButtonPressed : Elevator → FloorNum → GameView → CommandM Unit := fun _ _ _ => pure ()
  onStoppedAtFloor : Elevator → FloorNum → GameView → CommandM Unit := fun _ _ _ => pure ()
  onPassingFloor : Elevator → FloorNum → Direction → GameView → CommandM Unit := fun _ _ _ _ => pure ()
  onUpButtonPressed : FloorNum → GameView → CommandM Unit := fun _ _ => pure ()
  onDownButtonPressed : FloorNum → GameView → CommandM Unit := fun _ _ => pure ()
  onUpdate : Float → GameView → CommandM Unit := fun _ _ => pure ()

end EleanvatorSaga
