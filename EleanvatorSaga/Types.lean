/-! # Core types for Eleanvator Saga -/

namespace EleanvatorSaga

inductive Direction where
  | up
  | down
  deriving Repr, BEq, DecidableEq, Inhabited

instance : ToString Direction where
  toString
    | .up => "↑"
    | .down => "↓"

inductive DisplayType where
  | male
  | female
  | child
  deriving Repr, BEq, DecidableEq, Inhabited

/-- Unique identifier for a passenger. -/
abbrev PassengerId := Nat

/-- Unique identifier for an elevator. -/
abbrev ElevatorId := Nat

/-- A floor number (0 = ground). -/
abbrev FloorNum := Nat

/-- Events generated during a simulation step, dispatched to the player's strategy. -/
inductive GameEvent where
  | idle (elevatorIdx : ElevatorId)
  | floorButtonPressed (elevatorIdx : ElevatorId) (floorNum : FloorNum)
  | stoppedAtFloor (elevatorIdx : ElevatorId) (floorNum : FloorNum)
  | passingFloor (elevatorIdx : ElevatorId) (floorNum : FloorNum) (direction : Direction)
  | upButtonPressed (floorNum : FloorNum)
  | downButtonPressed (floorNum : FloorNum)
  deriving Repr, BEq

end EleanvatorSaga
