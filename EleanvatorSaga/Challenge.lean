import EleanvatorSaga.Types

/-! # Challenge definitions — all 18 levels from the original game -/

namespace EleanvatorSaga

structure ChallengeConfig where
  floorCount : Nat
  elevatorCount : Nat
  spawnRate : Float  -- passengers per second (used by continuous sim; discrete sim derives a tick rate)
  elevatorCapacities : List Nat := [4]
  deriving Repr, Inhabited

/-- How to determine capacity for elevator `i` given the config's capacity list. -/
def ChallengeConfig.capacityForElevator (c : ChallengeConfig) (i : Nat) : Nat :=
  match c.elevatorCapacities with
  | [] => 4
  | caps => caps[i % caps.length]!

inductive WinCondition where
  | transportWithinTime (userCount : Nat) (timeLimit : Float)
  | transportWithMaxWait (userCount : Nat) (maxWait : Float)
  | transportWithinTimeAndMaxWait (userCount : Nat) (timeLimit : Float) (maxWait : Float)
  | transportWithinMoves (userCount : Nat) (moveLimit : Nat)
  | demo
  deriving Repr

instance : Inhabited WinCondition := ⟨.demo⟩

structure Challenge where
  config : ChallengeConfig
  condition : WinCondition
  deriving Repr, Inhabited

/-- Evaluate whether the challenge is won, lost, or still in progress.
    Returns `none` if in progress, `some true` if won, `some false` if lost. -/
def WinCondition.evaluate (cond : WinCondition) (transported : Nat) (elapsedTime : Float)
    (maxWaitTime : Float) (moveCount : Nat) : Option Bool :=
  match cond with
  | .transportWithinTime count limit =>
    if elapsedTime >= limit || transported >= count then
      some (elapsedTime <= limit && transported >= count)
    else none
  | .transportWithMaxWait count maxWait =>
    if maxWaitTime >= maxWait || transported >= count then
      some (maxWaitTime <= maxWait && transported >= count)
    else none
  | .transportWithinTimeAndMaxWait count limit maxWait =>
    if elapsedTime >= limit || maxWaitTime >= maxWait || transported >= count then
      some (elapsedTime <= limit && maxWaitTime <= maxWait && transported >= count)
    else none
  | .transportWithinMoves count limit =>
    if moveCount >= limit || transported >= count then
      some (moveCount <= limit && transported >= count)
    else none
  | .demo => none

def Challenge.fmtFloat (f : Float) : String :=
  let n := f.toUInt64.toNat
  let frac := ((f - n.toFloat) * 10).toUInt64.toNat
  if frac == 0 then s!"{n}" else s!"{n}.{frac}"

def WinCondition.description : WinCondition → String
  | .transportWithinTime count limit =>
    s!"Transport {count} people in {Challenge.fmtFloat limit}s or less"
  | .transportWithMaxWait count maxWait =>
    s!"Transport {count} people and let no one wait more than {Challenge.fmtFloat maxWait}s"
  | .transportWithinTimeAndMaxWait count limit maxWait =>
    s!"Transport {count} people in {Challenge.fmtFloat limit}s or less, max wait {Challenge.fmtFloat maxWait}s"
  | .transportWithinMoves count limit =>
    s!"Transport {count} people using {limit} elevator moves or less"
  | .demo => "Perpetual demo"

def Challenge.description (c : Challenge) : String :=
  let cfg := c.config
  let caps := cfg.elevatorCapacities.map toString |> String.intercalate ","
  s!"{cfg.floorCount} floors, {cfg.elevatorCount} elevator(s) [capacity {caps}]\n" ++
  s!"🎯 {c.condition.description}"

private def cfg (fc ec : Nat) (sr : Float) (caps : List Nat := [4]) : ChallengeConfig :=
  { floorCount := fc, elevatorCount := ec, spawnRate := sr, elevatorCapacities := caps }

def challenges : Array Challenge := #[
  ⟨cfg 3 1 0.3,           .transportWithinTime 15 60⟩,
  ⟨cfg 5 1 0.4,           .transportWithinTime 20 60⟩,
  ⟨cfg 5 1 0.5 [6],       .transportWithinTime 23 60⟩,
  ⟨cfg 8 2 0.6,           .transportWithinTime 28 60⟩,
  ⟨cfg 6 4 1.7,           .transportWithinTime 100 68⟩,
  ⟨cfg 4 2 0.8,           .transportWithinMoves 40 60⟩,
  ⟨cfg 3 3 3.0,           .transportWithinMoves 100 63⟩,
  ⟨cfg 6 2 0.4 [5],       .transportWithMaxWait 50 21⟩,
  ⟨cfg 7 3 0.6,           .transportWithMaxWait 50 20⟩,
  ⟨cfg 13 2 1.1 [4, 10],  .transportWithinTime 50 70⟩,
  ⟨cfg 9 5 1.1,           .transportWithMaxWait 60 19⟩,
  ⟨cfg 9 5 1.1,           .transportWithMaxWait 80 17⟩,
  ⟨cfg 9 5 1.1 [5],       .transportWithMaxWait 100 15⟩,
  ⟨cfg 9 5 1.0 [6],       .transportWithMaxWait 110 15⟩,
  ⟨cfg 8 6 0.9,           .transportWithMaxWait 120 14⟩,
  ⟨cfg 12 4 1.4 [5, 10],  .transportWithinTime 70 80⟩,
  ⟨cfg 21 5 1.9 [10],     .transportWithinTime 110 80⟩,
  ⟨cfg 21 8 1.5 [6, 8],   .transportWithinTimeAndMaxWait 2675 1800 45⟩,
  ⟨cfg 21 8 1.5 [6, 8],   .demo⟩
]

end EleanvatorSaga
