import EleanvatorSaga.Sim.World
import EleanvatorSaga.Challenge

/-! # Unicode renderer for the continuous simulation -/

namespace EleanvatorSaga

open Sim

/-- Pad a string on the left with spaces to `n` characters. -/
private def padLeft (s : String) (n : Nat) : String :=
  let len := s.toList.length
  if len >= n then s
  else String.ofList (List.replicate (n - len) ' ') ++ s

/-- Render an elevator shaft cell for a given floor.
    7 display columns wide: │xxxxx│ (or ┃xxxxx┃ when car is here). -/
private def shaftCell (e : Sim.Elevator) (passengers : Array Sim.Passenger) (fi : FloorNum) : String :=
  if e.getRoundedCurrentFloor == fi then
    let pInside := passengers.filter fun (p : Passenger) => p.inElevator == some e.index
    let n := pInside.size
    let dir := if e.velocityY < 0.0 then "↑"
      else if e.velocityY > 0.0 then "↓"
      else " "
    -- 🧑 is 2 display cols, so "🧑 2↑" fits in 5 display cols inside ┃...┃
    if n > 0 then s!"┃🧑{padLeft (toString n) 2}{dir}┃"
    else s!"┃    {dir}┃"
  else
    "│     │"

def renderWorld (w : World) (config : ChallengeConfig)
    (challenge? : Option (Nat × Challenge) := none) : String := Id.run do
  let mut lines : List String := []
  let numE := config.elevatorCount

  -- Challenge info
  if let some (num, ch) := challenge? then
    lines := lines ++ [s!"━━━ Challenge {num + 1} ━━━"]
    lines := lines ++ [ch.description, ""]

  -- Stats — show progress toward the goal
  let elapsed := w.elapsedTime.toUInt64.toNat
  let mwW := w.maxWaitTime.toUInt64.toNat
  let mwF := ((w.maxWaitTime - mwW.toFloat) * 10.0).toUInt64.toNat
  let goalStr := match challenge? with
    | some (_, ch) => match ch.condition with
      | .transportWithinTime count _ => s!" / {count}"
      | .transportWithMaxWait count _ => s!" / {count}"
      | .transportWithinTimeAndMaxWait count _ _ => s!" / {count}"
      | .transportWithinMoves count _ => s!" / {count}"
      | .demo => ""
    | none => ""
  lines := lines ++ [s!"Time {elapsed}s | Transported {w.transportedCounter}{goalStr} | Max wait {mwW}.{mwF}s | Moves {w.moveCount}", ""]

  -- Building prefix is 7 chars: " NN XY " (floor label + buttons + space)
  -- Emojis go AFTER shafts so they never affect shaft alignment.
  let blank := "       "

  -- Roof
  let roof := String.join (List.replicate numE "┌─────┐")
  lines := lines ++ [s!"{blank}{roof}"]

  -- Floors top to bottom
  let topFloor := config.floorCount - 1
  for fi in List.reverse (List.range config.floorCount) do
    let floor := w.floors[fi]!
    let label := padLeft (toString fi) 2
    let up := if fi < topFloor then (if floor.buttonUp then "▲" else "△") else " "
    let dn := if fi > 0 then (if floor.buttonDown then "▼" else "▽") else " "
    let shafts := String.join (w.elevators.toList.map fun e =>
      shaftCell e w.passengers fi)
    -- Waiting passengers: emoji + count, right of the shafts
    let waiting := w.passengers.filter fun (p : Passenger) =>
      p.inElevator.isNone && !p.done && p.currentFloor == fi
    let maxShow := 8
    let shown := min waiting.size maxShow
    let people := String.join (List.replicate shown "🚶")
    let overflow := if waiting.size > maxShow then s!"+{waiting.size - maxShow}" else ""
    let waitStr : String :=
      if waiting.size > 0 then s!" {people}{overflow}"
      else ""
    lines := lines ++ [s!" {label} {up}{dn} {shafts}{waitStr}"]
    if fi > 0 then
      let sep := String.join (List.replicate numE "├─────┤")
      lines := lines ++ [s!"{blank}{sep}"]

  -- Ground
  let ground := String.join (List.replicate numE "└─────┘")
  lines := lines ++ [s!"{blank}{ground}"]

  -- Elevator details
  lines := lines ++ [""]
  for e in w.elevators.toList do
    let pInside := w.passengers.filter fun (p : Passenger) => p.inElevator == some e.index
    let pressed := e.pressedFloors
    let qStr := if e.destinationQueue.isEmpty then "idle"
      else String.intercalate " > " (e.destinationQueue.map toString)
    let bStr := if pressed.isEmpty then "none"
      else String.intercalate ", " (pressed.map toString)
    let rStr := if pInside.size == 0 then ""
      else
        let dests := String.intercalate ", " (pInside.toList.map fun p => toString p.destinationFloor)
        s!"  🧑x{pInside.size} → floor(s) {dests}"
    let upInd := if e.goingUpIndicator then "▲" else " "
    let dnInd := if e.goingDownIndicator then "▼" else " "
    lines := lines ++ [s!"  Elevator {e.index} {upInd}{dnInd} [{pInside.size}/{e.maxUsers}] floor {e.currentFloor}  queue: {qStr}  buttons: {bStr}{rStr}"]

  -- Result
  if let some (_, ch) := challenge? then
    let result := ch.condition.evaluate w.transportedCounter w.elapsedTime w.maxWaitTime w.moveCount
    lines := lines ++ [""]
    match result with
    | some true =>
      lines := lines ++ [s!"🎉 Challenge complete! ({elapsed}s, {w.transportedCounter} transported, {w.moveCount} moves)"]
    | some false =>
      let reason := match ch.condition with
        | .transportWithinTime count limit =>
          if w.transportedCounter < count then s!"Only transported {w.transportedCounter} / {count}"
          else s!"Ran out of time"
        | .transportWithMaxWait _ maxWait =>
          if w.maxWaitTime > maxWait then s!"Someone waited {mwW}.{mwF}s (max {Challenge.fmtFloat maxWait}s)"
          else s!"Not enough transported"
        | .transportWithinTimeAndMaxWait _ _ maxWait =>
          if w.maxWaitTime > maxWait then s!"Someone waited {mwW}.{mwF}s (max {Challenge.fmtFloat maxWait}s)"
          else s!"Ran out of time"
        | .transportWithinMoves count limit =>
          if w.moveCount > limit then s!"Used {w.moveCount} moves (max {limit})"
          else s!"Only transported {w.transportedCounter} / {count}"
        | .demo => ""
      lines := lines ++ [s!"💥 Challenge failed. {reason}"]
    | none => pure ()

  "\n".intercalate lines

end EleanvatorSaga
