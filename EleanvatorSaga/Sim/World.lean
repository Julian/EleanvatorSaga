import EleanvatorSaga.Types
import EleanvatorSaga.Strategy
import EleanvatorSaga.Challenge

/-! # Continuous simulation — faithful port of the original elevatorsaga physics -/

namespace EleanvatorSaga.Sim

-- ══════════════════════════════════════════════════════════════════════
-- Physics helpers (from base.js)
-- ══════════════════════════════════════════════════════════════════════

def limitNumber (num lo hi : Float) : Float :=
  Min.min hi (Max.max num lo)

def epsilonEquals (a b : Float) : Bool :=
  (a - b).abs < 0.00000001

def sign (x : Float) : Float :=
  if x > 0 then 1.0 else if x < 0 then -1.0 else 0.0

/-- v² = u² + 2ad → d = (v² - u²) / 2a -/
def distanceNeededToAchieveSpeed (currentSpeed targetSpeed acceleration : Float) : Float :=
  (targetSpeed ^ 2 - currentSpeed ^ 2) / (2.0 * acceleration)

/-- v² = u² + 2ad → a = (v² - u²) / 2d -/
def accelerationNeededToAchieveChangeDistance (currentSpeed targetSpeed distance : Float) : Float :=
  0.5 * ((targetSpeed ^ 2 - currentSpeed ^ 2) / distance)

-- ══════════════════════════════════════════════════════════════════════
-- RNG (same as discrete model)
-- ══════════════════════════════════════════════════════════════════════

structure Rng where
  state : Nat
  deriving Repr

def Rng.next (rng : Rng) : Rng × Nat :=
  let s := (rng.state * 1664525 + 1013904223) % (2 ^ 32)
  (⟨s⟩, s)

def Rng.natBelow (rng : Rng) (n : Nat) : Rng × Nat :=
  if n == 0 then (rng, 0)
  else
    -- Use upper bits to avoid LCG low-bit correlation
    let (rng', v) := rng.next
    (rng', (v / 65536) % n)

def Rng.floatRange (rng : Rng) (lo hi : Nat) : Rng × Nat :=
  if hi < lo then (rng, lo)
  else let (rng', v) := rng.natBelow (hi - lo + 1); (rng', lo + v)

-- ══════════════════════════════════════════════════════════════════════
-- Domain types
-- ══════════════════════════════════════════════════════════════════════

def floorHeight : Float := 50.0
def speedFloorsPerSec : Float := 2.6

structure Passenger where
  id : PassengerId
  weight : Float
  displayType : DisplayType
  currentFloor : FloorNum
  destinationFloor : FloorNum
  done : Bool := false
  inElevator : Option ElevatorId := none
  spawnTimestamp : Float := 0.0
  deriving Repr, BEq, Inhabited

structure Elevator where
  index : ElevatorId
  floorCount : Nat
  maxUsers : Nat := 4
  y : Float := 0.0
  destinationY : Float := 0.0
  velocityY : Float := 0.0
  isMoving : Bool := false
  goingUpIndicator : Bool := true
  goingDownIndicator : Bool := true
  currentFloor : FloorNum := 0
  previousTruncFutureFloorIfStopped : Nat := 0
  buttonStates : Array Bool
  moveCount : Nat := 0
  passengers : List PassengerId := []
  /-- Interface destination queue (managed by goToFloor/stop). -/
  destinationQueue : List FloorNum := []
  /-- True when the elevator is executing a wait (e.g., at a floor). -/
  busy : Bool := false
  busyTimer : Float := 0.0
  deriving Repr, BEq, Inhabited

def Elevator.acceleration (_e : Elevator) : Float := floorHeight * 2.1
def Elevator.deceleration (_e : Elevator) : Float := floorHeight * 2.6
def Elevator.maxSpeed (_e : Elevator) : Float := floorHeight * speedFloorsPerSec

def Elevator.getYPosOfFloor (e : Elevator) (floor : FloorNum) : Float :=
  (e.floorCount - 1).toFloat * floorHeight - floor.toFloat * floorHeight

def Elevator.getExactFloorOfYPos (_e : Elevator) (floorCount : Nat) (y : Float) : Float :=
  ((floorCount - 1).toFloat * floorHeight - y) / floorHeight

def Elevator.getExactCurrentFloor (e : Elevator) : Float :=
  e.getExactFloorOfYPos e.floorCount e.y

def Elevator.getRoundedCurrentFloor (e : Elevator) : Nat :=
  e.getExactCurrentFloor.round.toUInt64.toNat

def Elevator.isOnAFloor (e : Elevator) : Bool :=
  epsilonEquals e.getExactCurrentFloor e.getRoundedCurrentFloor.toFloat

def Elevator.getExactFutureFloorIfStopped (e : Elevator) : Float :=
  let dist := distanceNeededToAchieveSpeed e.velocityY 0.0 e.deceleration
  e.getExactFloorOfYPos e.floorCount (e.y - sign e.velocityY * dist)

def Elevator.isFull (e : Elevator) : Bool :=
  e.passengers.length >= e.maxUsers

def Elevator.isEmpty (e : Elevator) : Bool :=
  e.passengers.isEmpty

def Elevator.isSuitableForTravelBetween (e : Elevator) (fromFloor toFloor : FloorNum) : Bool :=
  if fromFloor > toFloor then e.goingDownIndicator
  else if fromFloor < toFloor then e.goingUpIndicator
  else true

def Elevator.pressedFloors (e : Elevator) : List FloorNum :=
  (List.range e.buttonStates.size).filter fun i =>
    e.buttonStates[i]?.getD false

def Elevator.getLoadFactor (e : Elevator) (allPassengers : Array Passenger) : Float :=
  let load := allPassengers.foldl (fun sum (p : Passenger) =>
    if e.passengers.contains p.id then sum + p.weight else sum) 0.0
  load / (e.maxUsers.toFloat * 100.0)

structure Floor where
  level : FloorNum
  buttonUp : Bool := false
  buttonDown : Bool := false
  deriving Repr, BEq, Inhabited

structure World where
  floors : Array Floor
  elevators : Array Elevator
  passengers : Array Passenger
  nextPassengerId : Nat := 0
  elapsedTime : Float := 0.0
  transportedCounter : Nat := 0
  moveCount : Nat := 0
  maxWaitTime : Float := 0.0
  avgWaitTime : Float := 0.0
  rng : Rng
  /-- Start with enough accumulated time to spawn a passenger immediately,
      matching the original: `elapsedSinceSpawn = 1.001/spawnRate`. -/
  spawnAccum : Float
  deriving Repr

-- ══════════════════════════════════════════════════════════════════════
-- Views for the strategy
-- ══════════════════════════════════════════════════════════════════════

def elevatorView (e : Elevator) (_passengers : Array Passenger) : ElevatorView :=
  { index := e.index
    currentFloor := e.currentFloor
    destinationQueue := e.destinationQueue
    pressedFloors := e.pressedFloors
    goingUpIndicator := e.goingUpIndicator
    goingDownIndicator := e.goingDownIndicator
    maxPassengerCount := e.maxUsers
    passengerCount := e.passengers.length }

def floorView (f : Floor) (passengers : Array Passenger) : FloorView :=
  { level := f.level
    buttonUp := f.buttonUp
    buttonDown := f.buttonDown
    waitingCount := (passengers.filter fun (p : Passenger) =>
      p.inElevator.isNone && !p.done && p.currentFloor == f.level).size }

def gameView (w : World) : GameView :=
  { elevators := w.elevators.map (elevatorView · w.passengers)
    floors := w.floors.map (floorView · w.passengers)
    elapsedTime := w.elapsedTime
    transportedCounter := w.transportedCounter }

-- ══════════════════════════════════════════════════════════════════════
-- Initialization
-- ══════════════════════════════════════════════════════════════════════

def initWorld (config : ChallengeConfig) (seed : Nat := 42) : World :=
  let floors := Array.ofFn (n := config.floorCount) fun i => { level := i.val : Floor }
  let elevators := Array.ofFn (n := config.elevatorCount) fun i =>
    let cap := config.capacityForElevator i
    let e : Elevator := {
      index := i.val
      floorCount := config.floorCount
      maxUsers := cap
      buttonStates := Array.ofFn (n := config.floorCount) fun _ => false
    }
    { e with
      y := e.getYPosOfFloor 0
      destinationY := e.getYPosOfFloor 0 }
  { floors, elevators, passengers := #[], rng := ⟨seed⟩,
    spawnAccum := 1.001 / config.spawnRate }

-- ══════════════════════════════════════════════════════════════════════
-- Elevator physics (faithful port of updateElevatorMovement)
-- ══════════════════════════════════════════════════════════════════════

def updateElevatorMovement (dt : Float) (e : Elevator) : Elevator × List GameEvent :=
  if e.busy then
    -- Counting down wait timer
    if e.busyTimer > 0 then
      ({ e with busyTimer := e.busyTimer - dt }, [])
    else
      ({ e with busy := false, busyTimer := 0.0 }, [])
  else if !e.isMoving then
    (e, [])
  else
    -- Clamp velocity
    let velocityY := limitNumber e.velocityY (-e.maxSpeed) e.maxSpeed
    -- Move elevator
    let y := e.y + velocityY * dt
    let e := { e with y, velocityY }
    let destinationDiff := e.destinationY - e.y
    let directionSign := sign destinationDiff
    let velocitySign := sign e.velocityY
    let e :=
      if destinationDiff != 0.0 then
        if directionSign == velocitySign then
          -- Moving in correct direction
          let distNeeded := distanceNeededToAchieveSpeed e.velocityY 0.0 e.deceleration
          if distNeeded * 1.05 < -(destinationDiff.abs) then
            -- Need to slow down
            let reqDecel := accelerationNeededToAchieveChangeDistance e.velocityY 0.0 destinationDiff
            let decel := min (e.deceleration * 1.1) reqDecel.abs
            { e with velocityY := e.velocityY - directionSign * decel * dt }
          else
            -- Speed up
            let accel := min (destinationDiff.abs * 5.0) e.acceleration
            { e with velocityY := e.velocityY + directionSign * accel * dt }
        else if velocitySign == 0.0 then
          -- Standing still, accelerate
          let accel := min (destinationDiff.abs * 5.0) e.acceleration
          { e with velocityY := e.velocityY + directionSign * accel * dt }
        else
          -- Wrong direction, decelerate
          let newV := e.velocityY - velocitySign * e.deceleration * dt
          if sign newV != velocitySign then
            { e with velocityY := 0.0 }
          else
            { e with velocityY := newV }
      else e
    -- Check if arrived (snap to destination)
    if e.isMoving && (e.destinationY - e.y).abs < 0.5 && e.velocityY.abs < 3.0 then
      let e := { e with y := e.destinationY, velocityY := 0.0, isMoving := false }
      -- Handle destination arrival
      let roundedFloor := e.getRoundedCurrentFloor
      if e.isOnAFloor then
        let e := { e with
          currentFloor := roundedFloor
          buttonStates := e.buttonStates.set! roundedFloor false }
        (e, [GameEvent.stoppedAtFloor e.index roundedFloor])
      else
        (e, [])
    else
      -- Check for floor passing and floor changes
      let roundedFloor := e.getRoundedCurrentFloor
      -- Update current floor if changed
      let e := if roundedFloor != e.currentFloor then
        { e with moveCount := e.moveCount + 1, currentFloor := roundedFloor }
      else e
      -- Check passing_floor
      let futureTruncF := e.getExactFutureFloorIfStopped.floor
      let futureTrunc := futureTruncF.toUInt64.toNat
      let (e, passingEvents) :=
        if futureTrunc != e.previousTruncFutureFloorIfStopped then
          let floorBeingPassed := e.getExactFutureFloorIfStopped.round.toUInt64.toNat
          let destFloor := (e.getExactFloorOfYPos e.floorCount e.destinationY).round.toUInt64.toNat
          let e := { e with previousTruncFutureFloorIfStopped := futureTrunc }
          if destFloor != floorBeingPassed then
            let floorYPos := e.getYPosOfFloor floorBeingPassed
            let elevToFloor := floorYPos - e.y
            let approaching := e.velocityY != 0.0 && sign e.velocityY == sign elevToFloor
            if approaching then
              let direction : Direction := if e.velocityY > 0.0 then .down else .up
              (e, [GameEvent.passingFloor e.index floorBeingPassed direction])
            else (e, [])
          else (e, [])
        else (e, [])
      (e, passingEvents)

-- ══════════════════════════════════════════════════════════════════════
-- Interface logic (faithful port of interfaces.js)
-- ══════════════════════════════════════════════════════════════════════

/-- Try to start moving to the next destination in the queue. -/
def checkDestinationQueue (e : Elevator) : Elevator × List GameEvent :=
  if !e.busy && !e.isMoving then
    match e.destinationQueue with
    | [] => (e, [GameEvent.idle e.index])
    | dest :: _ =>
      let destY := e.getYPosOfFloor dest
      ({ e with isMoving := true, destinationY := destY }, [])
  else
    (e, [])

/-- Handle arrival: dequeue, wait 1 second, then check for next destination. -/
def handleArrival (e : Elevator) (floorNum : FloorNum) : Elevator :=
  let queue := match e.destinationQueue with
    | [] => []
    | dest :: rest =>
      if epsilonEquals dest.toFloat floorNum.toFloat then rest
      else e.destinationQueue
  { e with destinationQueue := queue, busy := true, busyTimer := 1.0 }

def enqueueFloor (e : Elevator) (floorNum : FloorNum) (forceNow : Bool) : Elevator :=
  let floorNum := Nat.min floorNum (e.floorCount - 1)
  -- Deduplicate adjacent
  let dominated := match e.destinationQueue with
    | [] => false
    | _ =>
      if forceNow then e.destinationQueue.head? == some floorNum
      else e.destinationQueue.getLast? == some floorNum
  if dominated then e
  else
    let queue := if forceNow then floorNum :: e.destinationQueue
                 else e.destinationQueue ++ [floorNum]
    { e with destinationQueue := queue }

-- ══════════════════════════════════════════════════════════════════════
-- Passenger spawning (faithful to world.js)
-- ══════════════════════════════════════════════════════════════════════

def spawnPassenger (w : World) (config : ChallengeConfig) : World × List GameEvent :=
  let floorCount := config.floorCount
  if floorCount ≤ 1 then (w, []) else
  -- Weight: 55-100
  let (rng, weight) := w.rng.floatRange 55 100
  -- Display type: 1/41 child, else 50/50 male/female
  let (rng, typeRoll) := rng.natBelow 41
  let (rng, genderRoll) := rng.natBelow 2
  let displayType : DisplayType :=
    if typeRoll == 0 then .child
    else if genderRoll == 0 then .female
    else .male
  -- Current floor: 50% ground, else random
  let (rng, coinFlip) := rng.natBelow 2
  let (rng, randFloor) := rng.natBelow floorCount
  let currentFloor := if coinFlip == 0 then 0 else randFloor
  -- Destination
  let (rng, destFloor) :=
    if currentFloor == 0 then
      let (rng, r) := rng.natBelow (floorCount - 1)
      (rng, r + 1)
    else
      let (rng, rareRoll) := rng.natBelow 11
      if rareRoll == 0 then
        let (rng, r) := rng.natBelow (floorCount - 1)
        (rng, (currentFloor + r + 1) % floorCount)
      else
        (rng, 0)
  let passenger : Passenger :=
    { id := w.nextPassengerId
      weight := weight.toFloat
      displayType
      currentFloor
      destinationFloor := destFloor
      spawnTimestamp := w.elapsedTime }
  -- Press floor button and emit event
  let goingUp := destFloor > currentFloor
  let floors := w.floors.modify currentFloor fun f =>
    if goingUp then { f with buttonUp := true }
    else { f with buttonDown := true }
  let event := if goingUp then GameEvent.upButtonPressed currentFloor
               else GameEvent.downButtonPressed currentFloor
  ({ w with
    passengers := w.passengers.push passenger
    nextPassengerId := w.nextPassengerId + 1
    floors, rng },
   [event])

-- ══════════════════════════════════════════════════════════════════════
-- Passenger boarding/exiting at a floor
-- ══════════════════════════════════════════════════════════════════════

/-- Process passenger exits and entrances at a floor.
    Returns the updated world, events, and the number of passengers who boarded/exited
    (used to scale the stop time). -/
def processArrivals (w : World) (elevIdx : ElevatorId) (floorNum : FloorNum) : World × List GameEvent × Nat :=
  if elevIdx >= w.elevators.size then (w, [], 0) else
  let elev := w.elevators[elevIdx]!
  -- 1) Exit: passengers whose destination is this floor
  let exitingIds := elev.passengers.filter fun pid =>
    match w.passengers.find? (fun p => p.id == pid) with
    | some p => p.destinationFloor == floorNum
    | none => false
  let stayingIds := elev.passengers.filter (!exitingIds.contains ·)
  -- Mark exiting passengers as done, record wait times
  let (passengers, transported, maxWait, avgWait) :=
    w.passengers.foldl (fun (acc : Array Passenger × Nat × Float × Float) (p : Passenger) =>
      let (ps, tc, mw, aw) := acc
      if exitingIds.contains p.id then
        let waitTime := w.elapsedTime - p.spawnTimestamp
        let tc' := tc + 1
        let mw' := max mw waitTime
        let aw' := (aw * (tc'.toFloat - 1.0) + waitTime) / tc'.toFloat
        (ps.push { p with done := true, currentFloor := floorNum, inElevator := none },
         tc', mw', aw')
      else
        (ps.push p, tc, mw, aw)
    ) (#[], w.transportedCounter, w.maxWaitTime, w.avgWaitTime)
  -- 2) Board: waiting passengers on this floor, if elevator has room and direction matches
  let spotsAvailable := elev.maxUsers - stayingIds.length
  let (boarding, passengers) :=
    passengers.foldl (fun (acc : List PassengerId × Array Passenger) (p : Passenger) =>
      let (boardList, ps) := acc
      if p.inElevator.isNone && !p.done && p.currentFloor == floorNum &&
         boardList.length < spotsAvailable &&
         elev.isSuitableForTravelBetween floorNum p.destinationFloor then
        (boardList ++ [p.id],
         ps.map fun (q : Passenger) =>
           if q.id == p.id then { q with inElevator := some elevIdx } else q)
      else (boardList, ps)
    ) ([], passengers)
  -- Update elevator
  let elev' := { elev with passengers := stayingIds ++ boarding }
  -- Set button states for boarding passengers
  let elev' := boarding.foldl (fun (e : Elevator) pid =>
    match passengers.find? (fun p => p.id == pid) with
    | some p =>
      if p.destinationFloor < e.buttonStates.size then
        { e with buttonStates := e.buttonStates.set! p.destinationFloor true }
      else e
    | none => e) elev'
  -- Generate button press events
  let buttonEvents := boarding.filterMap fun pid =>
    match passengers.find? (fun p => p.id == pid) with
    | some p => some (GameEvent.floorButtonPressed elevIdx p.destinationFloor)
    | none => none
  -- Update floor buttons
  let anyWaitingUp := passengers.any fun (p : Passenger) =>
    p.inElevator.isNone && !p.done && p.currentFloor == floorNum && p.destinationFloor > floorNum
  let anyWaitingDown := passengers.any fun (p : Passenger) =>
    p.inElevator.isNone && !p.done && p.currentFloor == floorNum && p.destinationFloor < floorNum
  let floors := w.floors.modify floorNum fun f =>
    { f with buttonUp := anyWaitingUp, buttonDown := anyWaitingDown }
  let elevators := w.elevators.set! elevIdx elev'
  let transferCount := exitingIds.length + boarding.length
  ({ w with elevators, passengers, floors,
            transportedCounter := transported,
            maxWaitTime := maxWait, avgWaitTime := avgWait },
   buttonEvents, transferCount)

-- ══════════════════════════════════════════════════════════════════════
-- Command application
-- ══════════════════════════════════════════════════════════════════════

def applyCommand (w : World) (cmd : Command) : World :=
  match cmd with
  | .goToFloor elevIdx floor forceNow =>
    if elevIdx < w.elevators.size then
      let e := w.elevators[elevIdx]!
      let e := enqueueFloor e floor forceNow
      -- Try to start moving if idle
      let (e, _) := if !e.isMoving && !e.busy then checkDestinationQueue e else (e, [])
      { w with elevators := w.elevators.set! elevIdx e }
    else w
  | .stop elevIdx =>
    if elevIdx < w.elevators.size then
      let e := w.elevators[elevIdx]!
      { w with elevators := w.elevators.set! elevIdx { e with destinationQueue := [] } }
    else w
  | .setGoingUpIndicator elevIdx val =>
    if elevIdx < w.elevators.size then
      let e := w.elevators[elevIdx]!
      { w with elevators := w.elevators.set! elevIdx { e with goingUpIndicator := val } }
    else w
  | .setGoingDownIndicator elevIdx val =>
    if elevIdx < w.elevators.size then
      let e := w.elevators[elevIdx]!
      { w with elevators := w.elevators.set! elevIdx { e with goingDownIndicator := val } }
    else w

def applyCommands (w : World) (cmds : List Command) : World :=
  cmds.foldl applyCommand w

-- ══════════════════════════════════════════════════════════════════════
-- Event dispatch
-- ══════════════════════════════════════════════════════════════════════

private def mkElevatorHandle (view : GameView) (idx : ElevatorId) : EleanvatorSaga.Elevator :=
  { view := view.elevators[idx]! }

def dispatchEvent [strategy : Strategy] (view : GameView) (event : GameEvent) : List Command :=
  cmds <| match event with
  | .idle idx => strategy.onIdle (mkElevatorHandle view idx) view
  | .floorButtonPressed idx floor => strategy.onFloorButtonPressed (mkElevatorHandle view idx) floor view
  | .stoppedAtFloor idx floor => strategy.onStoppedAtFloor (mkElevatorHandle view idx) floor view
  | .passingFloor idx floor dir => strategy.onPassingFloor (mkElevatorHandle view idx) floor dir view
  | .upButtonPressed floor => strategy.onUpButtonPressed floor view
  | .downButtonPressed floor => strategy.onDownButtonPressed floor view

-- ══════════════════════════════════════════════════════════════════════
-- Main simulation step (faithful to world.js update)
-- ══════════════════════════════════════════════════════════════════════

/-- One physics substep. -/
def substep (dt : Float) (config : ChallengeConfig) [strategy : Strategy] (w : World) : World :=
  let w := { w with elapsedTime := w.elapsedTime + dt }
  -- Spawn passengers (while loop to handle bursts at high spawn rates)
  let spawnInterval := 1.0 / config.spawnRate
  let w := { w with spawnAccum := w.spawnAccum + dt }
  let rec spawnLoop (fuel : Nat) (w : World) (evs : List GameEvent) : World × List GameEvent :=
    match fuel with
    | 0 => (w, evs)
    | fuel + 1 =>
      if w.spawnAccum >= spawnInterval then
        let w := { w with spawnAccum := w.spawnAccum - spawnInterval }
        let (w, newEvs) := spawnPassenger w config
        spawnLoop fuel w (evs ++ newEvs)
      else (w, evs)
  let (w, spawnEvents) := spawnLoop 10 w []  -- cap at 10 spawns per frame
  -- Update max wait time for all waiting passengers
  let maxWait := w.passengers.foldl (fun mw (p : Passenger) =>
    if !p.done then max mw (w.elapsedTime - p.spawnTimestamp) else mw) w.maxWaitTime
  let w := { w with maxWaitTime := maxWait }
  -- Update elevator physics, collect events
  let (w, events) := w.elevators.foldl (fun (acc : World × List GameEvent) (e : Elevator) =>
    let (w, events) := acc
    let (e', newEvents) := updateElevatorMovement dt e
    let w := { w with elevators := w.elevators.set! e.index e' }
    -- Handle arrivals
    let (w, arrivalEvents) := newEvents.foldl (fun (acc : World × List GameEvent) ev =>
      let (w, extra) := acc
      match ev with
      | GameEvent.stoppedAtFloor idx floor =>
        -- Mark arrival on interface (dequeue + base wait)
        let elev := w.elevators[idx]!
        let elev := handleArrival elev floor
        let w := { w with elevators := w.elevators.set! idx elev }
        -- Process passenger exits/entrances
        let (w, buttonEvents, transferCount) := processArrivals w idx floor
        -- In the original, the 1s wait and passenger boarding/exiting animations
        -- run concurrently. The elevator departs after 1s unless there are many
        -- passengers (in which case boarding takes longer than the wait).
        -- We don't animate individual passengers, so just use 1s for the common
        -- case and add time only for large transfers.
        let elev := w.elevators[idx]!
        let transferTime := if transferCount > 4 then transferCount.toFloat * 0.25 else 0.0
        let elev := { elev with busyTimer := max elev.busyTimer (1.15 + transferTime) }
        let w := { w with elevators := w.elevators.set! idx elev }
        -- After processing, check destination queue
        let elev := w.elevators[idx]!
        let (elev, queueEvents) := checkDestinationQueue elev
        let w := { w with elevators := w.elevators.set! idx elev }
        (w, extra ++ buttonEvents ++ queueEvents)
      | _ => (w, extra)
    ) (w, [])
    -- After busy timer expires, check queue (fires idle if empty)
    -- Only trigger when transitioning from busy to not-busy (e was busy, e' is not)
    let e' := w.elevators[e.index]!
    let wasBusy := e.busy
    let (w, timerEvents) :=
      if wasBusy && !e'.busy && !e'.isMoving then
        let (e'', evs) := checkDestinationQueue e'
        ({ w with elevators := w.elevators.set! e.index e'' }, evs)
      else (w, [])
    (w, events ++ newEvents ++ arrivalEvents ++ timerEvents)
  ) (w, [])
  -- Update move count
  let mc := w.elevators.foldl (fun acc (e : Elevator) => acc + e.moveCount) 0
  let w := { w with moveCount := mc }
  -- Remove done passengers
  let w := { w with passengers := w.passengers.filter fun (p : Passenger) => !p.done }
  -- Dispatch all events (spawn + elevator) to strategy
  let allEvents := spawnEvents ++ events
  let view := gameView w
  let eventCmds := allEvents.foldl (fun acc ev => acc ++ dispatchEvent view ev) []
  let updateCmds := cmds (Strategy.onUpdate dt view)
  applyCommands w (eventCmds ++ updateCmds)

/-- Simulate for `duration` seconds of game time, with `dtMax` substeps. -/
def simulate (duration : Float) (config : ChallengeConfig) [strategy : Strategy]
    (seed : Nat := 42) (dtMax : Float := 1.0 / 60.0) : World :=
  let w := initWorld config seed
  -- Fire initial idle events
  let (w, initEvents) := w.elevators.foldl (fun (acc : World × List GameEvent) (e : Elevator) =>
    let (w, events) := acc
    let (e', evs) := checkDestinationQueue e
    let w := { w with elevators := w.elevators.set! e.index e' }
    (w, events ++ evs)
  ) (w, [])
  let view := gameView w
  let initCmds := initEvents.foldl (fun acc ev => acc ++ dispatchEvent view ev) []
  let w := applyCommands w initCmds
  -- Run substeps
  let totalSteps := (duration / dtMax).toUInt64.toNat
  let rec go (fuel : Nat) (w : World) : World :=
    match fuel with
    | 0 => w
    | fuel + 1 => go fuel (substep dtMax config w)
  go totalSteps w

end EleanvatorSaga.Sim
