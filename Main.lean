import EleanvatorSaga
open EleanvatorSaga

instance : Strategy where
  -- Whenever the elevator is idle (has no more queued destinations) ...
  -- let's go to all the floors (or did we forget one?)
  onIdle elevator _game := do
    elevator.goToFloor 0
    elevator.goToFloor 1
    -- Maybe we missed one??
  -- Do more stuff with the elevators and floors
  -- onFloorButtonPressed elevator floorNum game := do ...
  -- onStoppedAtFloor elevator floorNum game := do ...
  -- onPassingFloor elevator floorNum direction game := do ...
  -- onUpButtonPressed floorNum game := do ...
  -- onDownButtonPressed floorNum game := do ...

-- Run time at 4x speed
#play_speed 4

-- Change me to proceed to the next level by incrementing once you've solved a level!
#play 1
