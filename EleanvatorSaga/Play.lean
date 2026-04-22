import EleanvatorSaga.Sim.World
import EleanvatorSaga.Render
import EleanvatorSaga.RenderSvg
import EleanvatorSaga.Challenge
import ProofWidgets.Component.RefreshComponent
import ProofWidgets.Component.HtmlDisplay

/-! # Game widget — animated simulation in the infoview -/

open ProofWidgets Jsx Lean Server Widget Elab Command

namespace EleanvatorSaga

/-- Render the world state as Html for the infoview. -/
def renderHtml (w : Sim.World) (config : ChallengeConfig)
    (challenge? : Option (Nat × Challenge) := none) : Html :=
  renderSvgHtml w config challenge?

/-- Current play speed, settable via `#play_speed N`. -/
initialize playSpeed : IO.Ref Nat ← IO.mkRef 1

/-- `#play_speed N` — set the simulation speed multiplier. -/
scoped elab "#play_speed " n:num : command => do
  playSpeed.set n.getNat

/-- Run a challenge with animation in the infoview. -/
def challenge (challengeIdx : Nat) [Strategy] (seed : Nat := 42) : CoreM Html := do
  if challengeIdx < 1 || challengeIdx > challenges.size then
    return .text s!"No challenge {challengeIdx}. Choose 1–{challenges.size}."
  let speed ← playSpeed.get
  let idx := challengeIdx - 1
  let challenge := challenges[idx]!
  let config := challenge.config
  let dtMax : Float := 1.0 / 60.0
  let duration : Float := match challenge.condition with
    | .transportWithinTime _ limit => limit
    | .transportWithinTimeAndMaxWait _ limit _ => limit
    | .transportWithMaxWait count _ => max 120.0 (count.toFloat / config.spawnRate * 2.0)
    | .transportWithinMoves count _ => max 120.0 (count.toFloat / config.spawnRate * 2.0)
    | .demo => 120.0
  let totalSteps := (duration / dtMax).toUInt64.toNat
  -- At speed 1: 6 substeps/frame × (1/60)s/substep = 0.1 game seconds/frame.
  -- Sleep 100ms between frames → 1 game second = 1 wall second.
  -- At speed N: same substeps, sleep 100/N ms.
  let stepsPerFrame : Nat := 6
  let sleepMs : Nat := max 16 (100 / speed)
  mkRefreshComponentM (.text "Starting simulation...") fun token => do
    let mut w := Sim.initWorld config seed
    let (w', initEvents) := w.elevators.foldl
      (fun (acc : Sim.World × List GameEvent) (e : Sim.Elevator) =>
        let (w, events) := acc
        let (e', evs) := Sim.checkDestinationQueue e
        let w := { w with elevators := w.elevators.set! e.index e' }
        (w, events ++ evs))
      (w, [])
    w := w'
    let view := Sim.gameView w
    let initCmds := initEvents.foldl
      (fun acc ev => acc ++ Sim.dispatchEvent view ev) []
    w := Sim.applyCommands w initCmds
    token.update (renderHtml w config (some (idx, challenge)))
    let mut step := 0
    let mut finished := false
    while step < totalSteps && !finished do
      Core.checkSystem "play"
      w := Sim.substep dtMax config w
      step := step + 1
      let result := challenge.condition.evaluate
        w.transportedCounter w.elapsedTime w.maxWaitTime w.moveCount
      if result.isSome then
        finished := true
      if step % stepsPerFrame == 0 || finished then
        token.update (renderHtml w config (some (idx, challenge)))
        if !finished then IO.sleep sleepMs.toUInt32
    if !finished then
      w := { w with elapsedTime := max w.elapsedTime duration }
      token.update (renderHtml w config (some (idx, challenge)))

/-- `#play N` — run challenge N in the infoview. -/
scoped syntax (name := playCmd) "#play " num : command

@[command_elab playCmd]
unsafe def elabPlayCmd : CommandElab := fun
  | stx@`(#play $n) => do
    let htX ← liftTermElabM <|
      HtmlCommand.evalCommandMHtml <| ← ``(HtmlEval.eval (challenge $n))
    let ht ← htX
    liftCoreM <| Widget.savePanelWidgetInfo
      (hash HtmlDisplayPanel.javascript)
      (return json% { html: $(← rpcEncode ht) })
      stx
  | _ => throwUnsupportedSyntax

end EleanvatorSaga
