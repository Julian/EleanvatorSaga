import EleanvatorSaga.Sim.World
import EleanvatorSaga.Challenge
import ProofWidgets.Data.Html

/-! # SVG renderer — graphical building view for the infoview -/

namespace EleanvatorSaga

open Sim ProofWidgets Lean

-- ══════════════════════════════════════════════════════════════════════
-- Layout constants
-- ══════════════════════════════════════════════════════════════════════

private def floorH : Float := 50.0
private def elevGap : Float := 6.0
private def leftM : Float := 60.0
private def rightM : Float := 100.0
private def carH : Float := 46.0

private def elevWForCapacity (maxCap : Nat) : Float := max 48.0 (maxCap.toFloat * 10.0)

private def totalW (numE : Nat) (ew : Float) : Float := leftM + numE.toFloat * (ew + elevGap) + rightM
private def totalH (numF : Nat) : Float := numF.toFloat * floorH + 30.0
private def floorY' (numF fi : Nat) : Float := 30.0 + (numF - 1 - fi).toFloat * floorH
private def shaftX' (i : Nat) (ew : Float) : Float := leftM + i.toFloat * (ew + elevGap)

private def carYFromElev (e : Sim.Elevator) (numF : Nat) : Float :=
  let f := e.getExactCurrentFloor
  30.0 + (numF.toFloat - 1.0 - f) * floorH + (floorH - carH) / 2.0

-- ══════════════════════════════════════════════════════════════════════
-- Html SVG helpers — terse builders for SVG elements
-- ══════════════════════════════════════════════════════════════════════

private abbrev A := Array (String × Json)

private def rect (x y w h : Float) (fill : String) (extra : A := #[]) : Html :=
  .element "rect" (#[("x", .str s!"{x}"), ("y", .str s!"{y}"),
    ("width", .str s!"{w}"), ("height", .str s!"{h}"),
    ("fill", .str fill)] ++ extra) #[]

private def line' (x1 y1 x2 y2 : Float) (stroke : String) (sw : Float := 1) : Html :=
  .element "line" #[("x1", .str s!"{x1}"), ("y1", .str s!"{y1}"),
    ("x2", .str s!"{x2}"), ("y2", .str s!"{y2}"),
    ("stroke", .str stroke), ("stroke-width", .str s!"{sw}")] #[]

private def text' (x y : Float) (sz : Float) (fill : String) (t : String)
    (extra : A := #[]) : Html :=
  .element "text" (#[("x", .str s!"{x}"), ("y", .str s!"{y}"),
    ("font-size", .str s!"{sz}"), ("fill", .str fill)] ++ extra) #[.text t]

private def circle' (cx cy r : Float) (fill : String) : Html :=
  .element "circle" #[("cx", .str s!"{cx}"), ("cy", .str s!"{cy}"),
    ("r", .str s!"{r}"), ("fill", .str fill)] #[]

private def path' (d : String) (stroke fill : String) (sw : Float := 1.5) : Html :=
  .element "path" #[("d", .str d), ("stroke", .str stroke),
    ("fill", .str fill), ("stroke-width", .str s!"{sw}"),
    ("stroke-linecap", .str "round")] #[]

private def group (tx ty : Float) (scale : Float) (children : Array Html) : Html :=
  .element "g" #[("transform", .str s!"translate({tx},{ty}) scale({scale})")] children

-- ══════════════════════════════════════════════════════════════════════
-- Passenger figures
-- ══════════════════════════════════════════════════════════════════════

private def maleFigure (color : String) : Array Html :=
  #[circle' 5 3 2.5 color,
    path' "M5,6 L5,14 M1.5,9 L8.5,9 M5,14 L2,20 M5,14 L8,20" color "none"]

private def femaleFigure (color : String) : Array Html :=
  #[circle' 5 3 2.5 color,
    path' "M5,6 L2.5,16 L7.5,16 Z" color color 1.0,
    path' "M1.5,9 L8.5,9 M3,16 L3,20 M7,16 L7,20" color "none"]

private def childFigure (color : String) : Array Html :=
  #[circle' 4 3 2.5 color,
    path' "M4,6 L4,10 M1.5,8 L6.5,8 M4,10 L2,14 M4,10 L6,14" color "none"]

private def passengerFigure (dt : DisplayType) (color : String) : Array Html :=
  match dt with
  | .male => maleFigure color
  | .female => femaleFigure color
  | .child => childFigure color

-- ══════════════════════════════════════════════════════════════════════
-- Main SVG render
-- ══════════════════════════════════════════════════════════════════════

/-- Produce the game view as an `Html` SVG element. -/
def renderSvgHtml (w : World) (config : ChallengeConfig)
    (challenge? : Option (Nat × Challenge) := none) : Html := Id.run do
  let numF := config.floorCount
  let numE := config.elevatorCount
  let maxCap := w.elevators.foldl (fun m (e : Sim.Elevator) => max m e.maxUsers) 4
  let elevW := elevWForCapacity maxCap
  let width := totalW numE elevW
  let height := totalH numF
  let mut els : Array Html := #[]

  -- Gradient definitions
  let defs := Html.element "defs" #[] #[
    .element "linearGradient" #[("id", .str "floorGrad"), ("x1", .str "0"),
      ("y1", .str "0"), ("x2", .str "0"), ("y2", .str "1")] #[
      .element "stop" #[("offset", .str "0%"),
        ("stop-color", .str "rgba(255,255,255,0.1)")] #[],
      .element "stop" #[("offset", .str "50%"),
        ("stop-color", .str "rgba(255,255,255,0.22)")] #[],
      .element "stop" #[("offset", .str "100%"),
        ("stop-color", .str "rgba(255,255,255,0.1)")] #[]]]
  els := els.push defs

  -- Background
  els := els.push (rect 0 0 width height "#2a2a2a")

  -- Stats bar
  let elapsed := w.elapsedTime.toUInt64.toNat
  let mwW := w.maxWaitTime.toUInt64.toNat
  let goalStr := match challenge? with
    | some (_, ch) => match ch.condition with
      | .transportWithinTime count _ => s!"/{count}"
      | .transportWithMaxWait count _ => s!"/{count}"
      | .transportWithinTimeAndMaxWait count _ _ => s!"/{count}"
      | .transportWithinMoves count _ => s!"/{count}"
      | _ => ""
    | none => ""
  els := els.push (text' 5 18 12 "#999"
    s!"{elapsed}s  {w.transportedCounter}{goalStr} transported  wait {mwW}s  {w.moveCount} moves"
    #[("font-family", .str "monospace")])

  -- Floors
  for fi in List.range numF do
    let fy := floorY' numF fi
    els := els.push (rect 0 fy width floorH "url(#floorGrad)")
    els := els.push (line' 0 (fy + floorH) width (fy + floorH) "#555")
    -- Floor number
    els := els.push (text' 12 (fy + 35) 28 "rgba(255,255,255,0.15)" s!"{fi}"
      #[("font-family", .str "sans-serif")])
    -- Floor call buttons (circle with arrow, like original)
    let floor := w.floors[fi]!
    let btnX : Float := 48.0
    if fi < numF - 1 then
      let c := if floor.buttonUp then "#33ff44" else "rgba(255,255,255,0.4)"
      let cy' := fy + 18
      els := els.push (.element "circle" #[("cx", .str s!"{btnX}"), ("cy", .str s!"{cy'}"),
        ("r", .str "6"), ("fill", .str "none"), ("stroke", .str c), ("stroke-width", .str "1.5")] #[])
      -- Up arrow as SVG path
      els := els.push (path' s!"M{btnX},{cy' - 4} L{btnX - 3},{cy' + 2} L{btnX + 3},{cy' + 2} Z" c c 0.5)
    if fi > 0 then
      let c := if floor.buttonDown then "#33ff44" else "rgba(255,255,255,0.4)"
      let cy' := fy + 38
      els := els.push (.element "circle" #[("cx", .str s!"{btnX}"), ("cy", .str s!"{cy'}"),
        ("r", .str "6"), ("fill", .str "none"), ("stroke", .str c), ("stroke-width", .str "1.5")] #[])
      -- Down arrow as SVG path
      els := els.push (path' s!"M{btnX},{cy' + 4} L{btnX - 3},{cy' - 2} L{btnX + 3},{cy' - 2} Z" c c 0.5)

  -- Building frame
  let buildingLeft := leftM - 4.0
  let buildingTop : Float := 26.0
  let buildingW := numE.toFloat * (elevW + elevGap) + 8.0
  let buildingH := numF.toFloat * floorH + 8.0
  els := els.push (.element "rect" #[
    ("x", .str s!"{buildingLeft}"), ("y", .str s!"{buildingTop}"),
    ("width", .str s!"{buildingW}"), ("height", .str s!"{buildingH}"),
    ("fill", .str "none"), ("stroke", .str "#555"),
    ("stroke-width", .str "2"), ("rx", .str "3")] #[])

  -- Shafts
  for i in List.range numE do
    let sx := shaftX' i elevW
    let sh := numF.toFloat * floorH
    els := els.push (rect sx 30 elevW sh "#222" #[("opacity", .str "0.5")])
    els := els.push (line' sx 30 sx (30 + sh) "#444")
    els := els.push (line' (sx + elevW) 30 (sx + elevW) (30 + sh) "#444")
    -- Floor landing markers
    for fi in List.range numF do
      let fy := floorY' numF fi + floorH
      els := els.push (line' sx fy (sx + 4) fy "rgba(255,255,255,0.15)" 1)
      els := els.push (line' (sx + elevW - 4) fy (sx + elevW) fy "rgba(255,255,255,0.15)" 1)

  -- Cars
  for e in w.elevators.toList do
    let sx := shaftX' e.index elevW
    let cy := carYFromElev e numF
    -- Cable (from top of shaft to top of car)
    let cableCx := sx + elevW / 2.0
    els := els.push (line' cableCx 30 cableCx cy "#666" 1)
    -- Car body
    els := els.push (.element "rect" #[
      ("x", .str s!"{sx + 2}"), ("y", .str s!"{cy}"),
      ("width", .str s!"{elevW - 4}"), ("height", .str s!"{carH}"),
      ("fill", .str "#4F8686"), ("stroke", .str "white"),
      ("stroke-width", .str "2"), ("rx", .str "2")] #[])
    -- Door lines (center split)
    let doorCx := sx + elevW / 2.0
    els := els.push (line' doorCx (cy + 14) doorCx (cy + carH - 3) "rgba(255,255,255,0.3)" 1)
    els := els.push (line' (doorCx - 1) (cy + 14) (doorCx - 1) (cy + carH - 3) "rgba(255,255,255,0.12)" 0.5)
    els := els.push (line' (doorCx + 1) (cy + 14) (doorCx + 1) (cy + carH - 3) "rgba(255,255,255,0.12)" 0.5)
    -- Direction indicators + floor number
    let upC := if e.goingUpIndicator then "#33ff44" else "rgba(255,255,255,0.2)"
    let dnC := if e.goingDownIndicator then "#33ff44" else "rgba(255,255,255,0.2)"
    -- Up indicator (small triangle, top-left of car)
    let uIx := sx + 8
    let uIy := cy + 7
    els := els.push (path' s!"M{uIx},{uIy} L{uIx - 3},{uIy + 5} L{uIx + 3},{uIy + 5} Z" upC upC 0.5)
    -- Down indicator (small triangle, top-right of car)
    let dIx := sx + elevW - 8
    let dIy := cy + 12
    els := els.push (path' s!"M{dIx},{dIy} L{dIx - 3},{dIy - 5} L{dIx + 3},{dIy - 5} Z" dnC dnC 0.5)
    els := els.push (text' (sx + elevW / 2) (cy + 11) 8 "#ccc" s!"{e.getRoundedCurrentFloor}"
      #[("text-anchor", .str "middle"), ("font-family", .str "monospace")])
    -- Pressed floor buttons (small numbers along bottom of car)
    let pressed := e.pressedFloors
    if !pressed.isEmpty then
      let btnY := cy + carH - 4
      for idx in List.range pressed.length do
        let fl := pressed[idx]!
        let btnX := sx + 5 + idx.toFloat * 8.0
        els := els.push (text' btnX btnY 6 "#33ff44" s!"{fl}"
          #[("font-family", .str "monospace")])
    -- Passengers inside elevator
    let insidePassengers := w.passengers.filter (fun (p : Passenger) => p.inElevator == some e.index)
    let pCount := insidePassengers.size
    if pCount > 0 then
      let maxInside := min pCount 4
      let figStartX := sx + 5
      let figY := cy + 16
      for k in List.range maxInside do
        let ip := insidePassengers[k]!
        let figX := figStartX + k.toFloat * 9.5
        els := els.push (group figX figY 0.55 (passengerFigure ip.displayType "#ffffff"))
      if pCount > maxInside then
        els := els.push (text' (figStartX + maxInside.toFloat * 9.5 + 2) (cy + 30) 7 "#ccc"
          s!"+{pCount - maxInside}"
          #[("font-family", .str "monospace")])
      -- Capacity indicator (bottom right)
      els := els.push (text' (sx + elevW - 5) (cy + carH - 3) 7 "rgba(255,255,255,0.5)"
        s!"{pCount}/{e.maxUsers}"
        #[("text-anchor", .str "end"), ("font-family", .str "monospace")])

  -- Waiting passengers
  for fi in List.range numF do
    let waiting := w.passengers.filter fun (p : Passenger) =>
      p.inElevator.isNone && !p.done && p.currentFloor == fi
    let fy := floorY' numF fi
    let startX := leftM + numE.toFloat * (elevW + elevGap) + 4.0
    let maxShow := min waiting.size 6
    for j in List.range maxShow do
      let p := waiting[j]!
      let px := startX + j.toFloat * 12.0
      let py := fy + 10.0
      let scale := match p.displayType with
        | .child => 0.7
        | _ => 0.85
      els := els.push (group px py scale (passengerFigure p.displayType "#ffffff"))
    if waiting.size > maxShow then
      els := els.push (text' (startX + maxShow.toFloat * 12.0 + 2) (fy + 36) 9 "#999"
        s!"+{waiting.size - maxShow}")

  -- Result overlay
  if let some (_, ch) := challenge? then
    let result := ch.condition.evaluate w.transportedCounter w.elapsedTime w.maxWaitTime w.moveCount
    match result with
    | some true =>
      -- Full-screen semi-transparent overlay
      els := els.push (rect 0 0 width height "rgba(0,0,0,0.5)")
      -- Centered panel
      let panelH : Float := 60.0
      let panelY := height / 2.0 - panelH / 2.0
      els := els.push (rect 0 panelY width panelH "rgba(0,80,0,0.9)")
      els := els.push (text' (width / 2) (panelY + 24) 20 "#33ff44" "Success!"
        #[("text-anchor", .str "middle"), ("font-weight", .str "bold"),
          ("font-family", .str "sans-serif")])
      els := els.push (text' (width / 2) (panelY + 46) 12 "#aaffaa"
        s!"Challenge completed — {w.transportedCounter} transported in {elapsed}s"
        #[("text-anchor", .str "middle"), ("font-family", .str "sans-serif")])
    | some false =>
      els := els.push (rect 0 0 width height "rgba(0,0,0,0.5)")
      let panelH : Float := 60.0
      let panelY := height / 2.0 - panelH / 2.0
      els := els.push (rect 0 panelY width panelH "rgba(80,0,0,0.9)")
      els := els.push (text' (width / 2) (panelY + 24) 20 "#ff4444" "Challenge Failed"
        #[("text-anchor", .str "middle"), ("font-weight", .str "bold"),
          ("font-family", .str "sans-serif")])
      let reason := match ch.condition with
        | .transportWithinTime count limit =>
          if w.transportedCounter < count then s!"Only transported {w.transportedCounter}/{count}"
          else s!"Ran out of time ({elapsed}s/{limit.toUInt64.toNat}s)"
        | .transportWithMaxWait _ maxWait =>
          if w.maxWaitTime > maxWait then s!"Max wait {mwW}s exceeded {maxWait.toUInt64.toNat}s limit"
          else s!"Not enough transported"
        | .transportWithinTimeAndMaxWait _ _ maxWait =>
          if w.maxWaitTime > maxWait then s!"Max wait {mwW}s exceeded {maxWait.toUInt64.toNat}s limit"
          else s!"Ran out of time"
        | .transportWithinMoves count limit =>
          if w.moveCount > limit then s!"Used {w.moveCount} moves (max {limit})"
          else s!"Only transported {w.transportedCounter}/{count}"
        | .demo => "Demo ended"
      els := els.push (text' (width / 2) (panelY + 46) 12 "#ffaaaa" reason
        #[("text-anchor", .str "middle"), ("font-family", .str "sans-serif")])
    | none => pure ()

  .element "svg"
    #[("xmlns", .str "http://www.w3.org/2000/svg"),
      ("viewBox", .str s!"0 0 {width} {height}"),
      ("width", .str s!"{width}"),
      ("height", .str s!"{height}")]
    els

end EleanvatorSaga
