open ANSITerminal

type animation = Nothing
               | LineClear of Board.line_clear

type t = {
  active_animation : animation;
  elapsed : float;
  duration : float;
}

let init () = {
  active_animation = Nothing;
  elapsed = 0.;
  duration = 0.;
}

let active_animation animation = animation.active_animation
let elapsed animation = animation.elapsed
let duration animation = animation.duration
let pct_complete animation = animation.elapsed /. animation.duration

let reset_elapsed animation = 
  { animation with elapsed = 0. } 

let tick_elapsed animation dt = 
  { animation with elapsed = animation.elapsed +. dt } 

let animation_over animation = 
  animation.elapsed >= animation.duration

let reset_animation animation = 
  init ()

let do_line_clear_animation animation (line_clear : Board.line_clear) duration =
  {
    animation with
    active_animation = LineClear line_clear;
    duration;
  } |> reset_elapsed 

