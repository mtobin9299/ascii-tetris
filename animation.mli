(**[animation] is the type of the animation*)
type animation = Nothing
               | LineClear of Board.line_clear

(**The abstract animation type*)
type t 

(**[active_animation animation] is the specific animation held in [animation]*)
val active_animation : t -> animation
(**[elapsed animation] is how long [animation] has been active*)
val elapsed : t -> float
(**[duration animation] is how long [animation] should be active*)
val duration : t -> float
(**[pct_complete animation] is the percentage of how far into 
    [animation] is into its [duration]*)
val pct_complete : t -> float

(**[init ()] is a new, empty animation shell*)
val init : unit -> t

(**[reset_elapsed animation] is [animation] with [elapsed] set to 0*)
val reset_elapsed : t -> t

(**[tick_elapsed animation dt] is [animation] with [dt] added to its [elapsed]*)
val tick_elapsed : t -> float -> t

(**[animation_over animation] is whether or not [animation] has completed*)
val animation_over : t -> bool

(**[reset_animation animation] is [init ()]*)
val reset_animation : t -> t

(**[do_line_clear_animation animation line_clear duration] is an [animation] with
    resetted elapsed, duration [duration], and a LineClear [line_clear] animation*)
val do_line_clear_animation : t -> Board.line_clear -> float -> t

