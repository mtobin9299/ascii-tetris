(**The abstract screen type*)
type t

(**[pos] is the type that represents an x, y coord pair *)
type pos = int * int

(**[pixel_type] is the type representing the types of pixels that can
    be printed to the screen *)
type pixel_type = 
  | CharPixel of char
  | UnicodePixel of string

type pixel = pixel_type * ANSITerminal.style list

(**[init_screen w h] is a screen of width [w] and height [h]*)
val init_screen : int -> int -> t

(**[render_screen screen delta_time input st] draws the current state [st]
    of the game as well as the rest of [screen] to the terminal *)
val render_screen : t -> float -> Input.t -> Game_state.t -> unit
