(**The abstract state type*)
type t

(**[init ()] is the intial state of a new game *)
val init : unit -> t 

(**[board st] is the current board of [st]*)
val board : t -> Board.t

(**[active_tetromino st] is the current active tetromino of [st]. 
   None if one does not exist. *)
val active_tetromino : t -> Tetromino.t option

(**[hold_tetromino st] is the current hold tetromino  of [st]. 
   None if one does not exist. *)
val hold_tetromino : t -> Tetromino.t option

(**[piece_queue st] is the current piece queue of [st]*)
val piece_queue : t -> Piece_queue.t 

(**[score st] is the current score of [st]*)
val score : t -> int 

(**[game_over st] is whether or not the game of [st] has been won or lost*)
val game_over : t -> bool 

(**[lines_cleared st] is the number of lines that have been cleared in the
    game of [st]*)
val lines_cleared : t -> int

(**[lines_to_win st] is the number of lines that have to be cleared to win the
    game of [st]*)
val lines_to_win : t -> int

(**[level st] is the current level of the game of [st]*)
val level : t -> int

(**[get_game_status st] is the current game status of [st]*)
val get_game_status : t -> string

(**[animation st] is the current active animation of [st]*)
val animation : t -> Animation.t

(**[rotate cw st] is the state with the active tetronimo rotated clockwise
   if [cw] or counterclockwise otherwise. If rotation is not possible
   based on board state, returns [st]*)
val rotate : bool -> t -> t

(**[move right st] is the state with the active tetronimo moved right a space
   if [right] or left a space otherwise. If movement is not possible
   based on board state, returns [st]*)
val move : bool -> t -> t

(**[hard_drop st] is the state with the active tetromino of [st] dropped instantly
   and a new active tetronmino taken off the piece queue *)
val hard_drop : t -> t

(**[hold st] is the state with the current active tetronmino of [st] stored in
    [hold_tetromino] and with a new active tetromino taken from the piece queue. 
    If not possible, returns [st]. *)
val hold : t -> t

(**[update delta_time st] is the new, updated state based on [st] 
   after [delta_time] has passed*)
val update : float -> t -> t 
