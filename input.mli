(**The abstract input type*)
type t

(**[key_input] is the type that represents the possible key to action bindings*)
type key_input = 
  | KMoveRight
  | KMoveLeft
  | KHold
  | KRotateRight
  | KRotateLeft
  | KHardDrop
  | KQuit
  | KRestart

(**[init ()] is the intitial input state with key map derived from a json file*)
val init : unit -> t

(**[process_input input_state ch game_state] is the new game state following
    the execution of the command binded to [ch] in [input_state]*)
val process_input : t -> string -> Game_state.t -> Game_state.t

(**[has_quit input_state] is true if the player has pressed the quit key. 
   False otherwise.*)
val has_quit : t -> bool

(**[get_key key_input input_state] is the char binded to [key_input] in
   [input_state]*)
val get_key : key_input -> t -> char  
