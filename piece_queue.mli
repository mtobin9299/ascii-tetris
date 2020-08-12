(** The abstract data type representign a queue that holds upcoming tetrominos
 *)
type t

(** [init depth] is a fresh piece queue that holds [depth] number of upcoming
 * pieces.
 *)
val init : int -> t

(** [next_piece queue] is the next piece in [queue]. This function modifies the
 * queue mutably by popping the next piece off the [queue].
 *)
val next_piece : t -> Tetromino.t

(** [to_list queue] is the [queue] as a list. Order is preserved. *)
val to_list : t -> Tetromino.t list 
