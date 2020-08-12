(** The abstract type representing a piece generator that uses a 7-Bag
 * Randomizer.
 *)
type t

(** [init] is a new piece generator. *)
val init : unit -> t

(** [get_next_piece piece_gen] is the next randomly generated piece using a
 * 7-Bag Randomizer. *)
val get_next_piece : t -> Tetromino.piece_type
