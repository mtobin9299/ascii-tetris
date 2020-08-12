(** Abstract board type *)
type t

(** Type synonym for [Tetromino.piece_type] *)
type mino = Tetromino.piece_type

(** Type representing integer coordinates *)
type pos = (int * int)

(** Type for [line_clear] containing information about what lines were cleared *)
type line_clear =
{
  lines : int list;
}

(** [init width height] is an empty board with width equal to [width] and height
 * = [height] + 3. Note: [height] is the visible height of the board, the actual
 * height is [height] +3
 *)
val init : int -> int -> t

(** [get_width board] is the width of the board *)
val get_width : t -> int

(** [get_height board] is the height of the board. Note: this is not equal to
 * the height used to create the board but rather the extended height of the
 * board. [get_height board] = [h] + 3 where [h] is the height used to
 * initialize the board. *)
val get_height : t -> int

(** [get_visible_height board] is the visible height of the board.
 * [get_visible_height] is equal to the height of the board used to initialize
 * it.
 *)
val get_visible_height : t -> int

(** [has_mino board pos] is true if the [board] has the value [Some mino] at
 * [pos], otherwise false. Requires: [pos] is [in_bounds].
 *)
val has_mino : t -> pos -> bool 

(** [in_bounds board pos] is true if the [board] contains [pos], false otherwise
 *)
val in_bounds : t -> pos -> bool 

(** [is_valid board pos] is true if [pos] is in bounds and there is no mino
 * occupying the board at [pos], false otherwise. 
 *) 
val is_valid : t -> pos -> bool

(** [is_valid_tetromino board tetromino] is true if forall minos in [tetromino]
 * [is_valid] is true (i.e. if all mino positions map to valid locations on the
 * board).
 *)
val is_valid_tetromino : t -> Tetromino.t -> bool

(** [get_mino_at board pos] is the value of [board] at [pos].
 * Requires: [pos] is in bounds.
 *)
val get_mino_at : t -> pos -> mino option

(** [place_tetromino board tetromino] is the result of placing a [tetromino]
 * on the board. If the tetromino is placed in such a way that lines are
 * cleared, then the result of [place_tetromino] is [Some line_clear].
 * Otherwise, the result is [None].
 * Requires: [tetromino] is contained within the board.
 *)
val place_tetromino : t -> Tetromino.t -> line_clear option 
