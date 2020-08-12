(** [add_pos p1 p2] is [p1] + [p2] element-wise. I.e., if p1 = (x1,y1) and p2 =
 * (x2,y2), then [add_pos p1 p2] = (x1 + x2, y1 + y2).
 *)
val add_pos : int*int -> int*int -> int*int
