module type board = sig
  (** Represents the game board which consists of tiles *)
  type 'a t

  (** [create x] is a board of [x] tiles*)
  val create : int -> 'a t

  (** [to_tile x] is the tile at location x*)
  val to_tile : int -> 'a

end