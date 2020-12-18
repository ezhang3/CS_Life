(** Specialevents stores all the possible special events in the game board *)
open Playerstate
(** [find_special_event player players str] initiates the special event with id 
    [str] for the player [player]. Depending on the event, other players in 
    [players] can have their states changed*)
val find_special_event : player -> player list 
  -> Board.gameboard -> string -> unit