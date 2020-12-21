(** Contains all GUI related components of the game, mainly GUI minigames. *)

open Graphics

(** [coffee_break_gui ()] runs the coffee break minigame and returns the
    energy gained. The player fills an empty cup with coffee using the 
    buttons "fill" and "done". The more coffee poured before pressing done,
    the more energy the player can gain. *)
val coffee_break_gui : unit -> int