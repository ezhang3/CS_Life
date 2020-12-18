open Graphics


(* takes in variants and executes them*)
type action = Minigame | Quit


let rec loop () = 
  (* draw_string
     set_text_size
     draw_image
     wait_next_event returns status
     loop_at_exit could be useful for saving stuff from the minigame?
     mouse_pos, button_down, read_key, key_pressed
     hmm double buffering
  *)
  let x = (mouse_pos ()) |> fst |> string_of_int in
  let y = (mouse_pos ()) |> snd |> string_of_int in
  let event = wait_next_event [Poll] in
  if event.key == 'm' then (draw_string (x ^ ", " ^ y); loop ())
  else (clear_graph (); loop ())


(* let x =  *)
(* match x with
   | Quit -> exit 0
   | Minigame -> minigame; loop () *)

let run_gui () =
  open_graph "";
  (* draw_rect 50 50 300 200;
     set_color red;
     fill_rect 50 50 300 200; *)
  loop ()

let () = run_gui ()