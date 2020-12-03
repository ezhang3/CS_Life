open Graphics

let rec loop () = loop ()

let run_gui () =
  open_graph "";
  draw_rect 50 50 300 200;
  set_color red;
  fill_rect 50 50 300 200;
  loop ()

let () = run_gui ()