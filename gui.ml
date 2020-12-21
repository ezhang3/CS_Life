open Graphics

(* -------------- Type declarations and record instantiation --------------- *)

type button = {
  x : int; y : int;
  w : int; h : int;
  label : string;
  l_color : color; l_size : int;
  b_color : color
}
let b_fill = {x= 100; y= 70;
              w= 40; h= 20;
              label= "Fill";
              l_color= black; l_size = 5;
              b_color = rgb 200 200 200}
let b_done = {x= 100; y= 50;
              w= 40; h= 20;
              label= "Done";
              l_color= black; l_size = 5;
              b_color = rgb 200 200 200}
let b_list = [b_fill; b_done]

(** maxx : maximum width of the canvas
    maxy : maximum height of the canvas
    x, y : current position
    scale : scaling factor of visualization
    bc, fc, pc: background, foreground, current point color
    buttons : list of all the buttons in the minigame
    coffee : the height of the coffee *)
type state = {maxx : int; maxy : int;
              mutable x : int; mutable y : int;
              scale:int;
              bc : color; fc: color; pc : color;
              textsize : int;
              buttons : button list;
              mutable coffee : int}

let coffee_ist = {maxx = 150; maxy = 100;
                  x = 60; y = 60; 
                  scale = 4;
                  bc = rgb 200 200 200; fc = black; pc = red;
                  textsize = 15;
                  buttons = b_list;
                  coffee = 1}

(* -------------------- Initialization procedure -------------------- *)

(** if x,y are in box x0,y0,w,h *)
let mem (x,y) (x0,y0,w,h) = 
  (x >= x0) && (x< x0+w) && (y>=y0) && ( y<y0+h)

(** [make_button label x y w h] draws a button with the words [label] on it
    at point [x],[y] with width [w] and height [h]. *)
let make_button label x y w h =
  rgb 230 230 230 |> set_color;
  fill_rect x y w h;
  set_color black;
  draw_rect x y w h;
  let wtext = label |> text_size |> fst in
  let htext = label |> text_size |> snd in
  moveto (x + w/2 - wtext/2) (y + h/2 - htext/2);
  draw_string label

let rec make_buttons s lst =
  match lst with
  | [] -> ()
  | {x = x0; y = y0; w = w; h = h; label= label} :: t ->
    make_button label (x0*s.scale) (y0*s.scale) (w*s.scale) (h*s.scale);
    make_buttons s t

let coffee_init s () = 
  open_graph "";
  resize_window (s.scale*s.maxx) (s.scale*s.maxy);
  set_color s.bc;
  fill_rect 0 0 (s.scale*s.maxx+1) (s.scale*s.maxy+1);
  (* draws coffee cup and background*)
  set_color Graphics.black;
  draw_poly_line [|(75, 225); (75, 50); (200, 50); (200, 225)|]; 
  make_buttons s s.buttons;
  moveto (s.scale*80) (s.scale*20);
  draw_string "Coffee break! ";
  moveto (s.scale*80) (s.scale*15);
  draw_string "Click Fill to pour some coffee for yourself. ";
  moveto (s.scale*80) (s.scale*10);
  draw_string "Click Done when you're done pouring. "

(* -------------------- User interaction loop -------------------- *)

let rec brange s x y (lst : button list) =
  match lst with
  | [] -> None
  | {x = x0; y = y0; w = w; h = h; label = label} :: t ->
    if mem (x, y) (x0*s.scale, y0*s.scale, w*s.scale, h*s.scale)
    then Some label else brange s x y t

let fill_up s = 
  s.coffee <- (s.coffee + 20);
  rgb 139 69 19 |> set_color ;
  fill_rect 85 60 105 s.coffee

let energy amt =
  if amt > 100 then 100 else amt

(*
BC: If keypressed is q or done is clicked then return points
Otherwise: Handle other cases and loop again
*)
let rec loop points st () : int =
  let status = wait_next_event [Button_down; Key_pressed] in 
  if status.keypressed then
    (* handles keypress *)
    match status.key with
    | 'q' -> 0
    | _ -> loop points st ()
  else if status.button then
    (* handles mouseclick*)
    match brange st status.mouse_x status.mouse_y b_list with
    | Some "Fill" -> fill_up st;
      let points = points + 10 |> energy in loop points st ()
    | Some "Done" -> points
    | Some _ | None -> loop points st ()
  else loop points st ()

(* -------------------- Close procedure -------------------- *)

let coffee_close () =
  close_graph ();
  print_string "Minigame Completed!"; print_newline ()

(** [gui f_init f_loop f_close] initializes the game gui with [f_init],
    closes the game gui with [f_close], and handles user input with
    [f_loop]. *)
let gui f_init f_loop f_close =
  f_init ();
  let result = f_loop () in
  f_close (); 
  (match result with
   | 0 ->  print_endline "You drank no coffee. No energy for you."
   | x when x <= 60 ->
     begin
       print_string "You drank some delicious coffee, fueling your caffeine addiction. ";
       print_string "You feel your weariness melt away. "
     end
   | x ->
     begin
       print_string "You drank some delicious coffee, fueling your caffeine addiction. ";
       print_endline "You're practically buzzing with energy. ";
       print_endline "With caffeine pumping through your veins, you feel like you can take on anything! " 
     end);
  print_endline ("Gained " ^ string_of_int result ^ " energy.");
  result

(* [coffee_break st] is a minigame with gui where the user
   fills an empty cup with coffee using two buttons "fill" and "done".
   The more coffee you pour before pressing done, the more energy
   the player can gain *)
let coffee_break st = gui (coffee_init st) (loop 0 st) coffee_close

let coffee_break_gui () =
  coffee_break coffee_ist

(*let points = coffee_break_gui ()*)

(* Potentially useful functions for GUI:
   draw_string
   set_text_size
   draw_image
   wait_next_event (returns status)
   loop_at_exit could be useful for saving stuff from the minigame?
   mouse_pos, button_down, read_key, key_pressed
   hmm double buffering
*)