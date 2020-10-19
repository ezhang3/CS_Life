open Tile

type st = {
  current_tile: tile_id; 
  items: string list; (** not sure *)
  rep_level: float
  (** do the effects of events do anything other than change points *)
}

let init_state p = 
  {
    current_tile = start.tile_id; (** needs some sort of start tile *)
    items = []; 
    rep_level = 0.
  }

let current_tile_id st = 
  st.current_tile

let have_items st = 
  st.items

let rep_level st = 
  st.rep_level

