open Tile

type st = {
  current_tile: tile_id; 
  items: string list; (** not sure *)
  rep_level: float 
}

let init_state p = 
  {
    current_tile = start.tile_id; (** needs some sort of start tile *)
    items = []; 
    rep_level = 0.
  }

let current_tile_id st = 
  st.current_tile

