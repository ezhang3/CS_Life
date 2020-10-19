open Event

type color = Red | Blue | Green | Yellow

type event = Event.event

type tile_id = string

type tile = {
  color : color;
  event : event;
  id : tile_id;
}

let get_tile_color tile =
  tile.color

let get_tile_event tile = 
  tile.event

let get_tile_id tile = 
  tile.id