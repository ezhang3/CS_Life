<<<<<<< HEAD
type player_id = string
type player = {
  player_name : player_id;
  player_state : Playerstate.state;
}
=======
type player_name = string 

type player = {
  name: player_name; 
  mutable points: int; 
  mutable study_partners: player list
}

let get_name p = 
  p.name

let get_points p = 
  p.points

let get_partners p =
  p.study_partners
>>>>>>> f4c3fb1a070fbdb670ca87f40ee16de6a86893ef
