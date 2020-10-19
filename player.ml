type player_name = string 

type player = {
  name: player_name; 
  points: int; 
  mutable study_partners: player list
}

let get_name p = 
  p.name

let get_points p = 
  p.points

let get_partners p = 
  p.study_partners
