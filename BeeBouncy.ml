open WorldObject
open WorldObjectI
open Bee
open Direction

(** Bouncy bees will travel in a straight line in a random direction until an
    obstacle or edge of the world is reached, at which point a new random
    direction will be chosen. *)
class bee_bouncy p hive : bee_t =
object (self)
  inherit bee p hive as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable curr_direction = Some (random World.rand)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "bee_bouncy"

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* move straight unless obstructed; if so, find new random direction *)
  method private next_direction_default = 
    let rec gen_new_d () = 
      if World.can_move (move_point self#get_pos curr_direction) 
      then curr_direction 
      else (curr_direction <- Some (random World.rand); gen_new_d ()) in
      gen_new_d ()



end


