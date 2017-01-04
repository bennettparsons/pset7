open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let smelly_object_limit = 200

(** A pasture will spawn a cow when there are enough objects in the world that
    smell like pollen. *)
class pasture p hive : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* spawn cow if there is no cow and enough smelly objects in the world *)
  method private do_action =
    (* objects that smell like pollen *)
    let pollen_obs = 
      World.fold (fun ob acc -> if ob#smells_like_pollen = 
                    None then acc else acc + 1) 0 in
    if (pollen_obs > smelly_object_limit) && 
       not (World.fold (fun ob acc -> acc || ob#get_name = "cow") false)
    then (ignore (new Cow.cow self#get_pos hive self); 
      print_string "mooooooooo "; flush_all ())
    else ()

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "pasture"

  method draw = super#draw_circle (Graphics.rgb 70 100 130) Graphics.white "P"


end

