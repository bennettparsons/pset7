open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let spawn_bear_pollen = 500

(** A cave will spawn a bear when the hive has collected a certain amount of
    honey. *)
class cave p (hive : Hive.hive_i) : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable pollen = 0

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler hive#get_pollen_event (fun _ -> self#do_pollen_event)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* make a bear if hive has enough pollen and there isn't already a bear *)
  method private do_pollen_event =
    if (hive#get_pollen > spawn_bear_pollen) && 
       not (World.fold (fun ob acc -> acc || ob#get_name = "bear") false)
    then (ignore (new Bear.bear self#get_pos hive self); 
      print_string "omg bears! "; flush_all ())
    else ()

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "cave"

  method draw = super#draw_circle Graphics.black Graphics.white "C"

  method receive_pollen pol = pollen <- pollen + (List.length pol); pol

end
