open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let cow_inverse_speed = Some 1

(* ### Part 6 Custom Events ### *)
let max_consumed_objects = 100

(** Cows will graze across the field until it has consumed a satisfactory number
    of flowers *)
class cow p hive home : Movable.movable =
object (self)
  inherit Movable.movable p cow_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### Part 3 Actions ### *)
  val mutable consumed_objects = 0


  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* helper method, indicates cows hunger *)
  method private hungry () = consumed_objects < max_consumed_objects

  method private do_action =
  (* if consumed_objects < max_consumed_objects then *)
    if self#hungry () then
      (let neighbors = World.get self#get_pos in
      List.iter begin fun o -> match o#smells_like_pollen with
        | Some _ -> print_string "*nom* " ; flush_all () ;
          o#die ;
          consumed_objects <- consumed_objects + 1
        | None -> ()
      end neighbors)
    (* go "in" pasture *)
    else if self#get_pos = home#get_pos then self#die


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "cow"

  method draw = super#draw_circle (Graphics.rgb 180 140 255) 
                Graphics.white (string_of_int consumed_objects)

  method draw_z_axis = 4

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* walk aroung eating things; when no longer hungry, go to pasture *)
  method next_direction = 
    if self#hungry () then
      Helpers.with_inv_probability_or World.rand (World.size / 2) 
        (fun () -> World.direction_from_to self#get_pos hive#get_pos)
        (fun () -> Some (Direction.random World.rand))
    else World.direction_from_to self#get_pos home#get_pos

end
