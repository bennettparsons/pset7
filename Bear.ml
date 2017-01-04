open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let pollen_theft_amount = 1000

(* ### Part 4 Aging ### *)
let bear_starting_life = 20

(* ### Part 2 Movement ### *)
let bear_inverse_speed = Some 10

class bear p hive home : Movable.movable =
object (self)
  inherit Movable.movable p bear_inverse_speed as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable stolen_honey = 0

  val mutable life = bear_starting_life

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* helper function: like forfeit_pollen but private so bees don't steal *)
  method private store_pollen = 
    let temp = stolen_honey in stolen_honey <- 0; temp

  method private do_action : unit =
    (* bear can steal honey from the hive *)
    if self#get_pos = hive#get_pos then begin
      let amt = hive#forfeit_honey pollen_theft_amount (self:>world_object_i)in
      stolen_honey <- stolen_honey + amt
    end;
    (* if bear is on cave with honey, store honey and decide what to do next *)
    if (self#get_pos = home#get_pos) && stolen_honey > 0
    then 
      (ignore (home#receive_pollen (Helpers.replicate self#store_pollen 0));
      if hive#get_pollen < (pollen_theft_amount / 2) then self#die)
    
  
  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "bear"

  method draw = super#draw_circle (Graphics.rgb 170 130 110) 
                Graphics.black (string_of_int stolen_honey)

  method draw_z_axis = 3

  method receive_sting : unit =
    life <- life - 1; if life = 0 then self#die else ()

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  method next_direction = 
    if stolen_honey = 0 
    then World.direction_from_to self#get_pos hive#get_pos
    else World.direction_from_to self#get_pos home#get_pos

end
