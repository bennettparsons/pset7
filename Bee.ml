open WorldObject
open WorldObjectI

(* ### Part 2 Movement ### *)
let bee_inverse_speed = Some 1

(* ### Part 3 Actions ### *)
let max_pollen_types = 5

(* ### Part 4 Aging ### *)
let bee_lifetime = 1000

(* ### Part 5 Smart Bees ### *)
let max_sensing_range = 5

class type bee_t =
object
  inherit Ageable.ageable_t

  method private next_direction_default : Direction.direction option
end

(** Bees travel the world searching for honey.  They are able to sense flowers
    within close range, and they will return to the hive once they have
    pollenated enough species of flowers. *)
class bee p (home : world_object_i) : bee_t =
object (self)
  inherit CarbonBased.carbon_based 
            p bee_inverse_speed (World.rand bee_lifetime) bee_lifetime as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable pollen = []

  val sensing_range = World.rand max_sensing_range
  val pollen_types = World.rand max_pollen_types + 1

  (* whether or not hive is in danger *)
  val mutable danger = false

  (* aggressor attacking hive; initialized to dummy value home *)
  val mutable attack = home

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action) ;
    self#register_handler home#get_danger_event (fun ob -> self#do_danger ob)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* attack the aggressor if there's danger; otherwise pollinate neighbors *)
  method private do_action : unit =
    if danger && (self#get_pos = attack#get_pos)
    then (attack#receive_sting; self#die)
    else let neighbors = World.get self#get_pos in
      List.iter self#deposit_pollen neighbors ;
      List.iter self#extract_pollen neighbors

  (* update danger and attack variables; call listener aggressor's death *)
  method private do_danger evil = 
    self#dead_bear_listener evil; danger <- true; attack <- evil

  (* listen for agressor's death *)
  method private dead_bear_listener evil = 
    self#register_handler evil#get_die_event (fun _ -> self#no_danger)

  (* reset danger and attack variables *)
  method private no_danger = danger <- false; attack <- home

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  method private deposit_pollen (o:world_object_i) : unit =
    let pollen' = o#receive_pollen pollen in
    pollen <- pollen'

  method private extract_pollen (o:world_object_i) : unit =
    match o#forfeit_pollen with
    | None -> ()
    | Some i -> pollen <- i::pollen

  method private magnet_flower : world_object_i option =
    (* find flowers in range *)
    let frange = World.objects_within_range self#get_pos sensing_range in
    (* check if their pollen is new *)
    let fnew = List.filter 
      (fun fl -> 
        match fl#smells_like_pollen with
        | None -> false
        | Some p -> not (List.mem p pollen)) 
      frange in
    match fnew with
    | [] -> None
    | _ -> 
    (* find closest element in list *)
    Some (List.fold_left 
      (fun acc fl -> 
        if (Direction.distance acc#get_pos self#get_pos) > 
           (Direction.distance fl#get_pos self#get_pos) 
        then fl else acc) 
      (List.hd fnew) fnew)

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "bee"

  method draw_picture = super#draw_circle Graphics.yellow Graphics.black 
                (string_of_int (List.length pollen))

  method draw_z_axis = 2


  (***************************)
  (***** Movable Methods *****)
  (***************************)

  (* bees movement depends on danger signal, the pollen it possesses 
   * and the presence of surrounding flowers to pollinate *)
  method next_direction = 
    if danger then World.direction_from_to self#get_pos attack#get_pos
    else if pollen_types < (List.length (Helpers.unique pollen)) 
    then World.direction_from_to self#get_pos home#get_pos
    else match self#magnet_flower with
         | None -> self#next_direction_default
         | Some fl -> World.direction_from_to self#get_pos fl#get_pos


  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* overriden in BeeBouncy.ml and BeeRandom.ml *)
  method private next_direction_default = None

end
