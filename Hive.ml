open WEvent
open WorldObject
open WorldObjectI
open Helpers

(* ### Part 3 Actions ### *)
let starting_pollen = 500
let cost_of_bee = 10
let spawn_probability = 20
let pollen_probability = 50
let max_pollen_deposit = 3


class type hive_i =
object
  inherit world_object_i

  (* forfeit all honey to a world object *)
  method forfeit_honey : int -> WorldObjectI.world_object_i -> int

  method get_pollen_event : int WEvent.event

  method get_pollen : int
end


(** A hive will spawn bees and serve as a deposit point for the pollen that bees
    harvest.  It is possible to steal honey from a hive, however the hive will
    signal that it is in danger and its loyal bees will become angry. *)
class hive p : hive_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable pollen = starting_pollen

  val pollen_event = WEvent.new_event ()

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action)


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* increment pollen and generate bees according to random probabilities *)
  method private do_action =
    with_inv_probability World.rand pollen_probability
      begin fun () ->
        pollen <- pollen + 1
      end;
    if pollen >= cost_of_bee then
      with_inv_probability World.rand spawn_probability
        begin fun () ->
          pollen <- pollen - cost_of_bee ;
          self#generate_bee ()
        end

  (* ### TODO: Part 4 Aging ### *)

  (**************************)
  (***** Helper Methods *****)
  (**************************)

  (* generate random and bouncy bees with equal probability *)
  method private generate_bee _ = 
    let generate_bouncy _ = 
      ignore (new BeeBouncy.bee_bouncy self#get_pos (self :> world_object_i))in
    let generate_rand _ = 
      ignore (new BeeRandom.bee_random self#get_pos (self :> world_object_i))in
    with_equal_probability World.rand [generate_bouncy; generate_rand]

  (****************************)
  (*** WorldObjectI Methods ***)
  (****************************)

  method get_name = "hive"

  method draw = super#draw_circle Graphics.cyan Graphics.black 
                (string_of_int pollen)

  method receive_pollen ps =
    self#fire_pollen_event; 
    pollen <- pollen + (min (List.length ps) max_pollen_deposit);
    []

  method private fire_pollen_event = WEvent.fire_event pollen_event pollen

  (************************)
  (***** Hive Methods *****)
  (************************)

  method forfeit_honey n b =
    let stolen = min pollen n in
    pollen <- pollen - stolen ;
    self#danger b ;
    stolen

  method get_pollen_event = pollen_event
  method get_pollen = pollen

end
