open WorldObject
open WorldObjectI
open Ageable

(* ### Part 3 Actions ### *)
let next_pollen_id = ref 0
let get_next_pollen_id () =
  let p = !next_pollen_id in incr next_pollen_id ; p

(* ### Part 3 Actions ### *)
let max_pollen = 5
let produce_pollen_probability = 50
let bloom_probability = 4000
let forfeit_pollen_probability = 3

(* ### Part 4 Aging ### *)
let flower_lifetime = 2000

(* Flowers produce pollen.  They will also eventually die if they are not cross
    pollenated. *)
class flower p pollen_id : Ageable.ageable_t =
object (self)
  inherit CarbonBased.carbon_based 
            p None (World.rand flower_lifetime) flower_lifetime as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  val mutable pollen = World.rand max_pollen

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler World.action_event (fun () -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  method private do_action =
    Helpers.with_inv_probability World.rand produce_pollen_probability
      begin fun () ->
        pollen <- min max_pollen (pollen + 1)
      end ;
    Helpers.with_inv_probability World.rand bloom_probability
      begin fun () ->
        World.spawn 1 self#get_pos (fun p -> ignore (new flower p pollen_id))
      end


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "flower"

  method draw_picture = super#draw_circle (Graphics.rgb 255 150 255) 
                Graphics.black (string_of_int pollen)

  method smells_like_pollen = if pollen = 0 then None else Some pollen_id

  method forfeit_pollen =
    let result = ref None in
    if pollen > 0 then
      Helpers.with_inv_probability World.rand forfeit_pollen_probability
        begin fun () ->
          pollen <- pollen - 1 ;
          result := Some pollen_id
        end ;
    !result


  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* reset life if we receive pollen with a new pollen_id *)
  method private receive_pollen lst = 
    if List.fold_left (fun acc e -> acc || (e <> pollen_id)) false lst
    then (self#reset_life; lst)
    else lst

end
