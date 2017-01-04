open Helpers
open WorldObject
open WorldObjectI
open WEvent

(** Class type for objects which constantly try to move in a calculated next
    direction. *)
class type movable_t =
object
  inherit world_object_i

  (** The next direction for which this object should move. *)
  method next_direction : Direction.direction option
end

class movable p (inv_speed:int option) : movable_t =
object (self)
  inherit world_object p as super

  (***********************)
  (***** Initializer *****)
  (***********************)

  initializer
    self#register_handler 
      (WEvent.buffer (match inv_speed with
       | None -> max_int
       | Some i -> i) World.move_event)
      self#do_move

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  method private do_move = fun _ -> self#move (self#next_direction)

  (***************************)
  (***** Movable Methods *****)
  (***************************)

  method next_direction = None

end
