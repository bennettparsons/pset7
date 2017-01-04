open WorldObject
open WorldObjectI
open Bee

(** Random bees will move randomly. *)
class bee_random p hive : bee_t =
object (self)
  inherit bee p hive as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "bee_random"

  (***********************)
  (***** Bee METHODS *****)
  (***********************)

  method private next_direction_default = Some (Direction.random World.rand)

end


