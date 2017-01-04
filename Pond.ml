open WorldObject
open WorldObjectI

(** Ponds serve as obstruction for other world objects. *)
class pond p : world_object_i =
object (self)
  inherit world_object p as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  method get_name = "pond"

  method draw = super#draw_circle Graphics.blue Graphics.blue ""

  method is_obstacle = true


end
