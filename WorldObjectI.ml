open WEvent

(** world_object_i is the super type of all objects in the world. Note that
    world_object_i is defined with "class type" making it an interface. *)
class type world_object_i =
object
  (** The name of the object. get_name is used for displaying purposes. *)
  method get_name : string

  (** The position of the object.  This is the primary way to tell if one
      object is on the same square as another *)
  method get_pos : int*int

  (** How to draw the object. *)
  method draw : unit

  (** The z-axis used for drawing the object. Objects with higher z-axis will be
   * drawn on top of objects with lower z-axis. *)
  method draw_z_axis : int

  (** If this object is an obstacle or not. Objects are unable to move into
   * squares which contain obstacles *)
  method is_obstacle : bool

  (** If this object smells like pollen. *)
  method smells_like_pollen : int option

  (** Forfeit pollen to another object. If successful, return a pollen
      identifier.  If an object doesn't smell like pollen then it should never
      respond successfully to extract_pollen. Likewise, if extract_pollen has
      the potential to return a pollen identifier then it should smell like
      pollen. *)
  method forfeit_pollen : int option

  (** Receive pollen from another object. Return a list of remaining pollen that
      the sender may retain. *)
  method receive_pollen : int list -> int list

  (** This object has been stung! *)
  method receive_sting : unit

  (******************)
  (***** Events *****)
  (******************)

  (** This event is called when the object dies. *)
  method get_die_event : unit WEvent.event

  (** Kill the object. *)
  method die : unit

  (** This event is called when the object is in danger. *)
  method get_danger_event : world_object_i WEvent.event

  (** Signal that this object is in danger. *)
  method danger : world_object_i -> unit

  (** Register a handler with an event.  This handler will be removed if this
      object dies. *)
  method private register_handler : 'a. 'a WEvent.event -> ('a -> unit) -> unit

  (** A helper function exported to subclasses of world_object.  Moves the
      object in the specified direction (None represents staying put). *)
  method private move : Direction.direction option -> unit

  (** Draw a circle with background-color, foreground-color, and text at this
      object's location. See Draw.draw_circle for more info. *)
  method private draw_circle : Graphics.color -> Graphics.color -> string -> unit

  (** Draw a status bar with color and full-amount where 0 <= full-amount <= 1.
      See Draw.status_bar for more info. *)
  method private draw_status_bar : Graphics.color -> float -> unit


end
