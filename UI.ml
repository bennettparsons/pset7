open WEvent

(********************)
(***** UI State *****)
(********************)

let delay = ref 10
let counter = ref 0
let inc_counter () =
  incr counter ;
  if !counter >= !delay then (counter := 0 ; true) else false

let mouse_state = ref false
let mouse_pos = ref (0,0)

let paused = ref false

(*********************)
(***** UI Events *****)
(*********************)

(** Fires when a key is pressed and returns the character corresponding
    to the key. *)
let key_pressed : char WEvent.event = WEvent.new_event ()

(** Fires when the mouse button is pressed, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_down : (int*int) WEvent.event = WEvent.new_event ()

(** Fires when the mouse button is released, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_up : (int * int) WEvent.event = WEvent.new_event ()

(** Fires when the mouse moves, indicating the coordinates where the
    mouse was when the event occured. *)
let mouse_motion : (int * int) WEvent.event = WEvent.new_event ()

(** Fires each time the virtual clock ticks. *)
let clock : unit WEvent.event = WEvent.new_event ()

(************************)
(***** Event System *****)
(************************)

exception Stop

(* poll the Graphics module for the various events -- some care had to
   be taken to "de-bounce" the mouse. *)
let read_event () =
  let new_pos = Graphics.mouse_pos () in
  if new_pos <> !mouse_pos then begin
    mouse_pos := new_pos ;
    WEvent.fire_event mouse_motion (Graphics.mouse_pos ())
  end ;
  if Graphics.key_pressed () then begin
    WEvent.fire_event key_pressed (Graphics.read_key ())
  end ;
  if not !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Poll] in
    if s.Graphics.button then begin
      mouse_state := true ;
      WEvent.fire_event button_down new_pos
    end
  end ;
  if !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_up ; Graphics.Poll] in
    if not s.Graphics.button then begin
      mouse_state := false ;
      WEvent.fire_event button_up new_pos
    end
  end ;
  WEvent.fire_event clock ()

(** Our basic event loop consists of polling for the various events,
    firing those events (which typically re-draws some graphical elements)
    and then invokes Graphics.synchronize to flip the drawing buffer and
    display the new frame. *)
let rec event_loop () = read_event () ; Graphics.synchronize () ; event_loop ()

(** The command "run_ui x y init" starts up the graphical environment with a
    window size of x by y pixels, sets up the basic events such as the
    keyboard, mouse, etc. (see below), and then invokes the function init as
    an initializer, before entering an event polling loop which fires the
    appropriate event handlers whenever an action occurs. *)
let run_ui (x:int) (y:int) (init:unit->unit) : unit =
  try
    Graphics.open_graph "" ; Graphics.resize_window x y ;
    Graphics.auto_synchronize false ;
    init () ;
    event_loop ()
  with exn -> (Graphics.close_graph () ; raise exn)

(** only call the supplied function on every delay clock ticks and only if the
    simulation is not paused. *)
let clock_handler (f : unit -> unit) () : unit =
  if inc_counter () && not !paused then f ()

(** Press q or Q to stop the simulation.
    Press space to [un]pause the simulation.
    Press f or F to make the simulation go faster.
    Press s or S to make the simulation go slower. *)
let key_handler c =
  match c with
    | 'q' | 'Q' -> raise Stop
    | ' ' -> paused := not(!paused)
    | 'f' | 'F' -> delay := (!delay) - 5
    | 's' | 'S' -> delay := (!delay) + 5
    | _ -> ()

(** Start the graphical environment initialized to the size of the world.
    Handle clock and input events necessary to run the simulation. *)
let run_world (init:unit -> unit) (clock_f:unit -> unit) : unit =
  run_ui (World.size*World.obj_width) (* GUI width *)
         (World.size*World.obj_height) (* GUI height *)
         (* Event framework initializer *)
         begin fun () ->
           World.reset () ;
           ignore(WEvent.add_listener clock (clock_handler clock_f)) ;
           ignore(WEvent.add_listener key_pressed key_handler) ;
           init ()
         end
