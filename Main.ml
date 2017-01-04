open WEvent
open WorldObjectI

(* Generating ponds *)
let num_ponds = 12
let pond_size = 15
let gen_ponds () : unit =
  World.spawn_iter num_ponds pond_size
                   (fun () -> ())
                   (fun p -> ignore (new Pond.pond p))

(* Generating flowers *)
let num_flowers = 20
let flower_size = 20
let gen_flowers () : unit =
  let pollen_id = ref (-1) in
  World.spawn_iter num_flowers flower_size
                   (fun () -> pollen_id := Flower.get_next_pollen_id ())
                   (fun p -> ignore (new Flower.flower p  !pollen_id))

(* Do not ignore, since we will need to pass the hive to some other objects. *)
let gen_cave hive =
  new Cave.cave (0,0) hive

(* Do not ignore, since we will need to pass the hive to some other objects. *)
let gen_pasture hive =
  new Pasture.pasture (World.size-1,World.size-1) hive

let gen_bear hive home =
  ignore (new Bear.bear (0,0) hive home)

let gen_cow hive home =
  ignore (new Cow.cow (World.size-1,World.size-1) hive home)

let gen_hive () =
  (* Do not ignore, since we will need to pass the hive to some other objects. *)
  new Hive.hive (World.size/2,World.size/2)

(* Initializer functions *)
let part1_initializer () : unit =
  ignore (new Pond.pond (0,0)) ;
  ignore (new Flower.flower (1,1) 0) ;
  let hive = new Hive.hive (2,2) in
  ignore (new Bee.bee (3,3) (hive :> world_object_i)) ;
  let cave = new Cave.cave (4,4) hive in
  ignore (new Bear.bear (5,5) hive cave) ;
  let pasture = new Pasture.pasture (6,6) hive in
  ignore (new Cow.cow (7,7) hive pasture) ;
  ignore hive ;
  ignore cave ;
  ignore pasture

let part2_initializer () : unit =
  let hive = gen_hive () in
  ignore (new Bee.bee (World.size/2+1,World.size/2) (hive :> world_object_i)) ;
  gen_bear hive hive (* dummy arg in absence of cave *);
  gen_cow hive hive (* dummy arg in absence of pasture *)

let part3_initializer () : unit =
  let hive = gen_hive () in
  let cave = gen_cave hive in
  let pasture = gen_pasture hive in
  gen_ponds () ;
  gen_flowers () ;

  for _i = 1 to 20 do
    ignore(new Bee.bee (World.size/2+1,World.size/2) (hive :> world_object_i));
  done ;

  gen_bear hive cave ;
  gen_cow hive pasture ;
  ignore hive ;
  ignore cave ;
  ignore pasture

let part4_initializer () : unit =
  ignore (gen_hive ()) ;
  gen_ponds () ;
  gen_flowers ()

let final_initializer () : unit =
  let hive = gen_hive () in
  ignore (gen_cave hive) ;
  ignore (gen_pasture hive) ;
  gen_ponds () ;
  gen_flowers ();
  ignore hive

(* Function that is called continuously while the simulation is running. *)
let event_loop part () : unit =
  Graphics.clear_graph () ;
  if part >= 2 then WEvent.fire_event World.move_event () ;
  if part >= 3 then WEvent.fire_event World.action_event () ;
  if part >= 4 then WEvent.fire_event World.age_event () ;
  (* draw loop *)
  World.indices begin fun p ->
    let sorted = List.sort (fun x y -> compare x#draw_z_axis y#draw_z_axis)
                           (World.get p)
    in
    List.iter (fun w -> w#draw) sorted
  end

(* Parse command-line arguments. Returns the appropriate initialization
  function to run and the current part. *)
let parse_args () : (unit -> unit) * int =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "part1" -> part1_initializer, 1
  | "part2" -> part2_initializer, 2
  | "part3" -> part3_initializer, 3
  | "part4" | "part5" -> part4_initializer, 4
  | "final" | "part6" -> final_initializer, 6
  | _ -> usage ()


let run () : unit =
  let initialize, part = parse_args () in
  UI.run_world initialize (event_loop part)
;;

run () ;;
