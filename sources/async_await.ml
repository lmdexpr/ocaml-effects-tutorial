open Printf

module type Scheduler = sig
  type 'a promise
  (** Type of promises *)
  val async : (unit -> 'a) -> 'a promise
  (** [async f] runs [f] concurrently *)
  val await : 'a promise -> 'a
  (** [await p] returns the result of the promise. *)
  val yield : unit -> unit
  (** yields control to another task *)
  val run   : (unit -> 'a) -> unit
  (** Runs the scheduler *)
end

module Scheduler : Scheduler = struct

  open Effect
  open Effect.Deep

  type 'a _promise =
    Waiting of ('a,unit) continuation list
  | Done of 'a

  type 'a promise = 'a _promise ref

  type _ Effect.t +=
    | Async : (unit -> 'a) -> 'a promise Effect.t
    | Yield : unit Effect.t
    | Await : 'a promise -> 'a Effect.t

  let async f = perform (Async f)

  let yield () = perform Yield

  let await p = perform (Await p)

  let q = Queue.create ()
  let enqueue t = Queue.push t q
  let dequeue () = if not @@ Queue.is_empty q then Queue.pop q ()

  let run main =
    let rec fork : 'a. 'a promise -> (unit -> 'a) -> unit = fun pr main ->
      let retc v =
        match !pr with
        | Done _    -> failwith "done"
        | Waiting l ->
          pr := Done v;
          List.iter (fun k -> enqueue @@ fun () -> continue k v) l;
          dequeue ()
      in
      let effc : type b. b Effect.t -> ((b,_) continuation -> unit) option = function
        | Yield   -> Some (fun k -> enqueue (continue k); dequeue ())
        | Async f -> Some (fun k ->
            let p = ref @@ Waiting [] in
            enqueue (fun () -> fork p f);
            continue k p
          )
        | Await p -> Some (fun k ->
            match !p with
            | Done v    -> continue k v
            | Waiting l -> p := Waiting (k :: l); dequeue ()
          )
        | _ -> None
      in
      match_with main () { retc; exnc = raise; effc }
    in
    fork (ref (Waiting [])) main
end

open Scheduler

let main () =
  let task name () =
    printf "starting %s\n%!" name;
    let v = Random.int 100 in
    printf "yielding %s\n%!" name;
    yield ();
    printf "ending %s with %d\n%!" name v;
    v
  in
  let pa = async (task "a") in
  let pb = async (task "b") in
  let pc = async (fun () -> await pa + await pb) in
  printf "Sum is %d\n%!" (await pc);
  assert (await pa + await pb = await pc)

let () = run main
