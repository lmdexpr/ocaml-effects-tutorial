open Printf
open Effect
open Effect.Shallow

module type STATE = sig
  type t
  val put     : t -> unit
  val get     : unit -> t
  val history : unit -> t list
  val run : (unit -> unit) -> init:t -> unit
end

module State (S : sig type t end) : STATE with type t = S.t = struct
  type t = S.t

  type _ Effect.t +=
    | Get : t Effect.t
    | Put : t -> unit Effect.t
    | History : t list Effect.t

  let get ()     = perform Get
  and put v      = perform (Put v)
  and history () = perform History

  open struct
    let head ~default = function
      | [] -> default
      | xs -> List.hd xs
    let identity x = x
  end

  let run f ~init =
    let rec loop : type a r. t -> t list -> (a, r) continuation -> a -> r = fun init state k x ->
      let effc : type b. b Effect.t -> ((b, r) continuation -> r) option =
        function
        | Get     -> Some (fun k -> loop init state k @@ head ~default:init state)
        | Put v   -> Some (fun k -> loop init (v::state) k ())
        | History -> Some (fun k -> loop init state k @@ List.rev state)
        | _       -> None
      in
      continue_with k x { retc = identity ; exnc = raise ; effc }
    in
    loop init [] (fiber f) ()
end

module IS = State (struct type t = int end)
module SS = State (struct type t = string end)

let foo () : unit =
  assert (0 = IS.get ());
  IS.put 42;
  assert (42 = IS.get ());
  IS.put 21;
  assert (21 = IS.get ());
  SS.put "hello";
  assert ("hello" = SS.get ());
  SS.put "world";
  assert ("world" = SS.get ());
  assert ([42; 21] = IS.history ())

let _ = IS.run (fun () -> SS.run foo ~init:"") ~init:0
