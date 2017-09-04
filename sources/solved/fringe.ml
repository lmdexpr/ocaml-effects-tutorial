(* Same Fringe Problem

   Definition: Two binary trees have the same fringe if they have exactly
   the same leaves reading from left to right.

   Problem: Given two binary trees decide whether they have the same fringe.
*)

type ('elt,'cont) iterator = ('elt -> unit) -> 'cont -> unit

type 'elt generator = unit -> 'elt option

let generate (type elt) (i : (elt, 'cont) iterator) (c : 'cont) : elt generator =
  let module M = struct effect Yield : elt -> unit end in
  let open M in
  let rec step = ref (fun () ->
    i (fun v -> perform (Yield v)) c;
    step := (fun () -> None);
    None)
  in
  let loop () =
    try !step () with
    | effect (Yield v) k -> (step := continue k; Some v)
  in
  loop

type 'a tree =
| Leaf of 'a
| Node of 'a tree * 'a tree

let rec iter f = function
  | Leaf v -> f v
  | Node (l,r) -> iter f l; iter f r

let same_fringe t1 t2 =
  let gen_tree = generate iter in
  let g1 = gen_tree t1 in
  let g2 = gen_tree t2 in
  let rec loop () =
    match g1 (), g2 () with
    | None, None -> true
    | Some v1, Some v2 when v1 = v2 -> loop ()
    | _ -> false
  in
  loop ()

let t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
let t2 = Node (Node (Leaf 1, Leaf 2), Leaf 3)
let t3 = Node (Node (Leaf 3, Leaf 2), Leaf 1)
let t4 = Leaf 42
let t5 = Leaf 41
let t6 = Node (Leaf 1, Leaf 2)
let t7 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
;;

assert (same_fringe t1 t2);;
assert (same_fringe t2 t1);;
assert (not (same_fringe t1 t3));;
assert (same_fringe t1 t7);;
assert (same_fringe t2 t7);;
