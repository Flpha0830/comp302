let fact n =
  let rec factorial n =
    if n = 0 then 1
    else  n * factorial (n - 1)
  in
  if n <= 0 then 1 else factorial n

let binom (n, k) =
  if n < k then 0.0
  else float (fact n) /. (float (fact k) *. float (fact (n - k)))

let dist_black n x (marblesTotal, marblesDrawn) =
  (binom (n, x) *. binom (marblesTotal - n, marblesDrawn - x))
  /. (binom (marblesTotal, marblesDrawn))

let tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n-1) ((f n)::acc)
  in
  tab n []

let max_in_list l =
  let rec max_in_list' pos l =
    match l with
    | [] -> assert false
    | [h]  -> (pos, h)
    | h :: t ->
      let (q, mx) = max_in_list' (pos + 1) t in
      if h < mx then (q, mx)
      else (pos, h)
  in
  let (pos, _) = max_in_list' 0 l in
  pos

type ingredients = Chocolate | Orange | Almonds | Vanilla | Flour | BlackBeans

type cake = Slice of ingredients list | Cake of cake * cake

let rec insert x l = match l with
  | [] -> [x]
  | y::ys -> if x = y then l else y::(insert x ys)

let rec union l1 l2 = match l2 with
  | [] -> l1
  | x::xs -> union (insert x l1) xs

let tabulate_tests: (((int -> int) * int) * int list) list = [
  (((fun x -> x), 0), [0]);
  (((fun x -> x), 3), [0; 1; 2; 3])
]

let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  tabulate (fun n -> dist_black n x (marblesTotal, marblesDrawn)) marblesTotal

let is_empty_tests: (float list list * bool) list = [
  ([[]], true);
  ([[1.]], false);
  ([[]; []; []], true)
]

let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun l -> l = []) matrix

let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (fun x -> dist_table (total, drawn) x) resultList

let rec combined_dist_table (matrix: float list list) =
  if is_empty matrix then []
  else
    let get_heads = List.map (fun (h :: _) -> h) matrix  in
    let get_tails = List.map (fun (_ :: t) -> t) matrix  in
    let result    = List.fold_right (fun x r -> x *. r) get_heads 1.0 in
      result :: combined_dist_table get_tails

let combined_dist_table = function
  | [] -> []
  | x :: xs -> List.fold_left (List.map2 ( *. )) x xs

let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
      (dist_matrix (total, drawn) resultList))

let rec all (p: (ingredients list -> bool)) (c: cake) : bool =
  match c with
  | Slice l ->  p l
  | Cake (c1, c2) -> all p c1 && all p c2

let is_chocolate_cake_tests = [
  ((Slice []), false);
  ((Cake (Slice [Orange], Slice [])), false);
  ((Cake (Slice [], Slice [Almonds; Flour])), false);
  ((Slice [Chocolate]), true);
  ((Cake (Slice [Chocolate], Slice [Chocolate])), true);
  ((Cake (Slice [Orange; Flour], Slice [Chocolate])), false);
]

let is_chocolate_cake (c: cake) : bool =
  all (fun il -> List.exists (fun i -> i = Chocolate) il) c

let rec map (p: (ingredients list -> ingredients list)) (c: cake) =
  match c with
  | Slice il -> Slice (p il)
  | Cake (c1, c2) -> Cake (map p c1, map p c2)

let add_ingredient_tests = [
  ( (Orange, Slice []), Slice [Orange]);
  ( (Orange, (Cake (Slice[], Slice []))) , (Cake (Slice[Orange], Slice [Orange])) );
  ( (Orange, (Cake (Slice[Chocolate], Slice [Chocolate]))) , (Cake (Slice[Chocolate; Orange], Slice [Chocolate; Orange])) )
]

let add_ingredient (x: ingredients) (c: cake) : cake =
  map (fun il -> insert x il) c

let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a =
  match c with
  | Slice il -> f il base
  | Cake (c1, c2) -> (fold_cake f (fold_cake f base c1) c2)

let get_all_ingredients (c: cake) : ingredients list =
  fold_cake (fun il base -> union il base) [] c
