let domain () = failwith "Domain"

let fact_tests: (int * float) list = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.)
]

let fact (n: int): float =
  if n < 0 then domain ()
  else
    let rec fact (n: int) (acc: float): float =
      match n with
        | 0 -> acc
        | _ -> fact (n - 1) ((float_of_int n) *. acc)
    in fact n 1.

let binomial_tests = [
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.)
]

let binomial (n: int) (k: int): float =
  if 0 <= k && k <= n then (fact n) /. ((fact k) *. fact (n - k))
  else domain ()

let ackerman_tests: ((int * int) * int) list = [
  ((0, 0) , 1);
  ((0, 1) , 2);
  ((1, 0) , 2);
  ((3, 4) , 125);
  ((3, 1) , 13);
]

let ackerman ((n: int), (k: int)): int =
  if n < 0 || k < 0 then domain ()
  else
    let rec ack (n: int) (k: int): int =
      match (n, k) with
      | (0, _) -> k + 1
      | (_, 0) -> ack (n - 1) 1
      | (_, _) -> ack (n - 1) (ack n (k - 1))
    in ack n k

let is_prime_tests : (int * bool) list = [
  (2, true);
  (3, true);
  (47, true);
  (8, false);
  (42, false);
]

let is_prime (n: int): bool =
  let rec is_not_divisor (d: int): bool =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  if n > 1 then is_not_divisor 2
  else domain ()

let square_root_tests: (float * float) list = [
  (1., 1.);
  (4., 2.);
  (14.0625, 3.75);
  (12.25, 3.5);
]

let square_root (a: float): float =
  let rec findroot (x: float) (acc: float): float =
    let next = (a /. x +. x) /. 2. in
    if abs_float (x -. next) < acc then next
    else findroot next acc
  in
  if a > 0. then findroot 1. epsilon_float
  else domain ()

let fib_tl_tests: (int * int) list = [
  (0, 1);
  (1, 1);
  (2, 2);
  (4, 5);
  (5, 8)
]

let rec fib_aux (n: int) (a: int) (b: int): int =
  if n = 0 then a
  else fib_aux (n - 1) b (a + b)

let fib_tl (n: int): int =
  if n < 0 then domain ()
  else fib_aux n 1 1
