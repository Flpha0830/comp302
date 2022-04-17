
(* Have fun with LearnOCaml *) 

type either   = Right of int | Left  of int 
type either_l = Right_l of int list | Left_l of int list
 
let test_cases = [
  [];
  [Left(1)];
  [Right(1)];
  [Right(1); Right(2)];
  [Right(1); Right(2); Left(-3); Left(-4)];
  [Right(1); Right(2); Left(-2); Right(5)];
] 

(*
   answer
   [
    Right_l []; 
    Left_l [1]; 
    Right_l [1]; 
    Right_l [2; 1]; 
    Left_l [-4; -3];
    Left_l [-2]
  ]
*)
            
let eval init list = 
  List.fold_left
    (fun state var -> 
       match state, var with
       | Left_l(l),  Left(x)  -> Left_l(x::l)
       | Left_l(l),  Right(x) -> Left_l(l)
       | Right_l(l), Left(x)  -> Left_l([x])
       | Right_l(l), Right(x) -> Right_l(x::l) 
    ) init list 
  
(*  
    List.fold_left f init [b1; ...; bn] is f (... (f (f init b1) b2) ...) bn
    init -> initial state, Right_l []
    list -> test case eg.[Right(1); Right(2)]
    state-> the return state  eg. Left_l [-4; -3] / Right [2; 1]
    var  -> variable in the list eg. Left(1) / Right(1)
*)
  
let test = 
  List.map (fun list -> eval (Right_l []) list) test_cases
  
(*
   List.map f [a1; ...; an] -> [f a1; ...; f an]
*)
  
  
  
  
  
  
  
  
  
  
  