
(* Have fun with LearnOCaml *)

let test_cases = [
  [];
  [None];
  [Some(1)];
  [Some(1); Some(2)];
  [Some(1); Some(2); None];
  [Some(1); Some(2); None; Some(5)];
] 

(*
   answer
   [
    Some []; 
    None; 
    Some [1]; 
    Some [2; 1]; 
    None; 
    None
  ]
*)
            
let eval init list = 
  List.fold_left
    (fun state var -> 
       match state with 
       | None -> None
       | Some(l) -> 
           match var with
           | None -> None
           | Some(x) -> Some(x::l)
    ) init list 
    
    
(*  
    List.fold_left f init [b1; ...; bn] is f (... (f (f init b1) b2) ...) bn
    init -> initial state, Some []
    list -> test case eg.[Some(1); Some(2)]
    state-> the return state  eg. Some [2;1] / None
    var  -> variable in the list eg. Some(1) / None
*)
  
let test = 
  List.map (fun list -> eval (Some []) list) test_cases
  
(*
   List.map f [a1; ...; an] -> [f a1; ...; f an]
*)
  
  
  
  
  
  
  
  
  
  
  
  
  