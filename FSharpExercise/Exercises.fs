module Exercises

(* Helping function for running tests (just ignore it) *)
let test f tests =
  let res = List.filter (fun (e, exp) -> f e <> exp) tests in
  if res = []
  then printf "Success!\n"
  else 
    printf "Unsuccesful tests:\n";
    List.iter (fun (e, exp) -> 
      printf "  %A should give %A but gave %A\n" e exp (f e)) res

(*
  We will explore:
  * comments
  * bindings
  * algebraic datatypes (sum and product)
  * pattern matching
  * failure
  * recursion
  * let ... in
  * higher order functions
  * if-expressions
  * polymorphism
  * numbers
  * challenges
*)


(*
  bindings
*)

let binding = 4

(*
  algebraic datatypes
*)

let pair = (2, 3)

type 'a list =
  | Nil
  | Cons of 'a * 'a list

(*
  pattern matching
*)

let is_empty xs =
  match xs with
    | Nil -> true
    | Cons (hd, tl) -> false
test is_empty
  [ Nil, true;
    Cons (1, Nil), false;
    Cons (1, Cons (2, Nil)), false ]

(*
  failure
*)

let head xs =
  match xs with
    | Nil -> failwith "empty"
    | Cons (hd, tl) -> hd
test head
  [ Cons (1, Nil), 1;
    Cons (2, Cons (1, Nil)), 2 ]

let tail xs =
  match xs with
    | Nil -> Nil
    | Cons (hd, tl) -> tl
test tail
  [ Nil, Nil;
    Cons (1, Nil), Nil;
    Cons (1, Cons (2, Nil)), Cons (2, Nil) ]

(*
  recursion
*)

let rec length xs =
  match xs with
    | Nil -> 0
    | Cons (hd, tl) -> 1 + length tl
test length
  [ Nil, 0;
    Cons (1, Nil), 1;
    Cons (1, Cons (2, Nil)), 2 ]

let rec sum xs =
    match xs with
    | Nil -> 0
    | Cons (hd, tl) -> hd + sum tl
  (* exercise *)
test sum
  [ Nil, 0;
    Cons (1, Nil), 1;
    Cons (1, Cons (2, Nil)), 3]

    // Base case: ys = NIL
    // Put ys head at the end of xs
    // Call append with xs + yhead and ytail
let rec append xs ys =
    match xs, ys with
        | Nil, Nil -> Nil
        | Cons (xhd, xtl), Nil -> xs
        | Nil, Cons (yhd, ytl) -> ys
        | Cons (xhd, xtl), Cons (yhd, ytl) ->Cons(xhd, append xtl ys) 


  
  (* exercise *)
test (fun (xs, ys) -> append xs ys)
    [
    (Nil, Nil), Nil; (Nil, Cons (1, Nil)), Cons (1, Nil);
    (Cons (1, Nil), Nil), Cons (1, Nil);
    (Cons (1, Cons (2, Nil)), Cons (3, Nil)), Cons (1, Cons (2, Cons (3, Nil))) ]

let rec snoc xs x =
    match xs with
    | Nil -> Cons(x, Nil)
    | Cons (hd, tl) -> Cons(hd, snoc tl x) 
  (* exercise *)

test (fun (xs, x) -> snoc xs x)
  [ (Nil, 1), Cons (1, Nil);
    (Cons (1, Nil), 2), Cons (1, Cons (2, Nil));
    (Cons (1, Cons (2, Nil)), 3), Cons (1, Cons (2, Cons (3, Nil))) ]

let rec last xs =
    match xs with 
    | Nil -> 0 
    | Cons (hd, tl) -> if tl = Nil then hd else last tl  
  (* exercise *)
test last
  [ Cons (1, Nil), 1;
    Cons (1, Cons (2, Nil)), 2 ]

let rec rev xs =
    match xs with 
    | Nil -> Nil
    | Cons (hd, tl) -> snoc (rev tl) hd
  (* exercise *)
test rev
  [ Cons (1, Cons (2, Nil)), Cons (2, Cons (1, Nil)) ]

//(*
//  let ... in
//*)

let rev' xs =
  let rec loop xs acc =
    match xs with
      | Nil -> acc
      | Cons (hd, tl) -> loop tl (Cons (hd, acc)) in
  loop xs Nil
test rev'
  [ Cons (1, Cons (2, Nil)), Cons (2, Cons (1, Nil)) ]

//(*
//  higher order functions
//*)

let rec fold_right f b xs =
  match xs with
    | Nil -> b
    | Cons (x, xs) -> f x (fold_right f b xs)
let append' xs ys = fold_right (fun x b -> Cons (x, b)) ys xs
test (fun (xs, ys) -> append' xs ys)
  [ (Nil, Nil), Nil;
    (Nil, Cons (1, Nil)), Cons (1, Nil);
    (Cons (1, Nil), Nil), Cons (1, Nil);
    (Cons (1, Cons (2, Nil)), Cons (3, Nil)), Cons (1, Cons (2, Cons (3, Nil))) ]

//(* Fun fact: Every function (which terminates) can be made using fold_right, and no recursion nor pattern-matching *)

 //Missing
//let rec fold_left f acc xs =
//    match xs with
//    | Nil -> acc
//    | Cons (hd, tl) -> f hd (fold_left f acc tl)
//  (* exercise *)
//let rev'' xs = fold_left (fun x b -> Cons (x, b)) Nil xs
//test (fun (xs) -> rev'' xs)
//  [ Cons (1, Cons (2, Nil)), Cons (2, Cons (1, Nil)) ]


let rec map f xs =
    match xs with 
    | Nil -> Nil
    | Cons (hd, tl) -> Cons(f hd, map f tl)
  (* exercise *)
test (map (fun x -> x + 2))
  [ Cons (1, Nil), Cons (3, Nil);
    Cons (1, Cons (2, Nil)), Cons (3, Cons (4, Nil)) ]

let rec forall f xs =
    match xs with
    | Nil -> true
    | Cons (hd, tl) -> 
    if f hd = false
    then false
    else 
    forall f tl

  (* exercise *)
test (forall (fun x -> x < 5))
  [ Cons (10, Cons (2, Nil)), false;
    Cons (4, Cons (2, Nil)), true ]

let rec exists f xs =
    match xs with 
    | Nil -> false
    | Cons (hd, tl) -> 
    if f hd = true
    then true
    else exists f tl
test (exists (fun x -> x > 5))
  [ Cons (10, Cons (2, Nil)), true;
    Cons (4, Cons (2, Nil)), false ]

//(*
//  if-expressions
//*)

let rec filter_in f xs =
  match xs with
    | Nil -> Nil
    | Cons (hd, tl) -> 
      if f hd 
      then Cons (hd, filter_in f tl)
      else filter_in f tl
test (filter_in (fun x -> x > 5))
  [ Cons (10, Cons (2, Nil)), Cons (10, Nil);
    Cons (4, Cons (2, Nil)), Nil ]

let rec filter_out f xs =
    match xs with
    | Nil -> Nil
    | Cons (hd, tl) -> 
        if f hd = false
        then Cons (hd, filter_out f tl)
        else filter_out f tl

  (* exercise *)
test (filter_out (fun x -> x > 5))
  [ Cons (10, Cons (2, Nil)), Cons (2, Nil);
    Cons (4, Cons (2, Nil)), Cons (4, Cons (2, Nil)) ]

let rec contains f xs =
    match xs with 
    | Nil -> false
    | Cons (hd, tl) ->
    if f hd 
    then true 
    else 
    contains f tl
        
  (* exercise *)
test (contains (fun x -> x = 4))
  [ Cons (10, Cons (2, Nil)), false;
    Cons (4, Cons (2, Nil)), true ]

//let rec contains_all xs ys =
//  (* exercise *)


let rec find f xs =
    match xs with 
    | Nil -> -1
    | Cons (hd, tl) -> 
        if f hd 
        then hd 
        else 
        find f tl

  (* exercise *)
test (find (fun x -> x < 5))
  [ Cons (10, Cons (2, Nil)), 2;
    Cons (4, Cons (2, Nil)), 4 ]

let indexOf e xs =
    let rec recIndexOf e xs counter = 
        match xs with 
        | Nil -> -1
        | Cons (hd, tl) -> 
        if e = hd then
            counter 
        else 
        recIndexOf e tl (counter + 1)
    recIndexOf e xs 0 
    
  (* exercise *)
test (indexOf 4)
  [ Cons (10, Cons (2, Cons (4, Nil))), 2;
    Cons (4, Cons (2, Nil)), 0 ]

//(*
//  polymorphism

//Change int_list to:
//type 'a list =
//  | Nil
//  | Cons of 'a * 'a list
//*)

//(*
//  pairs
//*)

let rec zip xs ys =
  match xs, ys with
    | Nil, Nil -> Nil
    | Cons (x, xs), Cons (y, ys) -> Cons ((x, y), zip xs ys)
    | _ -> failwith "not same length"

let rec unzip xs =
  match xs with
    | Nil -> (Nil, Nil)
    | Cons ((x, y), xs) -> 
      let (o1, o2) = unzip xs in
      (Cons (x, o1), Cons (y, o2))
test unzip
  [ Cons ((10, 1), Cons ((9, 2), Nil)), (Cons (10, Cons (9, Nil)), Cons (1, Cons (2, Nil))) ]

let rec assoc key map =
    match map with 
    | Nil -> -1
    | Cons ((xkey, value), tl) ->
    if xkey = key
    then value 
    else 
    assoc key tl
  (* exercise *)
test (assoc 5)
  [ Cons ((5, 10), Cons ((2, 11), Nil)), 10;
    Cons ((4, 11), Cons ((5, 2), Nil)), 2 ]

// Need to do
//let partition f xs =
//    let rec recPartition xs left right =
//        match xs with
//        | Nil -> (Cons (left, Nil), (right, Nil))
//        | Cons (hd, tl) -> 
//            if f hd then
//                recPartition tl (append left (Cons (hd, Nil))) right
//            else 
//                recPartition tl left (append right (Cons (hd, Nil))) 
//    recPartition xs Nil Nil
  
//  (* exercise *)
//test (partition (fun x -> x > 5))
//  [ Cons (10, Cons (2, Nil)), (Cons (10, Nil), Cons (2, Nil));
//    Cons (4, Cons (2, Nil)), (Nil, Cons (4, Cons (2, Nil))) ]

//(*
//  numbers
//*)

let rec fac n =
  if n = 0
  then 1
  else n * fac (n-1)

let skip n xs =
  let rec recskip xs counter = 
    match xs with
    | Nil -> Nil
    | Cons (hd, tl) -> 
    if counter = n 
    then tl
    else 
    recskip tl (counter + 1)
  recskip xs 1

test (skip 2)
  [ Cons (1, Cons (2, Cons (3, Nil))), Cons (3, Nil);
    Cons (3, Nil), Nil ]

let index n xs =
    let rec recIndex xs counter =
        match xs with 
        | Nil -> -1
        | Cons (hd, tl) ->
        if counter = n 
        then hd
        else recIndex tl (counter + 1)
    recIndex xs 1
  (* exercise *)
test (index 2)
  [ Cons (1, Cons (2, Cons (3, Nil))), 2;
    Cons (3, Cons (5, Nil)), 5 ]
    
let take n xs =
   let rec recTake xs ys counter =
        match xs with 
        | Nil -> ys
        | Cons (hd, tl) ->
        if n = counter
        then ys
        else 
        recTake tl (snoc ys hd) (counter + 1)
   recTake xs Nil 0
  (* exercise *)
test (take 2)
  [ Cons (1, Cons (2, Cons (3, Nil))), Cons (1, Cons (2, Nil));
    Cons (3, Nil), Cons (3, Nil);
    Cons (4, Cons (5, Cons (6, Nil))), Cons (4, Cons (5, Nil));]

let ending n xs =
    let rec recEnding counter saved xs2 = 
        match xs2 with 
            | Nil ->
                if (counter - n) > 0
                then skip (counter - n) saved
                else saved
            | Cons (hd, tl) ->
            recEnding (counter + 1) (snoc saved hd) tl
    recEnding 0 Nil xs 
    
  (* exercise *)
test (ending 2)
  [ Cons (1, Cons (2, Cons (3, Nil))), Cons (2, Cons (3, Nil));
    Cons (3, Cons (5, Nil)), Cons (3, Cons (5, Nil)) ]

//(*
//  challenges
//*)

let conv xs ys1 =
    let rec recConv xs ys2 result counter = 
        match xs with 
        | Nil -> result
        | Cons (xhd, xtl) -> 
            match ys2 with 
            | Nil -> recConv xtl (take (counter) ys1) result 0
            | Cons (yhd, ytl) -> 
            if ytl = Nil then
                if result <> Nil then             
                    recConv xtl (take (counter) ys1) (append result (Cons((xhd, yhd), Nil))) 0
                else 
                    recConv xtl (take (counter) ys1) (Cons((xhd, yhd), Nil)) 0
            else 
            recConv xs ytl result (counter + 1)
    recConv xs ys1 Nil 0

  (* exercise *)
test (fun (xs, ys) -> conv xs ys)
  [ (Cons (1, Cons (2, Cons (3, Nil)))
    ,Cons (4, Cons (5, Cons (6, Nil)))),
    Cons ((1, 6), Cons ((2, 5), Cons ((3, 4), Nil))) ]


let sub_avg xs1a =
    let rec recSub_avg xs1b result avg= 
        match xs1b with 
        | Nil -> result
        | Cons (hd, tl) ->
            if result = Nil then
                recSub_avg tl (Cons (hd - avg, Nil)) avg
            else
                recSub_avg tl (append result (Cons (hd - avg, Nil))) avg
    recSub_avg xs1a Nil ((sum xs1a) / (length xs1a))
            

  (* exercise *)
test sub_avg
  [ Cons (1, Cons (2, Cons (3, Nil))), Cons (-1, Cons (0, Cons (1, Nil)));
    Cons (6, Cons (3, Cons (-9, Nil))), Cons (6, Cons (3, Cons (-9, Nil))) ]
