
type qbool = 
  | True
  | False

let not b =
  match b with
    | True -> False
    | False -> True

let And a b =
  match a with  
    | True -> b
    | False -> False
      
type nat = 
  | Z
  | S of nat

let c0 = Z
let c1 = S Z
let c2 = S (S Z) // = S c1

let is_zero n =
  match n with
    | Z -> True
    | S n' -> False

let P n = 
  match n with
    | Z -> Z
    | S n' -> n'

let rec plus a b =
  match a with
    | Z -> b
    | S christian -> S (plus christian b)

type 'a option = 
  | None
  | Some of 'a

let P2 n =
  match n with
    | Z -> None
    | S n' -> Some n'

let map f o = 
  match o with
    | None -> None
    | Some a -> Some (f a)

    
P2 c2
|> map (plus c1)


