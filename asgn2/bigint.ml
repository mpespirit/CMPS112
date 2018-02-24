(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
   
    let rec trim list = match list with
        | []        -> []
        | [0]       -> []
        | car::cdr  ->
            let cdr' = trim cdr
            in match car, cdr' with
                | 0, []     -> []
                | car, cdr' -> car::cdr'
    
    let rec comp list1 list2 =
        if (List.length list1) > (List.length list2)
            then 1
        else if (List.length list2) > (List.length list1)
            then -1
        else match (list1, list2) with
            | [], [] -> 0
            | [], list2 -> -1
            | list1, [] -> 1
            | list1, list2 ->
                let rev1 = reverse list1 in
                let rev2 = reverse list2 in
                    if (car rev1) > (car rev2) then 1
                    else if (car rev2) > (car rev1) then -1
                    else comp (reverse (cdr rev1)) (reverse (cdr rev2))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | [], _, _           -> []
        | list1, [], 0       -> list1
        | list1, [], carry   -> trim (sub' list1 [carry] 0)
        | car1::cdr1, car2::cdr2, carry ->
            let dif = car1 - (car2 + carry)
            in if dif < 0
                then dif + radix :: trim (sub' cdr1 cdr2 1)
                else dif :: trim (sub' cdr1 cdr2 0)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else let max = comp value1 value2 in
            if max > 0 then Bigint (neg1, (sub' value1 value2 0))
            else if 0 > max then Bigint (neg2, (sub' value2 value1 0))  
            else zero

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2 
        then let a = comp value1 value2 in
            if a > 0 then Bigint (Pos, (sub' value1 value2 0))
            else if 0 > a then Bigint (Neg, (sub' value2 value1 0))
            else zero
        (* need to put sign-checking here *)
        else Bigint (neg1, add' value1 value2 0) 

(*
    let rec mul' list1 list2 sum = match (list1, list2, sum) with
        | [], [], _          -> []
        | [], _, _          -> []
        | _, [], _          -> []
        | _, [1], _         -> add' list1 sum 0
        | list1, list2, _  -> 
            (*let a = comp list2 [1]
            in if a = 0 then (add' list1 sum 0)*)
            mul' list1 (sub' list2 [] 1) (add' sum list1 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2 then Bigint (Pos, mul' value1 value2 [] )
        else Bigint (Neg, mul' value1 value2 [0] )
*)
    let rec mul' list1 pow2 list2 =
    if (comp pow2 list1) > 0
    then list1, []
    else let rem, prod =
           mul' list1 (add' pow2 pow2 0) (add' list2 list2 0)
         in if (comp rem pow2) < 0
             then rem, prod
             else (sub' rem pow2 0), (add' prod list2 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2 
        then let _, prod = mul' value1 [1] value2 in Bigint(Pos, prod)
        else let _, prod = mul' value1 [1] value2 in Bigint(Neg, prod)

    let rec divrem' list1 pow2 list2 =
        if (comp list2 list1) > 0
        then [], list1
        else let quot, rem =
               divrem' list1 (add' pow2 pow2 0) (add' list2 list2 0)
            in if (comp list2 rem) > 0
               then quot, rem
               else (add' quot  pow2 0), (sub' rem list2 0)

    let divrem list1 list2 = divrem' list1 [1] list2

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let quot, _ = divrem value1 value2 in Bigint(Pos, quot)
        else let quot, _ = divrem value1 value2 in Bigint(Neg, quot)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2))=
        if neg1 = neg2
        then let _, rem = divrem value1 value2 in Bigint(Pos, rem)
        else let _, rem = divrem value1 value2 in Bigint(Neg, rem)

    let pow = add

    (*
    let div2 list1 = div (Bigint (Pos, value1)) (Bigint (neg2, value2)) 

    let rec pow' (Bigint(Neg1, list1) (Bigint (Neg2, list2) 
                result = match list2 with
        | []          -> result  
        | list2 
          when (comp (div (Bigint (Pos, list2)) (Bigint (Pos, [2]))) 
                []) = 0
          -> pow' (mul' list1 [1] list2) 
                  (div (Bigint (Pos, list2)) (Bigint (Pos, [2])))
                  result
        | list2 -> pow' list1 (sub' list2 [] 1) (mul' list1 result)  

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg 
            then pow' (div (Bigint(Pos, [1])) (Bigint(neg1, list1))) 
                      (Bigint (Pos, list2)) (Bigint(Pos, [1]))
            else pow' (Bigint (Pos, list1)) (Bigint (Pos, list2))
                      (Bigint(Pos, [1]))
     *)

end

