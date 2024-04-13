type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let node l r = Node ('x', l, r);;
let joinNodes lefts rights all =
        let f list left =
            List.fold_left (fun l right -> (node left right)::l) list rights
        in List.fold_left f all lefts;;

let cbal_tree n =
    let rec aux nRem =
        if nRem = 0
        then [ Empty ]
        else
            let newN = nRem - 1
            in let half = newN / 2
            in let otherhalf = newN - half
            in if half = otherhalf
            then let childs = aux half in joinNodes childs childs []
            else
                let somechilds = aux half
                in let otherchilds = aux otherhalf
                in joinNodes somechilds otherchilds (joinNodes otherchilds somechilds [])
    in aux n;;


let rec is_mirror a b =
    match a, b with
    | Empty, Empty -> true
    | Node (_, la, ra), Node (_, lb, rb) ->
            is_mirror la lb && is_mirror ra rb
    | _ -> false

let is_symmetric = function
    | Empty -> true
    | Node (_, l, r) -> is_mirror l r;;



let construct l =
    let rec insert acc a =
        match acc with
        | Empty -> Node (a, Empty, Empty)
        | Node (b, l, r) ->
                if a < b
                then Node (b, insert l a, r)
                else Node (b, l, insert r a)
    in List.fold_left insert Empty l;;


let sym_cbal_trees n =
    List.filter is_symmetric (cbal_tree n);;


let rec hbal_tree n =
    if n = 0
    then [ Empty ]
    else if n = 1
    then [ node Empty Empty ]
    else
        let normal = hbal_tree (n - 1)
        in let low = hbal_tree (n - 2)
        in joinNodes normal normal (joinNodes normal low (joinNodes low normal []));;


let rec amount_nodes = function
    | Empty -> 0
    | Node (_, l, r) -> 1 + amount_nodes l + amount_nodes r;;

let max_nodes h = 1 lsl h - 1;;

(* inefficient bc double recursion *)
let rec min_nodes h =
    if h <= 1
    then h
    else min_nodes (h-1) + min_nodes (h-2) + 1;;

let rec min_nodes_loop prev pprev h =
    if h <= 1
    then prev
    else min_nodes_loop (prev + pprev + 1) prev (h - 1);;

let min_nodes h =
    if h <= 0 then 0 else min_nodes_loop 1 0 h;;

let min_nodes1 n =
    let n = float_of_int n in
    let res = 0.5 *. ((3./.(sqrt 5.) +. 1.) *. ((1. +. sqrt 5.)/.2.) ** n
    +. (-.3./.(sqrt 5.) +. 1.) *. ((1. -. sqrt 5.)/.2.) ** n) -. 1.
    in int_of_float res;;

let min_height n = int_of_float (ceil (log (float_of_int (n + 1)) /. log 2.));;

let max_height n =
    (* search upwards for max_height using min_nodes *)
    let rec aux h =
        let newh = h + 1
        in if min_nodes newh > n
        then h
        else aux newh
    in aux 0;;

(* to test the accuracy of min_nodes1 fkt, _end might be Int.max_int *)
let test f g start _end =
    let rec aux curr =
        if curr > _end
        then None
        else
            let fapp = f curr
            in let gapp = g curr
            in if fapp = gapp
            then aux (curr + 1)
            else Some (curr, fapp, gapp)
    in aux start;;

let hbal_tree_nodes n =
    let filter_amount h =
        let f x = amount_nodes x = n
        in List.filter f (hbal_tree h)
    in let minH = min_height n
    in let maxH = max_height n
    in let rec joinHeights acc currH =
        if currH > maxH
        then acc
        else
            joinHeights (filter_amount currH @ acc) (currH + 1)
    in joinHeights [] minH;;
    (*let rec aux remNodes remHeight =
        if remHeight = 0
        then [ Empty ]
        else if n = 1
        then [ node Empty Empty ]
        else
            let normal = aux (n - 1)
            in let low = aux (n - 2)
            in joinNodes normal normal (joinNodes normal low (joinNodes low normal []))
    in aux n;;*)
