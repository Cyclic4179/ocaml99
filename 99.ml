

type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;


let cbal_tree n =
    let gen_node_list_from_lists left right all =
        let add_right_tree acc l =
            List.fold_left (fun a r -> Node ('x',l,r)::a) acc right in
        List.fold_left add_right_tree all left in
    let rec construct_tree i =
        if i = 0 then [Empty]
        else
            let newn = i-1 in
            let part2 = newn / 2 in let part1 = newn - part2 in
            if part1 = part2
            then
                let t = construct_tree part1 in
                gen_node_list_from_lists t t []
            else
                let t1 = construct_tree part1 in
                let t2 = construct_tree part2 in
                gen_node_list_from_lists t1 t2 (gen_node_list_from_lists t2 t1 []) in
    construct_tree n;;


let rec is_mirror a b =
    match a,b with
    | Empty, Empty -> true
    | Node (_,la,ra), Node (_,lb,rb) -> (is_mirror la lb) && (is_mirror ra rb)
    | _ -> false;;


let is_symmetric = function
    | Empty -> true
    | Node (_,l,r) -> is_mirror l r;;


let construct l =
    let rec insert x = function
        | Empty -> Node (x,Empty,Empty)
        | Node (y,l,r) as n ->
                if x = y then n
                else if x < y then Node (y,insert x l,r)
                else Node (y,l,insert x r) in
    let rec aux t = function
        | [] -> t
        | hd :: tl -> aux (insert hd t) tl in
    aux Empty l;;


let sym_cbal_trees n =
    List.filter is_symmetric (cbal_tree n);;


let hbal_tree n =
    (*let node_perms l acc r =
        Node ('x',l,r)::Node ('x',Empty,r)::Node ('x',l,Empty)::acc in*)
    let add_trees_with left right all =
        let add_right_tree all l =
            List.fold_left (fun a r -> Node ('x',l,r)::a) all right in
        List.fold_left add_right_tree all left in
    let rec aux i =
        if i <= 0 then [Empty]
        else if i = 1 then [Node ('x',Empty,Empty)]
        else
            let t1 = aux (i-1) in
            let t2 = aux (i-2) in
            add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 [])) in
    aux n;;


let max_nodes h = 1 lsl h - 1;;


let rec min_nodes h =
    if h <= 0 then 0 else
    1 + min_nodes (h-1) + min_nodes (h-2);;


let min_nodes1 h =
    let rec aux acc prev beforeprev i =
        if i = h then acc
        else
            let curr = prev + beforeprev in
            let newacc = acc + curr in
            aux newacc curr prev (i+1)
    in aux 0 0 1 0;;


let min_nodes2 n =
    let n = float_of_int n in
    0.5 *. ((3./.(sqrt 5.) +. 1.) *. ((1. +. sqrt 5.)/.2.) ** n
    +. (-.3./.(sqrt 5.) +. 1.) *. ((1. -. sqrt 5.)/.2.) ** n) -. 1.;;


let min_height n = int_of_float (ceil (log (float_of_int (n + 1)) /. log 2.));;


let max_height n = "??";;
(* max_height n = max_height ?/2*)


(**let hbal_tree_nodes n =
    let merge_trees left right all =
        let merge_left all r =
            List.fold_left (fun a l -> Node ('x',l,r)::a) all left in
        List.fold_left merge_left all right in
    let empty = [Empty] in
    let rec aux i remd =
        if remd = 0 || i = 0 then [Empty]
        else
            let t1 = aux (i-1) (remd-1) in
            let t2 = aux (i-2) (remd-1) in
            merge_trees t1 empty (merge_trees empty t1 (merge_trees t2 t2 [])) in
    aux n*)


(**let hbal_tree_nodess n =
    let merge_trees left right all =
        let merge_left all r =
            List.fold_left (fun a l -> Node ('x',l,r)::a) all left in
        List.fold_left merge_left all right in
    let rec aux i =
        if i = 0 then [Empty]
        else
            if i mod 2 = 0
            then
                let t = 3 + 3.;;*)
