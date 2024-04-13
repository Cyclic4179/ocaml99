type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;


let table2 var_a var_b exp =
    let rec aux val_a val_b = function
        | Var x ->
            if x = var_a then val_a
            else if x = var_b then val_b
            else raise (Invalid_argument "unknown var in expression")
        | Not e -> not (aux val_a val_b e)
        | And (e1,e2) -> (aux val_a val_b e1) || (aux val_a val_b e2)
        | Or (e1,e2) -> (aux val_a val_b e1) || (aux val_a val_b e2)
    in begin
        List.map
        (fun (val_a,val_b) -> val_a,val_b,aux val_a val_b exp)
        [true,true;true,false;false,true;false,false]
    end;;


table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")));;


let table vars expr =
    let rec assoc x = function
        | [] -> raise (Invalid_argument "unknown var in expression")
        | (a,b) :: tl -> if x = a then b else assoc x tl
    in let rec aux vars_vals = function
        | Var x -> assoc x vars_vals
        | Not e -> not (aux vars_vals e)
        | And (e1,e2) -> (aux vars_vals e1) && (aux vars_vals e2)
        | Or (e1,e2) -> (aux vars_vals e1) || (aux vars_vals e2)
    in let vars_perms = begin
        let rec gen acc = function
            | [] -> [List.rev acc]
            | hd :: tl -> gen ((hd,true)::acc) tl @ gen ((hd,false)::acc) tl
        in gen [] vars
    end
    in List.map (fun vars_vals -> vars_vals, (aux vars_vals expr)) vars_perms;;


table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;


let gray n =
    let rec aux s l even1 =
        if l = n then [s] else let nl = l + 1 in begin
            if even1
            then aux (s ^ "0") nl true @ aux (s ^ "1") nl false
            else aux (s ^ "1") nl true @ aux (s ^ "0") nl false
        end
    in aux "" 0 true;;


type tree = Leaf of (string * int) | Node of (tree * tree * int);;


let huffman fs =
    let sorted_fs =
        let rec sorted_insert x xfreq = function
            | [] -> [x]
            | (_,freq) as hd :: tl ->
                    if xfreq < freq
                    then x::hd::tl
                    else hd::(sorted_insert x xfreq tl) in
        let rec sort_by_freq acc = function
            | [] -> acc
            | (_,freq) as hd :: tl -> sort_by_freq (sorted_insert hd freq acc) tl in
        List.map
        (fun (s,freq) -> Leaf (s,freq))
        (sort_by_freq [] fs) in
    let huffman_tree =
        let rec sorted_insert x xfreq = function
            | [] -> [x]
            | (Leaf (_,freq) | Node (_,_,freq)) as hd :: tl ->
                    if xfreq < freq
                    then x::hd::tl
                    else hd::(sorted_insert x xfreq tl) in
        let rec put2 = function
            | [] -> []
            | (Leaf (_,freq1) | Node (_,_,freq1)) as hd1 :: tl -> begin
                match tl with
                | [] -> [hd1]
                | (Leaf (_,freq2) | Node (_,_,freq2)) as hd2 :: rem ->
                        let combindedfreq = freq1 + freq2 in
                        put2 (sorted_insert (Node (hd1, hd2, combindedfreq)) combindedfreq rem)
            end in
        List.hd (put2 sorted_fs) in
    let huffman_code =
        let rec follow_path acc = function
            | Leaf (str,freq) -> [(str,acc)]
            | Node (left,right,_) ->
                    follow_path (acc ^ "0") left @ follow_path (acc ^ "1") right in
        follow_path "" huffman_tree in
    huffman_code;;



let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16);
          ("e", 9); ("f", 5)];;
