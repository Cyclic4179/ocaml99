let asdf = 5;;


[ "a"; "b" ];;


let rec last = function
    | [] -> None
    | [item] -> Some item
    | _ :: t -> last t;;


last ["a" ; "b" ; "c" ; "d"];;


last [];;


let rec last_two = function
    | [] | [_] -> None
    | [a;b] -> Some (a,b)
    | h::t -> last_two t;;


last_two ["a"; "b"; "c"; "d"];;


let rec nth l c = match l with
    | [] -> raise (Failure "nth")
    (** better: if c = 0 then h else nth f (c-1) *)
    | h::f -> begin match c with
        | 0 -> h
        | x -> nth f (x-1)
    end;;


nth ["a"; "b"; "c"; "d"; "e"] 2;;


nth ["a"] 2;;


let rec length = function
    | [] -> 0
    | _::t -> 1 + length t;;


length ["a"; "b"; "c"];;


length [];;


let rec rev = function
    [] -> []
| [x] -> [x]
| h::t -> (rev t) @ [h];;


rev ["a"; "b"; "c"];;


let is_palindrome l =
    let rec aux l r =
        match l with
        | [] | [_] -> true
        | h::t -> let rh::rt=r in if rh = h then aux t rt else false
    in aux l (rev l);;


is_palindrome ["x"; "a"; "m"; "a"; "x"];;


not (is_palindrome ["a"; "b"]);;


type 'a node =
  | One of 'a
  | Many of 'a node list;;


let flatten l =
    let rec aux = function
        | [] -> []
        | h::t -> begin match h with
            | One x -> x :: (aux t)
            | Many x -> (aux x) @ (aux t)
        end
    in aux l;;


flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;


let compress l =
    let rec aux p = function
        | [] -> []
        | h::t -> if h=p then aux h t else p::(aux h t)
    in let h::t=l in aux h t;;


compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let rec compress = function
    | a :: (b :: _ as t) -> if a=b then compress t else b::(compress t)
    | x -> x;;


compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let rec pack l =
    let rec aux samevalue = function
        | [] -> [samevalue]
        | h::t -> begin match samevalue with
            | [] -> aux [h] t
            | sh::_ -> if h=sh then aux (h::samevalue) t else samevalue::(aux [h] t)
        end
    in aux [] l;;


pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;


(** this fails (expects unit list)
let encode l =
    let rec aux current counter = function
        | [] -> []
        | h::t -> if h = current then aux current (counter+1) t else (counter,current)::(aux () 1 t)
    in aux () 1 l;;
*)


let encode = function
    | [] -> []
    | hl::tl -> let rec aux current counter = function
        | [] -> [(counter,current)]
        | h::t -> if h = current
        then aux current (counter+1) t
        else (counter,current)::(aux h 1 t)
    in aux hl 1 tl;;


encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let encode1 list = List.map (fun l -> (List.length l, List.hd l)) (pack list);;


encode1 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let encode2 list =
    let rec aux count acc = function
        | [] -> []
        | [x] -> (count,x) :: acc
        | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t else (count,a)::(aux 1 acc t)
    in List.rev (aux 1 [] list);;


encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


type 'a rle =
  | One of 'a
  | Many of int * 'a;;


let encodemod list =
    let rec aux count acc = function
        | [] -> []
        | [x] -> (if count = 1 then One x else Many (count,x))::acc
        | a::(b::_ as t) -> if a = b
        then aux (count+1) acc t
        else begin
            if count = 1 then One a else (Many (count, a))
        end::(aux 1 acc t)
    in aux 1 [] list;;


encodemod ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let rec createmany c x = if c = 0 then [] else x::(createmany (c-1) x);;


let rec decode = function
    | [] -> []
    | hd::tl -> match hd with
        | One x -> x::(decode tl)
        | Many (c,x) -> (createmany c x)@(decode tl);;


decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;


let rec decode list =
    let rec many acc count x = if count = 0 then acc else many (x::acc) (count-1) x
    in let rec aux acc = function
        | [] -> acc
        | hd::tl -> begin match hd with
            | One x -> aux (x::acc) tl
            | Many (c,x) -> aux (many acc c x) tl
        end
    in aux [] (List.rev list);;


decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;


let encode list =
    let rle count x = if count = 0 then One x else Many (count,x)
    in let rec aux acc count = function
        | [] -> []
        | [x] -> rle count x :: acc
        | a::(b::_ as t) -> if a = b then aux acc (count + 1) t else aux (rle count a :: acc) 0 t
    in List.rev (aux [] 0 list);;


encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let rec duplicate = function
    | [] -> []
    | hd :: tl -> hd :: hd :: duplicate tl;;


duplicate ["a"; "b"; "c"; "c"; "d"];;


let replicate list count =
    let rec many acc currcount maxcount x = if currcount >= maxcount then acc else many (x::acc) (currcount+1) maxcount x
    in let rec aux acc c = function
        | [] -> acc
        | hd :: tl -> aux (many acc 0 c hd) c tl
    in aux [] count (List.rev list);;


replicate ["a"; "b"; "c"] 3;;


let drop list n =
    let rec aux acc x = function
        | [] -> acc
        | hd :: tl -> begin
            if x >= n
            then aux acc 1 tl
            else aux (hd::acc) (x+1) tl
        end
    in List.rev(aux [] 1 list);;


drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


let rec split list n =
    let rec aux acc i = function
        | [] -> List.rev acc, []
        | h::t as l -> if i <= 0 then List.rev acc, l else aux (h::acc) (i-1) t
    in aux [] n list;;


split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


let slice list s e =
    let rec aux acc i = function
        | [] -> []
        | h::t -> begin
            if i < s
            then aux acc (i+1) t
            else
                if i <= e
                then aux (h::acc) (i+1) t
                else acc
        end
    in List.rev (aux [] 0 list);;


slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;


let rec remove_at n = function
    | [] -> []
    | hd::tl -> if n <= 0 then tl else hd :: remove_at (n-1) tl;;


remove_at 1 ["a"; "b"; "c"; "d"];;


let rotate list n =
    let rec aux acc l i =
        match l with
        | [] -> l @ List.rev acc
        | hd :: tl -> begin
            if i < n
            then aux (hd::acc) tl (i+1)
            else l @ List.rev acc
        end
    in aux [] list 0;;


rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;


let rec insert_at x n = function
    | [] -> [x]
    | hd :: tl as l -> if n <= 0 then x :: l else hd :: insert_at x (n-1) tl;;


insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;


let rec range lb ub =
    if lb = ub
    then [lb]
    else
        if lb < ub
        then lb :: range (lb + 1) ub
        else ub :: range lb (ub - 1);;


range 3 6;;


(** better, tail recursive version ^^ *)
let range lb ub =
    let rec aux acc i j =
        if i = j
        then i::acc
        else
            if i < j
            then aux (j::acc) i (j-1)
            else aux (i::acc) (i-1) j
    in aux [] lb ub;;


range 3 6;;


(** get n random elements of list *)
let rand_select list n =
    Random.init 0;
    let rec contains x = function
        | [] -> false
        | hd :: tl -> if x = hd then true else contains x tl
    in
    let rec choosedifferent acc i cap = begin
        if i <= 0 then acc else
            let chosen=(Random.int cap)
            in begin
                if contains chosen acc then choosedifferent acc i cap
                else choosedifferent (chosen::acc) (i-1) cap
            end
    end
    in
    let rec at i l = if i <= 0 then List.hd l else at (i-1) (List.tl l)
    in
    let len =
        List.length list
    in
    let rec getfromindices acc indices list =
        match indices with
        | [] -> acc
        | hd :: tl -> getfromindices ((at hd list)::acc) (List.tl indices) list
    in getfromindices [] (choosedifferent [] n len) list;;


rand_select [12; 1; 6; 44; 123431234; 441; 1212; 5] 3;;


let lotto_select n ub =
    let rec contains x = function
        | [] -> false
        | hd :: tl -> if x = hd then true else contains x tl
    in let rec aux acc = function
        | 0 -> acc
        | c -> let rd = Random.int ub in if contains rd acc
            then aux acc c
            else aux (rd::acc) (c-1)
    in aux [] n;;


lotto_select 6 49;;


lotto_select 49 49;;


(** this one needs to be terminated with ctl-c *)
lotto_select 50 49;;


let permutation list = rand_select list (List.length list);;


permutation ["a"; "b"; "c"; "d"; "e"; "f"];;


(** generate all permutations of list *)
let rec extract2 = function
    | [] -> []
    | hd :: tl -> (List.map (fun x -> [hd;x]) tl) @ (extract2 tl);;


extract2 ["a"; "b"; "c"; "d"];;


let rec extract n list =
    let rec aux currpart i list =
        if i <= 0
        then [currpart]
        else match list with
            | [] -> []
            | hd :: tl -> aux currpart i tl @ aux (hd::currpart) (i-1) tl
    in aux [] n list;;


extract 2 ["a"; "b"; "c"; "d"];;


extract 3 ["a"; "b"; "c"; "d"];;


List.length (extract 2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"]) = 21;;


(** n: amount items in groups, k: amount groups *)
(** implementation -> (1..9) yields (1..) as well as (9..1), which is the same *)
(** solution: first generate all distinct subgroups of length n, then pick k groups *)
let rec group list n k =
    let rec aux groups currpart discarded remgrouplen groupsleft l =
        if groupsleft = 0 then [groups]
        else
            if remgrouplen = 0
            then aux (currpart::groups) [] [] n (groupsleft-1) (l@discarded)
            else
                match l with
                | [] -> []
                | hd ::tl -> begin
                    aux groups currpart (hd::discarded) remgrouplen groupsleft tl
                end @ begin
                    aux groups (hd::currpart) discarded (remgrouplen-1) groupsleft tl
                end
    in aux [] [] [] n k list;;


group [1;2;3;4;] 2 2;;


let rec group list sizes =
    let rec aux groups currpart discarded remgrouplen remsizes l =
        if remgrouplen = 0
        then
            match remsizes with
            | [] -> [List.rev (currpart::groups)]
            | hd :: tl -> aux (currpart::groups) [] [] hd tl (l@discarded)
        else
            match l with
            | [] -> []
            | hd ::tl -> begin
                aux groups currpart (hd::discarded) remgrouplen remsizes tl
            end @ begin
                aux groups (hd::currpart) discarded (remgrouplen-1) remsizes tl
            end
    in match sizes with
    | [] -> []
    | hd :: tl -> aux [] [] [] hd tl list;;


group ["a"; "b"; "c"; "d"] [2; 1];;


let rec test = List.fold_left

let hai = List.fold_left (fun x y -> let () = (Printf.printf "%d %d\n" x y) in y) 0 [2;4;6];;


let length_sort list =
    let rec length_sorted_insert l x =
        match l with
        | [] -> [x]
        | hd::tl ->
                if List.length hd > List.length x
                then x::hd::tl
                else hd::(length_sorted_insert tl x)
    in List.fold_left length_sorted_insert [] list;;


length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;


let frequency_sort list =
    let rec insert l llen = function
        | [] -> [1,llen,[l]]
        | (c,x,lol) as hd :: tl ->
                if llen < x
                then (1,llen,[l])::hd::tl
                else
                    if llen = x
                    then (c+1,x,(l::lol))::tl
                    else hd::(insert l llen tl)
    in let stage1 = List.fold_left (fun acc x -> insert x (List.length x) acc) [] list
    in let rec insert_sorted lc lx l = function
        | [] -> [lc,lx,l]
        | (c,_,_) as hd :: tl ->
                if c < lc
                then (lc,lx,l)::hd::tl
                else hd::(insert_sorted lc lx l tl)
    in let stage2 = List.fold_left (fun acc (lc,lx,l) -> insert_sorted lc lx l acc) [] stage1
    in let rec only_lists acc = function
        | [] -> acc
        | (_,_,l) :: tl -> only_lists (List.rev l@acc) tl
    in only_lists [] stage2;;


frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "f"];
                ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;


let lazy_and p1 p2 = match p1,p2 with
    | lazy false, _ -> false
    | lazy true, lazy b -> b;;


let is_prime n =
    let rec divides i =
        if i = n then true else if n mod i = 0 then false else divides (i+1)
    in lazy_and (lazy (n <> 1)) (lazy (divides 2));;


(** tried sieve, but inefficient memory usage (requires O(n) space)*)
let is_prime1 n =
    let sieve = Array.make (n-2) true in
    let last_index = n-3 in
    let rec filter_val i = (
        if i <= last_index
        then (sieve.(i-2) <- false; filter_val (2*i))
        else ()) in
    let rec divides i =
        if i = n then true
        else
            if sieve.(i-2)
            then begin
                if n mod i = 0
                then false
                else (filter_val i; divides (i+1))
            end
            else divides (i+1) in
    lazy_and (lazy (n <> 1)) (lazy (divides 2));;


not (is_prime 1);;


is_prime 7;;


not (is_prime 12);;


let rec gcd n m = let i = n mod m in if i = 0 then m else gcd m i;;


gcd 13 27;;


gcd 20536 7826;;


let coprime n m = gcd n m = 1;;


coprime 13 27;;


not (coprime 20536 7826);;


let phi n =
    let rec aux acc i =
        if i = n
        then acc
        else
            let newacc = if coprime i n then acc + 1 else acc
            in aux newacc (i+1)
    in aux 0 1;;


phi 10;;


let factors n =
    let rec aux acc currn i =
        if currn < i then acc else
            if currn mod i = 0 then aux (i::acc) (currn/i) i
            else aux acc currn (i+1)
    in List.rev (aux [] n 2);;


factors 315;;


let factors n =
    let rec count_dividing a b acc = if a mod b = 0 then count_dividing (a/b) b (acc+1) else acc,a
    in let rec gen_factorlist accl i currn =
        if i > n || currn = 1
        then accl
        else
            if is_prime i
            then
                let c,newn = count_dividing currn i 0 in
                if c <> 0
                then gen_factorlist ((i,c)::accl) (i+1) newn
                else gen_factorlist accl (i+1) currn
            else gen_factorlist accl (i+1) currn
        in List.rev (gen_factorlist [] 2 n);;


factors 315;;


let rec pow n m = if m < 1 then 1 else n * pow n (m-1);;


let phi_improved n =
    let rec aux acc = function
        | [] -> acc
        | (p, m) :: tl -> aux (acc * (p - 1) * pow p (m - 1)) tl
    in aux 1 (factors n);;


phi_improved 10;;


let timeit f x =
    let t0 = Unix.gettimeofday ()
    in ignore (f x);
    let t1 = Unix.gettimeofday ()
    in t1 -. t0;;


let all_primes n m =
    let rec aux acc i =
        if n <= i
        then
            if is_prime i
            then aux (i::acc) (i-1)
            else aux acc (i-1)
        else acc
    in aux [] m;;


List.length (all_primes 2 7920);;


let goldbach n =
    let exception NO in
    let prime_list = List.rev (all_primes 2 n) in
    let rec checkfori i = function
        | [] -> raise NO
        | hd :: tl -> if i + hd = n then hd,i else checkfori i tl in
    let rec aux = function
        | [] -> raise NO
        | hd :: tl -> begin
            match checkfori hd prime_list with
            | exception NO -> aux tl
            | s -> s
            end
    in
    aux prime_list;;


goldbach 28;;


let goldbach_list n m =
    let rec aux acc i =
        if m < i then acc
        else aux ((i,goldbach i)::acc) (i+2)
    in let n_even = if n mod 2 = 0 then n else n + 1
    in aux [] n_even;;


goldbach_list 9 20;;


type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;


let lazy_or p1 p2 = match p1,p2 with
    | lazy true, _ -> true
    | lazy false, lazy x -> x;;


let table2 var_a var_b exp =
    let rec aux val_a val_b = function
        | Var x ->
            if x = var_a then val_a
            else if x = var_b then val_b
            else raise (Invalid_argument "unknown var in expression")
        | Not e -> not (aux val_a val_b e)
        | And (e1,e2) -> lazy_and (lazy (aux val_a val_b e1)) (lazy (aux val_a val_b e2))
        | Or (e1,e2) -> lazy_or (lazy (aux val_a val_b e1)) (lazy (aux val_a val_b e2))
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
        | And (e1,e2) -> lazy_and (lazy (aux vars_vals e1)) (lazy (aux vars_vals e2))
        | Or (e1,e2) -> lazy_or (lazy (aux vars_vals e1)) (lazy (aux vars_vals e2))
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
    | Node (_,la,ra), Node (_,lb,rb) -> lazy_and (lazy (is_mirror la lb)) (lazy (is_mirror ra rb))
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
