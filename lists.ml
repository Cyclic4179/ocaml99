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


let rev2 l =
    let rec aux acc = function
        | [] -> acc
        | [x] -> x::a
        | h::t -> aux (h::acc) t in
    aux [] l;;


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
