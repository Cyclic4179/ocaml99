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
    in getfromindices [] (choosedifferent [] n len) list


(** generate all permutations of list *)
let rec extract n list =
    let rec aux currpart i list = if i <= 0 then [currpart] else
        match list with
        [] -> []
        | hd :: tl -> aux currpart i tl @ aux (hd::currpart) (i-1) tl
    in aux [] n list
