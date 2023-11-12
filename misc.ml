let computesquareof x n =
    let a x y = (x+.y)/.2. in
    let h x y = 2.*.x*.y/.(x+.y) in
    let rec aux an bn currn =
        Printf.printf "%d: a %f, b %f\n" (n-currn) an bn;
        if currn <= 0
        then (h an bn, a an bn)
        else aux (h an bn) (a an bn) (currn-1) in
    aux 1. (float_of_int x) n;;


let computesquareof2 n = computesquareof 2 n;;
