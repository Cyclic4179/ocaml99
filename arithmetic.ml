let is_prime n =
    let rec divides i =
        if i = n then true else if n mod i = 0 then false else divides (i+1)
    in (n <> 1) || (divides 2);;


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
    (n <> 1) && (divides 2);;


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
