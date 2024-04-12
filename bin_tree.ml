type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree;;

let cbal_tree n =
    let node l r = Node ('x', l, r)
    in let joinNodes lefts rights all =
        let f list left =
            List.fold_left (fun l right -> (node left right)::l) list rights
        in List.fold_left f all lefts
    in let rec aux nRem =
        if nRem = 0
        then [ Empty ]
        else
            let newN = nRem - 1
            in let half = newN / 2
            in let otherhalf = newN - half
            in if half = otherhalf
            then let childs = aux half in joinNodes childs childs []
            else (
                let somechilds = aux half
                in let otherchilds = aux otherhalf
                in joinNodes somechilds otherchilds (joinNodes otherchilds somechilds [])
            )
    in aux n;;

let rec is_mirror a = function
    | Empty -> a = Empty
    | Node (_, l, r) ->
            match a with
            | Empty -> false
            | Node (_, la, ra) -> is_mirror la l && is_mirror ra r;;

let is_symmetric = function
    | Empty -> true
    | Node (_, l, r) -> is_mirror l r;;

