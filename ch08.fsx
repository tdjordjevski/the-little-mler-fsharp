type list<'a> =
    | Empty
    | Cons of 'a * list<'a>

type orapl =
    | Orange
    | Apple

let rec subst =
    function
    | (_, _, _, Empty) -> Empty
    | (rel, n, a, Cons (e, t)) ->
        if rel a e then
            Cons(n, subst (rel, n, a, t))
        else
            Cons(e, subst (rel, n, a, t))

subst ((=), 11, 15, Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))
subst ((<), 11, 15, Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))

let in_range (small, large) x = small < x && x < large

subst (in_range, 22, (11, 16), Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))

let rec subst_pred =
    function
    | (pred, n, Empty) -> Empty
    | (pred, n, Cons (e, t)) ->
        if pred e then
            Cons(n, subst_pred (pred, n, t))
        else
            Cons(e, subst_pred (pred, n, t))

subst_pred ((=) 15, 11, Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))
subst_pred ((<) 15, 11, Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))
subst_pred (in_range (11, 16), 22, Cons(15, Cons(6, Cons(15, Cons(17, Cons(15, Cons(8, Empty)))))))

let rec combine =
    function
    | Empty, l2 -> l2
    | Cons (a, l1), l2 -> Cons(a, combine (l1, l2))

combine (Cons(1, Cons(2, Cons(3, Empty))), Cons(5, Cons(4, Cons(7, Cons(9, Empty)))))
combine (Cons(1, Cons(2, Cons(3, Empty))), Cons(12, Cons(11, Cons(5, Cons(7, Empty)))))

let rec combine_c a b =
    match a, b with
    | Empty, l2 -> l2
    | Cons (a, l1), l2 -> Cons(a, combine (l1, l2))

combine_c (Cons(1, Cons(2, Cons(3, Empty)))) (Cons(5, Cons(4, Cons(7, Cons(9, Empty)))))
