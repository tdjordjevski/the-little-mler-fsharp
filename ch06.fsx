type fruit =
    | Peach
    | Apple
    | Pear
    | Lemon
    | Fig

type tree =
    | Bud
    | Flat of fruit * tree
    | Split of tree * tree

let rec flat_only =
    function
    | Bud -> true
    | Flat (_, t) -> flat_only t
    | Split _ -> false


let rec split_only =
    function
    | Bud -> true
    | Flat _ -> false
    | Split (s, t) -> split_only s && split_only t

split_only Bud
split_only (Split(Bud, Flat(Peach, Bud)))
split_only (Split(Split(Bud, Split(Bud, Bud)), Split(Bud, Split(Bud, Bud))))

let rec contain_fruit =
    function
    | Bud -> false
    | Flat _ -> true
    | Split (s, t) -> contain_fruit s || contain_fruit t

let contain_fruit' = not << split_only

contain_fruit Bud
contain_fruit (Split(Bud, Flat(Peach, Bud)))
contain_fruit (Split(Split(Bud, Split(Bud, Bud)), Split(Bud, Split(Bud, Bud))))

contain_fruit' Bud
contain_fruit' (Split(Bud, Flat(Peach, Bud)))
contain_fruit' (Split(Split(Bud, Split(Bud, Bud)), Split(Bud, Split(Bud, Bud))))

open System

let rec height =
    function
    | Bud -> 0
    | Flat (_, t) -> 1 + height t
    | Split (s, t) -> 1 + Math.Max(height s, height t)

height (Split(Split(Bud, Bud), Flat(Fig, Flat(Lemon, Flat(Apple, Bud)))))
height (Split(Bud, Bud))

let rec subst_in_tree =
    function
    | (_, _, Bud) -> Bud
    | (n, a, Flat (f, t)) ->
        if a = f then
            Flat(n, subst_in_tree (n, a, t))
        else
            Flat(f, subst_in_tree (n, a, t))
    | (n, a, Split (s, t)) -> Split(subst_in_tree (n, a, s), subst_in_tree (n, a, t))

subst_in_tree (Apple, Fig, Split(Split(Flat(Fig, Bud), Flat(Fig, Bud)), Flat(Fig, Flat(Lemon, Flat(Apple, Bud)))))

let rec occurs =
    function
    | (_, Bud) -> 0
    | (n, Flat (f, t)) ->
        if n = f then
            1 + occurs (n, t)
        else
            occurs (n, t)
    | (n, Split (s, t)) -> occurs (n, s) + occurs (n, t)


occurs (Fig, Split(Split(Flat(Fig, Bud), Flat(Fig, Bud)), Flat(Fig, Flat(Lemon, Flat(Apple, Bud)))))

type slist<'a> =
    | Empty
    | Scons of sexp<'a> * slist<'a>

and sexp<'a> =
    | An_atom of 'a
    | A_slist of slist<'a>

let rec occurs_in_slist =
    function
    | (_, Empty) -> 0
    | (a, Scons (s, y)) -> occurs_in_sexp (a, s) + occurs_in_slist (a, y)

and occurs_in_sexp =
    function
    | (a, An_atom (b)) -> if a = b then 1 else 0
    | (a, A_slist (y)) -> occurs_in_slist (a, y)


occurs_in_slist (Fig, Scons(An_atom(Fig), Scons(An_atom(Fig), Scons(An_atom(Lemon), Empty))))

occurs_in_slist (
    Fig,
    Scons(A_slist(Scons(An_atom(Fig), Scons(An_atom(Peach), Empty))), Scons(An_atom(Fig), Scons(An_atom(Lemon), Empty)))
)

occurs_in_sexp (Fig, A_slist(Scons(An_atom(Fig), Scons(An_atom(Peach), Empty))))

let rec subst_in_slist =
    function
    | (_, _, Empty) -> Empty
    | (n, a, Scons (s, y)) -> Scons(subst_in_sexp (n, a, s), subst_in_slist (n, a, y))

and subst_in_sexp =
    function
    | (n, a, An_atom b) -> if a = b then An_atom n else An_atom b
    | (n, a, A_slist y) -> A_slist(subst_in_slist (n, a, y))


subst_in_slist (Apple, Fig, Scons(An_atom Fig, Scons(An_atom Fig, Scons(An_atom Lemon, Empty))))

subst_in_slist (
    Apple,
    Fig,
    Scons(A_slist(Scons(An_atom Fig, Scons(An_atom Peach, Empty))), Scons(An_atom Fig, Scons(An_atom Lemon, Empty)))
)

let rec remove_from_slist =
    function
    | (_, Empty) -> Empty
    | (a, Scons (An_atom b, y)) ->
        if a = b then
            remove_from_slist (a, y)
        else
            Scons(An_atom b, remove_from_slist (a, y))
    | (a, Scons (A_slist x, y)) -> Scons(A_slist(remove_from_slist (a, x)), remove_from_slist (a, y))

remove_from_slist (Fig, Scons(An_atom Fig, Scons(An_atom Fig, Scons(An_atom Lemon, Empty))))

remove_from_slist (
    Fig,
    Scons(A_slist(Scons(An_atom Fig, Scons(An_atom Peach, Empty))), Scons(An_atom Fig, Scons(An_atom Lemon, Empty)))
)
