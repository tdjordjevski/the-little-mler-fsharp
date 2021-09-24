type list<'a> =
    | Empty
    | Cons of 'a * list<'a>

type box =
    | Bacon
    | Ix of int

let is_bacon =
    function
    | Bacon -> true
    | Ix _ -> false

exception No_bacon of int

let rec where_is =
    function
    | Empty -> raise (No_bacon 0)
    | Cons (a_box, rest) ->
        if is_bacon a_box then
            1
        else
            1 + where_is rest

where_is (Cons(Ix 5, Cons(Ix 13, Cons(Bacon, Cons(Ix 8, Empty)))))
where_is (Cons(Bacon, Cons(Ix 8, Empty)))

try
    where_is (Cons(Ix 5, Cons(Ix 13, Cons(Ix 8, Empty))))
with
| No_bacon an_int -> an_int


exception Out_of_range

let rec list_item =
    function
    | (_, Empty) -> raise Out_of_range
    | (n, Cons (abox, rest)) ->
        if n = 1 then
            abox
        else
            list_item (n - 1, rest)

let rec find n boxes =
    try
        check (n, boxes, list_item (n, boxes))
    with
    | Out_of_range -> find (n / 2) boxes

and check =
    function
    | (n, _, Bacon) -> n
    | (_, boxes, Ix i) -> find i boxes

let t =
    Cons(Ix 5, Cons(Ix 4, Cons(Bacon, Cons(Ix 2, Cons(Ix 7, Empty)))))

find 1 t


let rec path n boxes =
    Cons(
        n,
        try
            check' (boxes, list_item (n, boxes))
        with
        | Out_of_range -> path (n / 2) boxes
    )

and check' =
    function
    | (_, Bacon) -> Empty
    | (boxes, Ix i) -> path i boxes

path 1 t
