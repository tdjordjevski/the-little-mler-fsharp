type pizza<'a> =
    | Bottom
    | Topping of 'a * pizza<'a>

type fish =
    | Anchovy
    | Lox
    | Tuna

let rec rem_anchovy =
    function
    | Bottom -> Bottom
    | Topping (Anchovy, p) -> rem_anchovy p
    | Topping (t, p) -> Topping(t, rem_anchovy p)

rem_anchovy (Topping(Lox, Topping(Anchovy, Topping(Tuna, Topping(Anchovy, Bottom)))))
rem_anchovy (Topping(Lox, Topping(Tuna, Bottom)))

let rec rem_tuna =
    function
    | Bottom -> Bottom
    | Topping (Tuna, p) -> rem_tuna p
    | Topping (t, p) -> Topping(t, rem_tuna p)

rem_tuna (Topping(Lox, Topping(Anchovy, Topping(Tuna, Topping(Anchovy, Bottom)))))
rem_tuna (Topping(Lox, Topping(Tuna, Bottom)))

let rec rem_fish =
    function
    | x, Bottom -> Bottom
    | Anchovy, Topping (Anchovy, p) -> rem_fish (Anchovy, p)
    | Anchovy, Topping (t, p) -> Topping(t, rem_fish (Anchovy, p))
    | Lox, Topping (Lox, p) -> rem_fish (Lox, p)
    | Lox, Topping (t, p) -> Topping(t, rem_fish (Lox, p))
    | Tuna, Topping (Tuna, p) -> rem_fish (Tuna, p)
    | Tuna, Topping (t, p) -> Topping(t, rem_fish (Tuna, p))


rem_fish (Anchovy, (Topping(Lox, Topping(Anchovy, Topping(Tuna, Topping(Anchovy, Bottom))))))
rem_fish (Tuna, (Topping(Lox, Topping(Tuna, Bottom))))

let rec rem_fish' =
    function
    | x, Bottom -> Bottom
    | x, Topping (t, p) ->
        if t = x then
            rem_fish' (x, p)
        else
            Topping(t, rem_fish' (x, p))

rem_fish' (Anchovy, (Topping(Lox, Topping(Anchovy, Topping(Tuna, Topping(Anchovy, Bottom))))))
rem_fish' (Tuna, (Topping(Lox, Topping(Tuna, Bottom))))

let rec rem_int =
    function
    | x, Bottom -> Bottom
    | x, Topping (t, p) ->
        if t = x then
            rem_int (x, p)
        else
            Topping(t, rem_int (x, p))

rem_int (3, Topping(2, Topping(3, Topping(2, Bottom))))

let rec subst_fish =
    function
    | (n, a, Bottom) -> Bottom
    | (n: fish, a, Topping (t, p)) ->
        if t = a then
            Topping(n, subst_fish (n, a, p))
        else
            Topping(t, subst_fish (n, a, p))

subst_fish (Lox, Anchovy, (Topping(Anchovy, Topping(Tuna, Topping(Anchovy, Bottom)))))

let rec subst_int =
    function
    | (n, a, Bottom) -> Bottom
    | (n: int, a, Topping (t, p)) ->
        if t = a then
            Topping(n, subst_int (n, a, p))
        else
            Topping(t, subst_int (n, a, p))

subst_int (5, 3, (Topping(3, Topping(2, Topping(3, Bottom)))))
