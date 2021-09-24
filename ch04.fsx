type meza =
    | Shrimp
    | Calamari
    | Escargots
    | Hummus

type main =
    | Steak
    | Ravioli
    | Chicken
    | Eggplant

type salad =
    | Green
    | Cucumber
    | Greek

type dessert =
    | Sundae
    | Mousee
    | Torte

(Calamari, Ravioli, Greek, Sundae)

let add_a_steak =
    function
    | Shrimp -> (Shrimp, Steak)
    | Calamari -> (Calamari, Steak)
    | Escargots -> (Escargots, Steak)
    | Hummus -> (Hummus, Steak)

add_a_steak Shrimp

let add_a_steak' x = (x, Steak)

let eq_main =
    function
    | Steak, Steak -> true
    | Ravioli, Ravioli -> true
    | Chicken, Chicken -> true
    | Eggplant, Eggplant -> true
    | _ -> false

eq_main (Steak, Chicken)
eq_main (Steak, Steak)

let eq_main' (x, y) = x = y

eq_main (Steak, Chicken)
eq_main (Steak, Steak)

let has_steak =
    function
    | (a: meza, Steak, b: dessert) -> true
    | _ -> false


// has_steak (5, Steak, 6)
// This expression was expected to have type
//     'meza'
// but here has type
//     'int'


has_steak (Shrimp, Steak, Sundae)

has_steak (Shrimp, Chicken, Sundae)

let add_a_steak'' (x: meza) = (x, Steak)
add_a_steak'' Shrimp
