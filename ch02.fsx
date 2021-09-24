type shish_kebob =
    | Skewer
    | Onion of shish_kebob
    | Lamb of shish_kebob
    | Tomato of shish_kebob

let rec only_onions =
    function
    | Skewer -> true
    | Onion x -> only_onions x
    | Lamb _ -> false
    | Tomato _ -> false

only_onions (Onion(Onion Skewer))
only_onions (Onion(Lamb Skewer))

let rec is_vegetarian =
    function
    | Skewer -> true
    | Onion x -> is_vegetarian x
    | Lamb _ -> false
    | Tomato x -> is_vegetarian x

is_vegetarian (Onion(Onion Skewer))
is_vegetarian (Onion(Lamb Skewer))

type shish<'a> =
    | Bottom of 'a
    | Onion of shish<'a>
    | Lamb of shish<'a>
    | Tomato of shish<'a>

type rod =
    | Dagger
    | Fork
    | Sword

type plate =
    | Gold_plate
    | Silver_plate
    | Brass_plate

Onion(Tomato(Bottom Dagger))
Onion(Tomato(Bottom Gold_plate))

let rec is_veggie =
    function
    | Bottom _ -> true
    | Onion x -> is_veggie x
    | Lamb _ -> false
    | Tomato x -> is_veggie x

is_veggie (Onion Fork)
is_veggie (Onion(Bottom(Fork)))
is_veggie (Onion(Tomato(Bottom(Dagger))))

let rec what_bottom =
    function
    | Bottom x -> x
    | Onion x -> what_bottom x
    | Lamb x -> what_bottom x
    | Tomato x -> what_bottom x

what_bottom (Bottom Sword)
what_bottom (Onion(Tomato(Bottom Dagger)))
what_bottom (Tomato(Onion(Lamb(Bottom 52))))
