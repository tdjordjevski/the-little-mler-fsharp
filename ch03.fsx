type pizza =
    | Crust
    | Cheese of pizza
    | Onion of pizza
    | Anchovy of pizza
    | Suasage of pizza

let rec remove_anchovy =
    function
    | Crust -> Crust
    | Cheese x -> Cheese(remove_anchovy x)
    | Onion x -> Onion(remove_anchovy x)
    | Anchovy x -> remove_anchovy x
    | Suasage x -> Suasage(remove_anchovy x)

remove_anchovy (Cheese Crust)
remove_anchovy (Anchovy(Onion(Anchovy(Anchovy(Cheese Crust)))))


let rec top_anchovy_with_cheese =
    function
    | Crust -> Crust
    | Cheese x -> Cheese(top_anchovy_with_cheese x)
    | Onion x -> Onion(top_anchovy_with_cheese x)
    | Anchovy x -> Cheese(Anchovy(top_anchovy_with_cheese x))
    | Suasage x -> Suasage(top_anchovy_with_cheese x)

top_anchovy_with_cheese (remove_anchovy (Onion(Anchovy(Cheese(Anchovy Crust)))))
remove_anchovy (top_anchovy_with_cheese (Onion(Anchovy(Cheese(Anchovy Crust)))))

let substitute_anchovy_by_cheese x =
    remove_anchovy (top_anchovy_with_cheese x)

// alternative
// let substitute_anchovy_by_cheese =
//     remove_anchovy >> top_anchovy_with_cheese


substitute_anchovy_by_cheese (Onion(Anchovy(Cheese(Anchovy Crust))))

let rec substitute_anchovy_by_cheese' =
    function
    | Crust -> Crust
    | Cheese x -> Cheese(substitute_anchovy_by_cheese' x)
    | Onion x -> Onion(substitute_anchovy_by_cheese' x)
    | Anchovy x -> Cheese(substitute_anchovy_by_cheese' x)
    | Suasage x -> Suasage(substitute_anchovy_by_cheese' x)


substitute_anchovy_by_cheese' (Onion(Anchovy(Cheese(Anchovy Crust))))
