let identity x = x

identity 4
identity "four"

let true_maker _ = true

true_maker 4
true_maker "four"

type bool_or_int =
    | Hot of bool
    | Cold of int

Hot true
Cold 10

let hot_maker x = Hot

hot_maker 5 true

let help f =
    Hot(true_maker (if true_maker () then f else true_maker))

type chain = Link of int * (int -> chain)

let rec ints n = Link(n + 1, ints)

ints 0
ints 5
ints 13

let rec skips n = Link(n + 2, skips)

skips 8
skips 17

let divides_evenly n c = n % c = 0

let is_mod_5_or_7 n =
    if divides_evenly n 5 then
        true
    else
        divides_evenly n 7

let rec some_ints n =
    if is_mod_5_or_7 (n + 1) then
        Link(n + 1, some_ints)
    else
        some_ints (n + 1)

some_ints 1
some_ints 17
some_ints 116

let rec chain_items n (Link (i, f)) =
    if n = 1 then
        i
    else
        chain_items (n - 1) (f i)

chain_items 1 (some_ints 0)
chain_items 6 (some_ints 0)
chain_items 37 (some_ints 0)

let rec is_prime n = has_no_divisors n (n - 1)

and has_no_divisors n c =
    if c = 1 then true
    else if divides_evenly n c then false
    else has_no_divisors n (c - 1)

let rec primes n =
    if is_prime (n + 1) then
        Link(n + 1, primes)
    else
        primes (n + 1)

chain_items 12 (primes 1)

let rec fibs n m = Link(n + m, fibs m)

fibs 0 1
fibs 1 1
fibs 1 2
chain_items 1 (fibs 0 1)
chain_items 2 (fibs 0 1)
chain_items 3 (fibs 0 1)
chain_items 4 (fibs 0 1)
