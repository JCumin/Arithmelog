# Arithmelog
A library of Prolog predicates for CLP(FD) arithmetic.

###Examples

    ?- X #> Y, Y#> Z, prime(X), even(Y), composite(Z), divisible_by(Z, 7), product([X,Y,Z],P), unsafe_label([X,Y,Z]).
    X = 17,
    Y = 16,
    Z = 14,
    P = 3808 ;
    X = 19,
    Y = 16,
    Z = 14,
    P = 4256 ;
    X = 19,
    Y = 18,
    Z = 14,
    P = 4788 ;
    …
