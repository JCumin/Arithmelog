# Arithmelog
A library of Prolog predicates for CLP(FD) arithmetic.

##Available predicates

###Labeling

 - `unsafe_label/1`
 - `unsafe_indomain/1`
 
 ###Constraints
 
 - `prime/1`
 - `composite/1`
 - `even/1`
 - `odd/1`
 - `divisible_by/2`
 - `polygonal_number/2`
 - `(<)/1`: A list of strictly increasing integers.
 - `(>)/1`: A list of strictly decreasing integers.
 - `(=<)/1`: A list of increasing integers.
 - `(>=)/1`: A list of decreasing integers.
 
 ###Operators
 
 - `sum/2`
 - `product/2`
 - `factorial/2`
 - `nth_root/3`
 - `floored_sqrt/2`
 
 ###Miscellaneous
 
 - `range/3`
 

##Examples

    ?- prime(X), unsafe_indomain(X).
    X = 2 ;
    X = 3 ;
    X = 5 ;
    X = 7 ;
    X = 11 ;
    X = 13 ;
    …
    
    
    ?- prime(X), even(X).
    X = 2.


    ?- composite(X), prime(X).
    false.
    
    
    ?- factorial(X, 720).
    X = 6.


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


    ?- range(I, J, [5,A,B,C,D]).
    I = 5,
    J = D, D = 9,
    A = 6,
    B = 7,
    C = 8 ;
    false.
