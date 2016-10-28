# Arithmelog
A library of Prolog predicates for CLP(FD) arithmetic.

This module is here to provide simple predicates with descriptive names so that programs dealing with arithmetic become much more readable and declarative. We feel that there is a gap in built-in tools for arithmetic based on CLP(FD).

##How to use

###SWI-Prolog

Arithmelog is self-contained for SWI-Prolog.

###SICStus - No-fully functional

For SICStus, Arithmelog requires [CLP(Z)](https://github.com/triska/clpz) (by Markus Triska), [available here](https://www.metalevel.at/clpz.pl). It also requires `module(reif)` (by Ulrich Neumerkel), [available here](http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/sicstus/reif.pl).

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
 - `perfect_square/1`
 
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
