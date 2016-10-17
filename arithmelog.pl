:- module(arithmelog, [
                       unsafe_label/1,
                       unsafe_indomain/1,
                       prime/1,
                       composite/1,
                       even/1,
                       odd/1,
                       sum/2,
                       product/2,
                       range/3,
                       factorial/2,
                       divisible_by/2,
                       nth_root/3,
                       floored_sqrt/2,
                       polygonal_number/2
                      ]).

:- use_module(library(clpfd)).

:- multifile clpfd:run_propagator/2.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Utils from module reif
   Credits to Ulrich Neumerkel
   See: http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/sicstus/reif.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
if_(If_1, Then_0, Else_0) :-
    call(If_1, T),
    (   T == true -> Then_0
    ;   T == false -> Else_0
    ;   nonvar(T) -> throw(error(type_error(boolean,T),
                                 type_error(call(If_1,T),2,boolean,T)))
    ;   throw(error(instantiation_error,instantiation_error(call(If_1,T),2)))
    ).

=(X, Y, T) :-
    (   X == Y -> T = true
    ;   X \= Y -> T = false
    ;   T = true, X = Y
    ;   T = false,
        dif(X, Y)
    ).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   has_constraint/2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
has_constraint(N, C) :-
    get_attr(N, clpfd, Cs),
    compound_name_arguments(Cs, clpfd_attr, Z),
    maplist(=.., Z, W),
    member([fd_props|T], W),
    Constraint =.. [C,N],
    member(E, T),
    member(propagator(Constraint,_), E).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   doesnt_have_constraint/2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
doesnt_have_constraint(N, C) :-
    get_attr(N, clpfd, Cs),
    compound_name_arguments(Cs, clpfd_attr, Z),
    maplist(=.., Z, W),
    member([fd_props|T], W),
    Constraint =.. [C,N],
    \+ (member(E, T),
        member(propagator(Constraint,_), E)
    ).



%% unsafe_label(+Vars)
%
% Like label/1, but also works for variables that have an infinite domain.

unsafe_label(L) :-
    must_be(list, L),
    maplist(unsafe_indomain, L).


%% unsafe_indomain(+Var)
%
% Like indomain/1, but also works for variables that have an infinite domain.

unsafe_indomain(X) :-
    fd_inf(X, Inf0),
    fd_sup(X, Sup0),
    maplist(limit_pure, [Inf0,Sup0], [Inf,Sup]),
    unsafe_indomain_(Inf, Sup, X).

limit_pure(inf, inf) :- !.
limit_pure(sup, sup) :- !.
limit_pure(N, n(N))  :- !.

unsafe_indomain_(inf, Sup, X) :-
    infinite_down(Sup, X).
unsafe_indomain_(n(Low), Sup, X) :-
    unsafe_up_(Sup, Low, X).

infinite_down(sup, X) :-
    (   X = 0 
    ;   abs(X) #= abs(N),
        positive_integer(N),
        ( X #= N ; X #= -N )
    ).
infinite_down(n(Up), X ) :-
    (   between(0, Up, X)
    ;   abs(X) #= abs(N),
        positive_integer(N),
        X #= -N
    ).

unsafe_up_(sup, Low, X) :-
    (   between(Low, 0, X)
    ;   positive_integer(X)
    ).
unsafe_up_(n(Up), Low, X) :- between(Low, Up, X).

% See: http://stackoverflow.com/a/39259871/2554145
positive_integer(I) :-
    I #> 0,
    (   var(I) ->
        fd_inf(I, Inf),
        (   I #= Inf
        ;   I #\= Inf,
            positive_integer(I)
        )
    ;   true
    ).


%% prime(?Int)
%
% Int is a prime number.

prime(N) :-
    clpfd:make_propagator(prime(N), Prop),
    clpfd:init_propagator(N, Prop),
    clpfd:trigger_once(Prop).

clpfd:run_propagator(prime(N), MState) :-
    (   integer(N) ->
        clpfd:kill(MState),
        check_prime(N)
    ;   has_constraint(N, even) ->
        clpfd:kill(MState),
        N = 2
    ;   doesnt_have_constraint(N, composite),
        clpfd:fd_get(N, ND, NL, NU, NPs),
        clpfd:cis_max(NL, n(2), NNL),
        clpfd:update_bounds(N, ND, NPs, NL, NU, NNL, NU)
    ).

check_prime(N) :-
    (   N = 2 ->
        true
    ;   N #> 2,
        floored_sqrt(N, SNF),
        SN #= SNF + 1,
        check_prime_1(N, SN, 2, [], [_])
    ).

check_prime_1(1, _, _, L, L) :- !.
check_prime_1(N, SN, D, L, LF) :-
    (   0 #= N mod D ->
        false
    ;   D1 #= D+1,
        (   D1 #> SN ->
            LF = [N|L]
        ;   check_prime_2(N, SN, D1, L, LF)
        )
    ).

check_prime_2(1, _, _, L, L) :- !.
check_prime_2(N, SN, D, L, LF) :-
    (   0 #= N mod D -> false
    ;   D1 #= D+2,
        (   D1 #> SN ->
            LF = [N|L]
        ;   check_prime_2(N, SN, D1, L, LF)
        )
    ).


%% composite(?Int)
%
% Int is a composite number, i.e. a positive number with a least one
% divisor other than 1 and itself.

composite(N) :-
    clpfd:make_propagator(composite(N), Prop),
    clpfd:init_propagator(N, Prop),
    clpfd:trigger_once(Prop).

clpfd:run_propagator(composite(N), MState) :-
    (   integer(N) ->
        clpfd:kill(MState),
        \+ check_prime(N)
    ;   clpfd:fd_get(N, ND, NL, NU, NPs),
        doesnt_have_constraint(N, prime),
        (   has_constraint(N, odd) ->
            clpfd:cis_max(NL, n(5), NNL)
        ;   clpfd:cis_max(NL, n(4), NNL)
        ),
        clpfd:update_bounds(N, ND, NPs, NL, NU, NNL, NU)
    ).


%% even(?Int)
%
% Int is an even number.

even(N) :-
    clpfd:make_propagator(even(N), Prop),
    clpfd:init_propagator(N, Prop),
    clpfd:trigger_once(Prop).

clpfd:run_propagator(even(N), MState) :-
    (   integer(N) ->
        clpfd:kill(MState),
        N #= 2*_
    ;   has_constraint(N, prime) ->
        clpfd:kill(MState),
        N = 2
    ;   doesnt_have_constraint(N, odd)
    ).


%% odd(?int)
%
% Int is an odd number.

odd(N) :-
    clpfd:make_propagator(odd(N), Prop),
    clpfd:init_propagator(N, Prop),
    clpfd:trigger_once(Prop).

clpfd:run_propagator(odd(N), MState) :-
    (   integer(N) ->
        clpfd:kill(MState),
        N #= 2*_ + 1
    ;   doesnt_have_constraint(N, even),
        (   has_constraint(N, prime) ->
            clpfd:fd_get(N, ND, NL, NU, NPs),
            clpfd:cis_max(NL, n(3), NNL),
            clpfd:update_bounds(N, ND, NPs, NL, NU, NNL, NU)
        ;   has_constraint(N, composite) ->
            clpfd:fd_get(N, ND, NL, NU, NPs),
            clpfd:cis_max(NL, n(5), NNL),
            clpfd:update_bounds(N, ND, NPs, NL, NU, NNL, NU)
        ;   true
        )
    ).


%% sum(+Vars, ?Sum)
%
% Sum is the sum of the elements in Vars.

sum(L, S) :-
    sum(L, #=, S).


%% product(?Vars, ?Prod)
%
% Prod is the product of the elements in Vars

product(L, P) :-
    if_(L = [],
        P = 1,
        (   L = [H|T],
            if_(P = 0,
                true,
                (   abs(Q) #=< abs(P),
                    abs(H) #=< abs(P)
                )
            ),
            P #= H*Q,
            product(T, Q)
        )
    ).


%% range(?Inf, ?Sup, ?R)
%
% R is the list of integers between and including Inf and Sup.
% Inf must be less than or equal to Sup.

range(I, S, [I|R]) :-
    I #=< S,
    if_(I = S,
        R = [],
        (   J #= I + 1,
            range(J, S, R)
        )
    ).


%% factorial(?Int, ?Fact)
%
% Fact is the factorial of Int.

factorial(N, F) :-
    F #> 0,
    N #>= 0,
    if_(N = 0,
        F = 1,
        (   N #> 0,
            M #= N - 1,
            F #= G * N,
            factorial(M, G)
        )
    ).


%% divisible_by(?Int, ?Div)
%
% Int is divisible by Div. Div cannot be 0 even if Int is 0.

divisible_by(N, D) :-
    dif(D, 0),
    N #= D*_.


%% nth_root(?Int, ?N, ?Root)
%
% Root is the N-th root of Int.

nth_root(N, I, R) :-
    N #= R^I.


%% floored_sqrt(+Int, ?Root)
%
% Root is the biggest integer which when squared is smaller than Int.
% Int must be ground.

floored_sqrt(N, Root) :-
    must_be(integer, N),
    Max in 0..N,
    R^2 #= Max,
    fd_sup(R, Root).


%% polygonal_number(?Side, ?Int)
%
% Int is a Side-gonal number.

polygonal_number(S, 1) :-
    S in 3..sup.
polygonal_number(S, N) :-
    I in 0..sup,
    S in 3..sup,
    N in 1..sup,
    I #=< N,
    S #=< N,
    N #= (I*I*(S - 2) - I*(S - 4))//2.
