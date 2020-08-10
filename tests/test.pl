insere_ordenado(El, [X | R], List):-
    X @>= El,
    List = [El, X | R].

insere_ordenado(El, [], List):-
    List = [El].

insere_ordenado(El, [X | R], List):-
    X @< El,
    insere_ordenado(El, R, List1),
    List = [X|List1].

junta_novo_aleatorio(List1, Lim_inf, Lim_sup, List):-
    random_between(Lim_inf, Lim_sup, N),
    subtract([N], List1, Result),
    Result = [N|[]] ->
    insere_ordenado(N, List1, List);
    junta_novo_aleatorio(List1, Lim_inf, Lim_sup, List).

repete_el(_, 0, []).
repete_el(El, N, L):-
    X is N - 1,
    repete_el(El, X, L1),
    L = [El|L1],
    !.

duplica_elementos([], []).
duplica_elementos([El|R], L):-
    duplica_elementos(R, L1),
    insere_ordenado(El, L1, L2),
    insere_ordenado(El, L2, L).

num_occ([],_, 0).
num_occ([X | R], El, N) :-
    X == El ->
    num_occ(R, El, N1),
    N is N1 + 1;
    num_occ(R, El, N).

substitui_maiores_N(_, _, [], []).
substitui_maiores_N(El, Subst, [X|R], L):-
    X > El,
    substitui_maiores_N(El, Subst, R, L1),
    L = [Subst|L1].

substitui_maiores_N(El, Subst, [X|R], L):-
    X =< El,
    substitui_maiores_N(El, Subst, R, L1),
    L = [X|L1].

perimetro(R, P):-
    P is R * 2 * pi.

divisor(D, N):-
    N mod D =:= 0.

aplica_op(Op, Val1, Val2, R):-
   Op = +,
   R is +(Val1, Val2).

soma_digitos(0,0).
soma_digitos(N, S):-
    X is N // 10,
    Y is N mod 10,
    soma_digitos(X, S1),
    S is S1 + Y,
    !.


inverte(N, R):-
    inverte(N , R, 0).

inverte(0, R, R).
inverte(N, R, Up):-
    X is N // 10,
    Y is N mod 10,
    New is Up*10 + Y,
    inverte(X, R, New),
    !.

triangular(N):-
    triangular(N, 1, 2).

triangular(N, Total, Next):-
    N = Total;
    N > Total, 
    NewT is Total + Next,
    NewN is Next + 1,
    triangular(N, NewT, NewN),
    !.