pertenece(X, [X|_]) :-  X \= [_|_].
pertenece(X, [Z|_]) :- pertenece(X, Z).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|Y], L) :- invierte(Y, N), concatena(N, [X], L).

insert([X-P], [], R) :- concatena([X-P], [], R).
insert([X-P], [Y-Q|Z], R) :- P < Q, concatena([X-P], [Y-Q], N), concatena(N, Z, R).
insert([X-P], [Y-Q|Z], R) :- P > Q, insert([X-P], Z, N), concatena([Y-Q], N, R).

elem_count(X, L, Xn) :- elem_count_aux(X, L, Xn, 0).
elem_count_aux(X, [X|Z], Xn, Cnt) :- Ncount is Cnt+1, elem_count_aux(X, Z, Xn, Ncount).
elem_count_aux(X, [Y|Z], Xn, Cnt) :- X \= Y, elem_count_aux(X, Z, Xn, Cnt).
elem_count_aux(_, [], Xn, Cnt) :- Xn = Cnt.

list_count(L1, L2, L3) :- list_count_aux(L1, L2, L3, []).
list_count_aux([X|Z], L2, L3, Laux) :- elem_count(X, L2, C), concatena(Laux, [X-C], N), list_count_aux(Z, L2, L3, N).
list_count_aux([], _, L3, Laux) :- L3 = Laux.

sort_list(L1, L2) :- sort_list_aux(L1, L2, []).
sort_list_aux([X|Z], L2, L) :- insert([X], L, N), sort_list_aux(Z, L2, N).
sort_list_aux([], L2, L) :- L2 = L.


