pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [Rs|_]) :- pertenece_m(X, Rs).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|L1], L2) :- invierte(L1, M), concatena(M, [X], L2).
