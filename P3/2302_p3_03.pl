pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [Ls|_]) :- pertenece_m(X, Ls).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|L1], L2) :- invierte(L1, M), concatena(M, [X], L2).

insert([X-P], [], R) :- concatena([X-P], [], R).
insert([X-P], [Y-Q|Z], R) :- P < Q, concatena([X-P], [Y-Q], N), concatena(N, Z, R).
insert([X-P], [Y-Q|Z], R) :- P >= Q, insert([X-P], Z, N), concatena([Y-Q], N, R).

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

add_node(X, nil, tree(X, nil, nil)).
add_node(X, tree(Info, nil, nil), tree(Info, L, nil)) :- add_node(X, nil, L), !.
add_node(X, tree(Info, Left, Right), tree(Info, Left, R)) :- add_node(X, Right, R), !.
build_tree(L, T) :- build_tree_aux(L, T, nil).
build_tree_aux([X-_], T, U) :- add_node(X, U, T).
build_tree_aux([X-_|R], T, U) :-    add_node(1, U, V),
                                    add_node(X, V, W),
                                    build_tree_aux(R, T, W).

encode_elem(X1, X2, Tree) :- encode_elem_aux(X1, X2, Tree, []).
encode_elem_aux(E, X, tree(1, tree(E, _, _), _), L) :- concatena(L, [0], X).
encode_elem_aux(E, X, tree(1, tree(A, _, _), tree(E, _, _)), L) :-  A \= E,
                                                                    concatena(L, [1], X).
encode_elem_aux(E, X, tree(1, tree(A, _, _), Right), L) :- A \= E,
     													   concatena(L, [1], M),
     													   encode_elem_aux(E, X, Right, M).

encode_list(L1, L2, Tree) :- encode_list_aux(L1, L2, Tree, []).
encode_list_aux([], X, _, L) :- X = L.
encode_list_aux([E|Rs], X, Tree, L) :-  encode_elem(E, F, Tree),
                                        concatena(L, [F], M),
                                        encode_list_aux(Rs, X, Tree, M).

dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

encode(L1, L2) :-   dictionary(D),
                    list_count(D, L1, E),
                    sort_list(E, F),
                    invierte(F, G),
                    build_tree(G, H),
    				encode_list(L1,L2,H).
