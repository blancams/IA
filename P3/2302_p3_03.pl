/*******************************************
*    Lab assignment 3
*    LAB GROUP:   2302
*    Couple:      3
*    Author 1:    Blanca Martin Selgas
*    Author 2:    Fernando Villar Gomez
*******************************************/

/*******************************************
*    Exercise 1:
*
*    pertenece(X, L)
*      Predicate that evaluates if the element X is contained in the list L.
*      If the element is contained multiple times, a 'true' is returned for
*      each coincidence. When given an uninitialized variable, it returns all
*      of the elements through X.
*      Note: The list L must not contain sublists.
*
*    pertenece_m(X, L)
*      Predicate that performs the same evaluation that 'pertenece', but in
*      this case, L may contain sublists.
*
*/

pertenece(X, [X|_]).
pertenece(X, [_|Rs]) :- pertenece(X, Rs).

pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [Ls|_]) :- pertenece_m(X, Ls).
pertenece_m(X, [_|Rs]) :- pertenece_m(X, Rs).

/*
*    Examples:
*      ?- pertenece(1, [2, 1, 3, 1]).
*      true ;
*      true ;
*      false.
*
*      ?- pertenece(X, [2, 1, 3, 1]).
*      X = 2 ;
*      X = 1 ;
*      X = 3 ;
*      X = 1 ;
*      false.
*
*      ?- pertenece_m(1, [2, [1, 3], [1, [4, 5]]]).
*      true ;
*      true ;
*      false.
*
*      ?- pertenece_m(X, [2, [1, 3], [1, [4, 5]]]).
*      X = 2 ;
*      X = 1 ;
*      X = 3 ;
*      X = 1 ;
*      X = 4 ;
*      X = 5 ;
*******************************************/


/*******************************************
*    Exercise 2:
*
*    concatena(L1, L2, L3)
*      Predicate that evaluates if L3 is concatenation of L1 and L2. When
*      given an uninitialized variable, it returns the concatenation of L1 and
*      L2 through L3.
*
*    invierte(L1, L2)
*      Predicate that evaluates if L2 is the reverse list of L1. When given
*      an uninitialized variable, it returns the reverse list of L1 through L2.
*
*/

concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|L1], L2) :- invierte(L1, M), concatena(M, [X], L2).

/*
*    Examples:
*      ?- concatena([1, 2, 3], [4, 5], X).
*      X = [1, 2, 3, 4, 5]
*
*      ?- invierte([1, 2, 3], X).
*      X = [3, 2, 1]
*******************************************/


/*******************************************
*    Exercise 3:
*
*    insert(X-P, L, R)
*      Predicate that inserts a pair of elements (X-P) in an ordered pair list
*      (L) in the position P, returning the resulting list through R.
*
*/

insert([X-P], [], R) :- concatena([X-P], [], R).
insert([X-P], [Y-Q|Z], R) :-    P < Q,
                                concatena([X-P], [Y-Q], N),
                                concatena(N, Z, R).
insert([X-P], [Y-Q|Z], R) :-    P >= Q,
                                insert([X-P], Z, N),
                                concatena([Y-Q], N, R).

/*
*    Examples:
*      ?- insert([a-6],[p-0, g-7], X).
*      X = [p-0, a-6, g-7] ;
*      false.
*
*      ?- insert([a-6],[p-0, g-7, t-2], X).
*      X = [p-0, a-6, g-7, t-2] ;
*      false.
*******************************************/


/*******************************************
*    Exercise 4.1:
*
*    elem_count(X, L, Xn)
*      Predicate that satisfies when the element X appears Xn times in the list L.
*
*/

elem_count(_, [], _).
elem_count(X, [X|Z], Xn) :- elem_counts(X, Z, N), Xn is N+1.
elem_count(X, [Y|Z], Xn) :- X \= Y, elem_counts(X, Z, Xn).

/*
*    Examples:
*      ?- elem_count(b,[b,a,b,a,b],Xn).
*      Xn = 3 ;
*      false.
*
*      ?- elem_count(a,[b,a,b,a,b],Xn).
*      Xn = 2 ;
*      false.
*******************************************/


/*******************************************
*    Exercise 4.2:
*
*    list_count(L1, L2, L3)
*      Predicate that satisfies when L3 contains the appearances of the elements
*      of L1 in L2, with format [element-appearances] (for example, [b-6]).
*
*/

list_count([], _, []).
list_count([X|Z], L2, L3) :-    list_counts(Z, L2, M),
                                elem_counts(X, L2, C),
                                concatena([X-C], M, L3).

/*
*    Examples:
*      ?- list_count([b],[b,a,b,a,b],Xn).
*      Xn = [b-3] ;
*      false.
*
*      ?- list_count([b,a],[b,a,b,a,b],Xn).
*      Xn = [b-3, a-2] ;
*      false.
*
*      ?- list_count([b,a,c],[b,a,b,a,b],Xn).
*      Xn = [b-3, a-2, c-0] ;
*      false.
*******************************************/


/*******************************************
*    Exercise 5:
*
*    sort_list(L1, L2)
*      Predicate that satisfies when L2 contains the pairs of L1 sorted (as in
*      the previous exercises, with format [element-appearances]).
*
*/

sort_list([], []).
sort_list([X|Z], L2) :- sort_list(Z, N), insert([X], N, L2).

/*
*    Examples:
*      ?- sort_list([p-0, a-6, g-7, t-2], X).
*      X = [p-0, t-2, a-6, g-7] ;
*      false.
*
*      ?- sort_list([p-0, a-6, g-7, p-9, t-2], X).
*      X = [p-0, t-2, a-6, g-7, p-9] ;
*      false.
*
*      ?- sort_list([p-0, a-6, g-7, p-9, t-2, 9-99], X).
*      X = [p-0, t-2, a-6, g-7, p-9, 9-99] ;
*      false.
*******************************************/


/*******************************************
*    Exercise 6:
*
*    build_tree(L, T)
*      Predicate that transforms an ordered list of pairs into a simplified
*      Huffman tree.
*      Note: build_tree uses two auxiliary functions, build_tree_aux (used to
*      keep track of the tree at each step) and add_node (as its name suggests,
*      it is used to add nodes to the tree).
*
*/

add_node(X, nil, tree(X, nil, nil)).
add_node(X, tree(Info, nil, nil), tree(Info, L, nil)) :- add_node(X, nil, L), !.
add_node(X, tree(Info, Left, Right), tree(Info, Left, R)) :- add_node(X, Right, R), !.
build_tree(L, T) :- build_tree_aux(L, T, nil).
build_tree_aux([X-_], T, U) :- add_node(X, U, T).
build_tree_aux([X-_|R], T, U) :-    add_node(1, U, V),
                                    add_node(X, V, W),
                                    build_tree_aux(R, T, W).

/*
*    Examples:
*      ?-build_tree([p-0, a-6, g-7, p-9, t-2, 9-99], X).
*      X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil),
*      tree(1, tree(g, nil, nil), tree(1, tree(p, nil, nil), tree(1,
*      tree(t, nil, nil), tree(9, nil, nil)))))) ;
*      false.
*
*      ?-build_tree([p-55, a-6, g-2, p-1], X).
*      X = tree(1, tree(p, nil, nil), tree(1, tree(a, nil, nil),
*      tree(1, tree(g, nil, nil), tree(p, nil, nil)))) ;
*      false.
*******************************************/


/*******************************************
*    Exercise 7.1:
*
*    encode_elem(X1, X2, Tree)
*      Predicate that encodes (returning the value through X2) the element X1,
*      based on the Huffman tree Tree.
*      Note: encode_elem uses the auxiliary function encode_elem_aux that fills
*      a list in which the encoding of X1 is being saved each step.
*
*/

encode_elem(X1, X2, Tree) :- encode_elem_aux(X1, X2, Tree, []).
encode_elem_aux(E, X, tree(1, tree(E, _, _), _), L) :- concatena(L, [0], X).
encode_elem_aux(E, X, tree(1, tree(A, _, _), tree(E, _, _)), L) :- A \= E,
                                            concatena(L, [1], X).
encode_elem_aux(E, X, tree(1, tree(A, _, _), Right), L) :- A \= E,
                                            concatena(L, [1], M),
                                            encode_elem_aux(E, X, Right, M).

/*
*    Examples:
*      ?-build_tree([a-11, b-6, c-2, d-1], X).
*      X = tree(1, tree(a, nil, nil), tree(1, tree(b, nil, nil),
*      tree(1, tree(c, nil, nil), tree(d, nil, nil)))) ;
*      false.
*
*
*      ?- encode_elem(a, X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [0] ;
*      false.
*
*      ?- encode_elem(b, X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [1, 0] ;
*      false.
*
*      ?- encode_elem(c, X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [1, 1, 0] ;
*      false.
*
*      ?- encode_elem(d, X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [1, 1, 1] ;
*      false.
*******************************************/


/*******************************************
*    Exercise 7.1:
*
*    encode_list(L1, L2, Tree)
*      Performs the same task that encode_elem, but this time using lists of
*      elements and lists of codes.
*      Note: encode_list uses encode_list_aux for the same reasons encode_elem
*      uses encode_elem_aux.
*
*/

encode_list(L1, L2, Tree) :- encode_list_aux(L1, L2, Tree, []).
encode_list_aux([], X, _, L) :- X = L.
encode_list_aux([E|Rs], X, Tree, L) :-  encode_elem(E, F, Tree),
                                        concatena(L, [F], M),
                                        encode_list_aux(Rs, X, Tree, M).

/*
*    Examples:
*      ?- encode_list([a], X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [[0]] ;
*      false.
*
*      ?- encode_list([a, a], X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [[0], [0]] ;
*      false.
*
*      ?- encode_list([a, d, a], X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      X = [[0], [1, 1, 1], [0]] ;
*      false.
*
*      ?- encode_list([a, d, a, q], X, tree(1, tree(a, nil, nil), tree(1,
*         tree(b, nil, nil), tree(1, tree(c, nil, nil), tree(d, nil, nil))))).
*      false.
*******************************************/


/*******************************************
*    Exercise 7.1:
*
*    encode(L1, L2)
*      Predicate that encodes each element of L1 into L2 based on its frequence
*      inside L1.
*      Note: encode uses the predicate dictionary as a data base of the elements
*      we want to allow in L1.
*
*/

dictionary([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z]).

encode(L1, L2) :-   dictionary(D),
                    list_count(D, L1, E),
                    sort_list(E, F),
                    invierte(F, G),
                    build_tree(G, H),
    				encode_list(L1,L2,H).

/*
*    Examples:
*      ?- encode([i,n,t,e,l,i,g,e,n,c,i,a,a,r,t,i,f,i,c,i,a,l], X).
*      X = [[0], [1, 1, 1, 0], [1, 1, 0], [1, 1, 1, 1, 1, 0], [1, 1, 1, 1, 0],
*      [0], [1, 1, 1, 1, 1, 1, 1, 1, 0], [1, 1, 1, 1, 1, 0], [1, 1, 1, 0],
*      [1, 1, 1, 1, 1, 1, 0], [0], [1, 0], [1, 0], [1, 1, 1, 1, 1, 1, 1, 0],
*      [1, 1, 0], [0], [1, 1, 1, 1, 1, 1, 1, 1, 1, 0], [0], [1, 1, 1, 1, 1, 1, 0],
*      [0], [1, 0], [1, 1, 1, 1, 0]] ;
*      false.
*
*      ?- encode([i,a], X).
*      X = [[0], [1, 0]] ;
*      false.
*
*      ?- encode([i,2,a], X).
*      false.
*******************************************/
