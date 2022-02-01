%% 2 Lógica proposicional

% Variables True:
eval(var(true)).
eval(and(A,B)) :- eval(A), eval(B).
eval(or(A,B)) :- (eval(A);eval(B)).
eval(not(A)) :- eval2(A).
eval(imp(A,B)) :- eval(or(not(A),B)).
eval(syss(A,B)) :- eval(and(imp(A,B),imp(B,A))).

% Variables False:
eval2(var(false)).
eval2(and(A,_B)) :- eval2(A).
eval2(and(_A,B)) :- eval2(B).
eval2(not(A)) :- eval(A).
eval2(or(A,B)) :- eval2(A), eval2(B).
eval2(imp(A,B)) :- eval2(or(not(A),B)).
eval2(syss(A,B)) :- eval2(and(imp(A,B),imp(B,A))).

form_equiv(A,B) :- \+ eval2(syss(A, B)).

% 3. Algoritmo Hao Wang
elem(X, [X|_]).
elem(_, []) :- false.
elem(X, [_|S]) :- elem(X,S).

intersect([], _) :- false.
intersect([X|S], [Y|T]) :- elem(X, [Y|T]); intersect(S, [Y|T]).

delete(X,[H|T],L) :- rm(X, [H|T], NL), NL == L.

rm( _, [], []).
rm( X, [X|T], T).
rm( X, [H|T], [H|T2]) :- rm(X, T, T2).

% Regla básica
reglas(G,D):- intersec(G,D).

% 2, sobre G
reglas(G,D):- elem(not(X),G), delete(not(X),G,G2), reglas(G2,[X|D]). 

% 3, sobre D
reglas(G, D):- elem(not(X),D), delete(not(X),D,D2), reglas([X|G],D2).

% 4, Sobre G
reglas(G,D):- elem(and(X,Y),G), delete(and(X,Y),G,G2), reglas([X,Y|G2],D).

% 5, sobre D
reglas(G,D):- elem(or(X,Y),D), delete(or(X,Y),D,D2), reglas(G,[X,Y|D2]).

% 6, sobre G
reglas(G,D):-elem(or(X,Y),G), delete(or(X,Y),G,G2), reglas([X|G2],D),
	     reglas([Y|G2],D).

% 7, D
reglas(G,D):- elem(and(X,Y),D), delete(and(X,Y),D,D2), reglas(G,[X|D2]),
	      reglas(G,[Y|D2]).

%% Para eval
reglas(G,D):- elem(impl(X,Y),G), delete(impl(X,Y),G,G2),
	     reglas([or(not(X),Y)|G2],D).

reglas(G,D):- elem(impl(X,Y),D), delete(impl(X,Y),D,D2),
	       reglas(G,[or(not(X),Y)|D2]).

reglas(G,D):- elem(syss(X,Y),G), delete(syss(X,Y),G,G2),
	       reglas([and(impl(X,Y),impl(Y,X))|G2],D).

reglas(G,D):- elem(syss(X,Y),D), delete(syss(X,Y),D,D2),
		reglas(G,[and(impl(X,Y),impl(Y,X))|D2]).



wang([],[]). % 
wang([G|G2],[D|D2]):- reglas(G,D), wang(G2,D2).

valid(_). 
valid(F) :- wang([],[F]).
