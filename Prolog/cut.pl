% Illustrate the effect of a cut.
foo(1).
foo(2).
bar(3,3).
bar(X,Y) :- foo(X), !, foo(Y).
bar(1,4).

% Green cut.
max(X,Y,Y) :- X < Y, !.
max(X,Y,X) :- X >= Y.

% Red cut.
find(X,[X|_]) :- !.
find(X,[_|L]) :- find(X,L).

% Negation as failure.
not(P) :- P, !, fail.
not(_).

% Just for testing.
hobbit(frodo).
hobbit(bilbo).
likes(X,beer) :- hobbit(X).
