% A big-step operational semantics for the integer arithmetic langauge.

% Define the set of terms.
expr(N)        :- number(N).
expr(neg(E))   :- expr(E).
expr(add(L,R)) :- expr(L), expr(R).
expr(mul(L,R)) :- expr(L), expr(R).

% Big-step operational semantics.
eval(N,N)          :- number(N).
eval(neg(E),Res)   :- eval(E,N), Res is -N.
eval(add(L,R),Res) :- eval(L,N), eval(R,M), Res is N+M.
eval(mul(L,R),Res) :- eval(L,N), eval(R,M), Res is N*M.
