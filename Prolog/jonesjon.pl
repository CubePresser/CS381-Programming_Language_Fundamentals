% Jonathan Jones -- jonesjon 932709446

% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y, X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- parent(X, _), female(X).
isFather(X) :- parent(X, _), male(X).

% 3. Define a predicate `grandparent/2`.
% X is the grandparent, Y is the grandchild
grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(Z, X), parent(Z, Y), X \= Y.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(X, Y) :- sibling(X, Y), male(X).
sister(X, Y) :- sibling(X, Y), female(X).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(X, Z), married(Z, Y).
siblingInLaw(X,Y) :- married(X, Z), sibling(Z, Y).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X, Y) :- parent(Z, Y), sibling(Z, X), female(X).
aunt(X, Y) :- parent(Z, Y), siblingInLaw(Z, X), female(X).
uncle(X, Y) :- parent(Z, Y), sibling(Z, X), male(X).
uncle(X, Y) :- parent(Z, Y), siblingInLaw(Z, X), male(X).

% 8. Define the predicate `cousin/2`.
cousin(X, Y) :- parent(Z, Y), aunt(Z, X).
cousin(X, Y) :- parent(Z, Y), aunt(Z, X).

% 9. Define the predicate `ancestor/2`.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X,Z), ancestor(Z, Y).

% Extra credit: Define the predicate `related/2`.

%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.

cmd(add, [X, Y|L], S)   :- Res is (X + Y), S = [Res|L].
cmd(lte, [X, Y|L], S)   :- Z = (X =< Y -> Res=t; Res=f), call(Z), S=[Res|L].
cmd(if(T, _), [t|Z], S) :- prog(T, Z, S).
cmd(if(_, F), [f|Z], S) :- prog(F, Z, S).
cmd(X, S1, S2)          :- S2 = [X|S1].

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.
prog([],S1,S2)      :- S2 = S1.
prog([X|L], S1, S2) :- cmd(X, S1, S3), prog(L, S3, S2).
