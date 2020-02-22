%max integer is 2147483647
%get minimum priority
% [[[1, 50], [2, 20], [3, 30], [4, 45], [5, 60]] ,[[1,2, 6],[2,3, 3], [3,4, 1], [4,5, 500], [2,4, 5], [1,4, 1]]]
root([],[I,Min],[I,Min]) :-!.
root([[Node,P]|T],[_,Min],R) :- P<Min,root(T,[Node,P],R),!.
root([_|T],Min,R) :- root(T,Min,R).

%get priority of node X
priorityOf([[X,P]|_],X,P):-!.
priorityOf([_|T],X, R) :- priorityOf(T,X,R).

%get cost for edge
getcost([[X,Y,C]|_],X,Y,C) :- !.
getcost([[Y,X,C]|_],X,Y,C) :- !.
getcost([_|T],X,Y,R) :- getcost(T,X,Y,R).

%check if there is edge between X and Y
edge(X,Y,[_,E]) :- member([X,Y,_], E).
edge(X,Y,[_,E]) :- member([Y,X,_], E).

%check if there is edge between X and Y without costs
edge_no_cost(X,Y,[_,E]) :- member([X,Y], E).
edge_no_cost(X,Y,[_,E]) :- member([Y,X], E).

%get path between X and Y
path(X,Y,[V,E],Visited,P) :- edge_no_cost(X,Y,[V,E]), 
                             not(member(Y, Visited)), 
                             P = [Y|Visited],!.


path(X,Y,[V,E],Visited,P) :- edge_no_cost(X,Z,[V,E]),
                             not(member(Z, Visited)),
                             path(Z,Y,[V,E],[Z|Visited],P),
                             !.
%populate a list with elements of Y
populate(X,Y,1,[Y|X]):-!.
populate(X,Y,N,[Y|R]) :- Q is N-1 ,populate(X,Y,Q,R).

%initialize distance list
initD([[Node,_]],[[Node,2147483647]]) :-!.
initD([[Node,_]|T],[[Node,2147483647]|R]) :- initD(T,R).

%initialize parent list
initP([[Node,_]],[[Node,-1]]) :-!.
initP([[Node,_]|T],[[Node,-1]|R]) :- initP(T,R).

%initialize the distance and parent lists
initDP(V,D1,P) :- initD(V,D),
					initP(V,P),root(V,[_,2147483647],[Node,_]),
					modify(D,Node,0,D1).

%replace elem  N in list with Y
modify([[N,_]|T],N,Y,[[N,Y]|T]) :- !.
modify([H|T],N,Y, [H|R]) :- modify(T,N,Y,R).

%get elem X from list
getElem([[X,C]|_],X,C) :- !.
getElem([_|T],X,R) :-  getElem(T,X,R).

%gets the node from an edge which is in Arb
inArb([X,_],Arb,X) :- member(X,Arb),!.
inArb([_,Y],Arb,Y) :- member(Y,Arb).

%get minimum edge connecting Arb and the rest
min_edge(_,[],_,Min,_,Min) :- !.
min_edge(V,[[X,Y,C]|T],Arb,[_,_,Min,_],D,R ) :- member(X,Arb),not(member(Y,Arb)),
									edge(X,Y,[_,[[X,Y,C]|T]]),
									getElem(D,X,D1),C1 is D1 + C,
									C1 < Min,
									min_edge(V,T,Arb,[X,Y,C1,C],D,R),!.
min_edge(V,[[X,Y,C]|T],Arb,[_,_,Min,_],D,R ) :- member(Y,Arb),not(member(X,Arb)),
									edge(X,Y,[_,[[X,Y,C]|T]]),
									getElem(D,Y,D1),C1 is D1 + C,
									C1 < Min,
									min_edge(V,T,Arb,[Y,X,C1,C],D,R),!.
min_edge(V,[[X,Y,C]|T],Arb,[M,N,Min,_],D,R) :-member(X,Arb),not(member(Y,Arb)),
									edge(X,Y,[_,[[X,Y,C]|T]]),
									getElem(D,X,D1),C1 is D1 + C,
									C1 is Min,
									priorityOf(V,X,P1),
									inArb([M,N],Arb,N2),priorityOf(V,N2,P2),
									P1 < P2,
									min_edge(V,T,Arb,[X,Y,C1,C],D,R),!.
min_edge(V,[[X,Y,C]|T],Arb,[M,N,Min,_],D,R) :-member(Y,Arb),not(member(X,Arb)),
									edge(X,Y,[_,[[X,Y,C]|T]]),
									getElem(D,Y,D1),C1 is D1 + C,
									C1 is Min,
									priorityOf(V,Y,P1),
									inArb([M,N],Arb,N2),priorityOf(V,N2,P2),
									P1 < P2,
									min_edge(V,T,Arb,[Y,X,C1,C],D,R),!.
min_edge(V,[_|T],Arb,Min,D,R) :- min_edge(V,T,Arb,Min,D,R).

%relaxing an edge
relax([U,V,C],V1,D,P,D1,P1) :- getElem(D,V,R1),getElem(D,U,R2),
					  R3 is R2+C, R1 > R3, modify(D,V,R3,D1),
					  priorityOf(V1,U,PR),
					  modify(P,V,[U,PR],P1),!.
relax([_,_,_],_,D,P,D,P).

%form spanning tree
form([V,_],Arb,D,P,D,P) :- length(V,X),length(Arb,Y), X is Y,!.
form([V,E],Arb,D,P,D3,P3) :- min_edge(V,E,Arb,[_,_,2147483647,_],D,[X,Y,_,C]),
						 relax([X,Y,C],V,D,P,D1,P1),
						 relax([Y,X,C],V,D1,P1,D2,P2),
						 form([V,E],[Y|Arb],D2,P2,D3,P3).

getEdges([],[]):-!.
getEdges([[_,R]|T],Edges) :- R is -1,getEdges(T,Edges),!.
getEdges([[Node,[R,_]]|T],[[R,Node]|Edges]) :-getEdges(T,Edges).


stp([V,E],Root,Edges) :- root(V,[_,2147483647],[Root,_]),
						 initDP(V,D,P),
						 form([V,E],[Root],D,P,_,PF),
						 getEdges(PF,Edges),!.

drum([V,E],S,D,Root,Edges,Path) :- stp([V,E],Root,Edges),
								   path(D,S,[V,Edges],[D],Path).

						 
