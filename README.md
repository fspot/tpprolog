%:- dynamic ouvert/2
% table 7*6

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

maxlist([X],X).
maxlist([X|Xs],X):- max(Xs,Y), X >=Y.
maxlist([X|Xs],N):- max(Xs,N), N > X.

index(X,Y,Pos):-
  Pos is 7*Y + X.

value(T,[X,Y],-1):-
	X < 0;
	X > 7;
	Y < 0;
	Y > 6.

value(T,[X,Y],Val):-
	index(X,Y,Pos), nth0(Pos,T,Val).
	
firstFree(Table,Column,ResultY):-
	firstFree1(Table,Column,0,ResultY).
	
firstFree1(Table,Column,Y,Y):-
	index(Column,Y,Pos), nth0(Pos,Table,0).
	
firstFree1(Table,Column,Y,ResultY):-
	Y1 is Y+1, firstFree1(Table,Column,Y1,ResultY).

colJouables(_,[],0).
	
colJouables(T,[TL|QL],Col):-
	Col2 is Col-1,
	colJouables(T,QL,Col2),
	firstFree(T,Col,Y),
	Y < 6,
	TL = Col.
	
insert(Table,Column,Player,ResultTable,Y):-
	firstFree(Table,Column,Y), index(Column,Y,Pos), replace(Table,Pos,Player,ResultTable).

nbjetons(T, [X,Y], [Vx, Vy], Player, Nb):-
	X2 is X + Vx, Y2 is Y + Vy,
	value(T, [X2, Y2], Player2),
	Player2 \== Player,
	Nb = 0.

nbjetons(T, [X,Y], [Vx, Vy], Player, Nb):-
	X2 is X + Vx, Y2 is Y + Vy,
	value(T, [X2, Y2], Player2),
	Player2 == Player,
	nbjetons(T, [X2, Y2], [Vx, Vy], Player, Nbsuite),
	Nb is 1 + Nbsuite.
	
victory1(T,[X,Y], [Vx, Vy]):- 
	value(T,[X,Y],Player), 
	nbjetons(T, [X,Y], [Vx, Vy], Player, Nba), 
	Vx2 is -Vx, Vy2 is -Vy,
	nbjetons(T, [X,Y], [Vx2, Vy2], Player, Nbb),
	Nbtot is Nba + Nbb,
	Nbtot >= 3.

victory(T, [X,Y]) :-
	victory1(T,[X,Y], [1, 0]);
	victory1(T,[X,Y], [1, 1]);
	victory1(T,[X,Y], [1, -1]);
	victory1(T,[X,Y], [0, 1]).

afficher1([],_).

afficher1([T|Q],0):-
	write('\n'),
	afficher1([T|Q],7).

afficher1([T|Q],I) :-
	I\==0,
	write(T),
	write(' '),
	I2 is I-1,
	afficher1(Q,I2).

afficher(T):-
	reverse(T, T2),
	write('6 5 4 3 2 1 0\n==============\n'),
	afficher1(T2, 7).

tick2J(T, Player, [X,Y]):-
	value(T, [X,Y], V),
	V \== 0,
	victory(T,[X,Y]),
	afficher(T),
	write('victoire du joueur '), write(Player), write(' !\n').
	
tick2J(T, Player, [_,_]):-
	afficher(T),
	write('\n\nEntrez une colonne : '),
	read(Col),
	insert(T, Col, Player, T2, Height),
	Player2 is 3-Player,
	tick2J(T2, Player2, [Col,Height]).
	
minmax(T,Col):-
	min(T,V0,5,0),
	min(T,V1,5,1),
	min(T,V2,5,2),
	min(T,V3,5,3),
	min(T,V4,5,4),
	min(T,V5,5,5),
	min(T,V6,5,6),
	 
%%%%%%%%%%%%%%%%%% MIN
min(T,Val,0,-1):-
	heuristique(T,Val).
	
min(T,Val,Acc,Col):-
	Acc2 is Acc-1,
	ColJouables(T,LCoups),
	min1(T,LCoups,Acc2,Val,Col).
	
min1(_,[],_,9999999999999999999999,-1).
	
min1(T,[MoveT|MoveQ],Acc2,MinVal,MinIndex):-
	insert(T,MoveT,1,T2),
	max(T2,Val1,Acc2,Col),
	min1(T,MoveQ,Acc2,Val2,MinIndex2),
	compareMin(Col,Val,MinIndex2,Val2,MinIndex,MinVal).
	
compareMin(I1,V1,I2,V2,I1,V1):-
	V1=<V2.
	
compareMin(I1,V1,I2,V2,I2,V2):-
	V1>V2.
	
%%%%%%%%%%%%%%%%%% MAX
max(T,Val,0,-1):-
	heuristique(T,Val).
	
max(T,Val,Acc,Col):-
	Acc2 is Acc-1,
	ColJouables(T,LCoups),
	max1(T,LCoups,Acc2,Val,Col).
	
max1(_,[],_,9999999999999999999999,-1).
	
max1(T,[MoveT|MoveQ],Acc2,MaxVal,MaxIndex):-
	insert(T,MoveT,1,T2),
	min(T2,Val1,Acc2,Col),
	max1(T,MoveQ,Acc2,Val2,MaxIndex2),
	compareMax(Col,Val,MaxIndex2,Val2,MaxIndex,MaxVal).
	
compareMax(I1,V1,I2,V2,I1,V1):-
	V1>=V2.
	
compareMax(I1,V1,I2,V2,I2,V2):-
	V1<V2.
	
	
	
	
	
start2J:-
	T = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	tick2J(T, 1,[0,0]).
	
start1J:-
	T = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0].
	
start(X):-
	T = [1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0],
	colJouables(T,X,7).

% get -> nth0


	