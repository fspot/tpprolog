%:- dynamic ouvert/2
% table 7*6

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
	write('\n\n6 5 4 3 2 1 0\n==============\n'),
	afficher1(T2, 7).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > 0, I1 is I-1, replace(T, I1, X, R).

maxlist([X],X).
maxlist([X|Xs],X):- max(Xs,Y), X >=Y.
maxlist([X|Xs],N):- max(Xs,N), N > X.

%permet de récupérer la position dans la liste à partir de X,Y.
index(X,Y,Pos):-
  Pos is 7*Y + X.

%permet de récupérer X,Y à partir de l'index de la liste.
rindex(X,Y,Pos):-
  X is Pos mod 7,
  Y is Pos // 7.

%renvoie la couleur du jeton en X,Y; et -1 si en dehors du tableau.
value(T,[X,Y],-1):-
	X < 0;
	X >= 7;
	Y < 0;
	Y >= 6.

value(T,[X,Y],Val):-
	index(X,Y,Pos), nth0(Pos,T,Val).

%renvoie la position en Y de la 1ère case vide dans la colone Column.
firstFree(T,Column,ResultY):-
	firstFree1(T,Column,0,ResultY).
	
firstFree1(T,Column,Y,Y):-
	value(T,[Column,Y],X), X =< 0, !.
	
firstFree1(T,Column,Y,ResultY):-
	Y1 is Y+1, firstFree1(T,Column,Y1,ResultY).

colJouables(_,[],-1):- !.
	
%renvoie la liste des colones (X) dans lesquelles il est encore possible de jouer.
colJouables(T,[TL|QL],Col):-
	Col2 is Col-1,
	firstFree(T,Col,Y),
	Y < 6,
	colJouables(T,QL,Col2),
	TL = Col.

colJouables(T,QL,Col):-
	Col2 is Col-1,
	firstFree(T,Col,Y),
	Y >= 6,
	colJouables(T,QL,Col2).

%insère un jeton de la couleur Player dans la colonne Column. renvoie également la position en Y du jeton inséré.
insert(T,Column,Player,ResultTable,Y):-
	firstFree(T,Column,Y), index(Column,Y,Pos), replace(T,Pos,Player,ResultTable).

%compte le nombre de jetons de la couleur Player, à partir de la position X,Y et dans la direction Vx,Vy.
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

%teste si le jeton inséré en X,Y crée un alignement de 4 (et donc la victoire du joueur l'ayant posé)
victory(T, [X,Y]) :-
	victory1(T,[X,Y], [1, 0]);
	victory1(T,[X,Y], [1, 1]);
	victory1(T,[X,Y], [1, -1]);
	victory1(T,[X,Y], [0, 1]).

%%%%%%%% TICK2J %%%%%%%%%%%
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

%%%%%%%%%%% TICK1J %%%%%%%%%%
tick1J(T, Player, [X,Y]):-
	value(T, [X,Y], V),
	V \== 0,
	victory(T,[X,Y]),
	afficher(T),
	write('victoire du joueur '), write(Player), write(' !\n').
	
tick1J(T, Player, [_,_]):-
	Player == 1,
	afficher(T),
	write('\n\nEntrez une colonne : '),
	read(Col),
	insert(T, Col, Player, T2, Height),
	Player2 is 3-Player,
	tick1J(T2, Player2, [Col,Height]).

tick1J(T, Player, [_,_]):-
	Player == 2,
	afficher(T),
	max(T,_,3,Col),
	insert(T, Col, Player, T2, Height),
	Player2 is 3-Player,
	tick1J(T2, Player2, [Col,Height]).

	 
%%%%%%%%%%%%%%%%%% MIN
min(T,Val,0,-1):-
	heuristique(T,Val).

% choisit la valeur minimum renvoyée par ses fils (Val) et renvoie la Colone correspondante.
min(T,Val,Acc,Col):-
	Acc > 0,
	Acc2 is Acc-1,
	colJouables(T,LCoups,6),
	min1(T,LCoups,Acc2,Val,Col).
	
min1(_,[],_,999999999,-1).

%permet l'appel aux 0..7 fils de min et la récupération de la branche minimisant l'heuristique.
min1(T,[MoveT|MoveQ],Acc2,MinVal,MinIndex):-
	insert(T,MoveT,1,T2,_),
	max(T2,Val1,Acc2,_),
	min1(T,MoveQ,Acc2,Val2,MinIndex2),
	compareMin(MoveT,Val1,MinIndex2,Val2,MinIndex,MinVal).

%permet de comparer V1 et V2, et de récupérer l'index du minimum.
compareMin(I1,V1,I2,V2,I1,V1):-
	V1=<V2.
	
compareMin(I1,V1,I2,V2,I2,V2):-
	V1>V2.
	
%%%%%%%%%%%%%%%%%% MAX
max(T,Val,0,-1):-
	heuristique(T,Val).

% choisit la valeur maximum renvoyée par ses fils (Val) et renvoie la Colone correspondante.
max(T,Val,Acc,Col):-
	Acc > 0,
	Acc2 is Acc-1,
	colJouables(T,LCoups,6),
	max1(T,LCoups,Acc2,Val,Col).
	
max1(_,[],_,-999999999,-1).

%permet l'appel aux 0..7 fils de min et la récupération de la branche maximisant l'heuristique.
max1(T,[MoveT|MoveQ],Acc2,MaxVal,MaxIndex):-
	insert(T,MoveT,2,T2,_),
	min(T2,Val1,Acc2,_),
	max1(T,MoveQ,Acc2,Val2,MaxIndex2),
	compareMax(MoveT,Val1,MaxIndex2,Val2,MaxIndex,MaxVal).

%permet de comparer V1 et V2, et de récupérer l'index du maximum.
compareMax(I1,V1,I2,V2,I1,V1):-
	V1>=V2.
	
compareMax(I1,V1,I2,V2,I2,V2):-
	V1<V2.
	

%%%%%%%%%%%%%%%%%%%%%% HEURISTIQUE SIMPLE %%%%%%%%%%%%%%%%%%%%%%%%

%valeur des alignements. Un alignement de 4 compte comme une alignement de 4 + un alignement de 3 + un alignement de 2.
nbJetonsToVal(0, 0).   % 1 jetons alignes => 0 points
nbJetonsToVal(1, 1).   % 2 => 1
nbJetonsToVal(2, 19).  % 3 => 20
nbJetonsToVal(3, 980). % 4 => 1000.
nbJetonsToVal(N, 980):- N>=4.

%calcule la valeur des alignements à partir de X,Y dans 4 des 8 directions disponibles.
heuristiqueCase(T, [X,Y], Val, PlayerVoulu):-
	value(T,[X,Y],Player), 
	Player \== PlayerVoulu,
	Val is 0.

heuristiqueCase(T, [X,Y], Val, PlayerVoulu):-
	value(T,[X,Y],PlayerVoulu), 
	nbjetons(T, [X,Y], [-1, 1], PlayerVoulu, JetHG), % haut gauche
	nbjetons(T, [X,Y], [0, 1], PlayerVoulu, JetH), % haut
	nbjetons(T, [X,Y], [1, 1], PlayerVoulu, JetHD), % haut droite
	nbjetons(T, [X,Y], [1, 0], PlayerVoulu, JetD), % droite
	nbJetonsToVal(JetHG, ValHG), nbJetonsToVal(JetH, ValH), nbJetonsToVal(JetHD, ValHD), nbJetonsToVal(JetD, ValD),
	Val is ValHG + ValH + ValHD + ValD, !.

heuristique(T,Val):-
	heuristique1(T,41,SumMin,SumMax),
	Val is SumMax - SumMin.

heuristique1(_,-1,0,0).

%évalue une situation de jeu, si >0 avantage à Max, si <0 avantage à Min.
heuristique1(T,Pos,SumMin,SumMax):-
	Pos >= 0,
	PosNext is Pos-1,
	heuristique1(T,PosNext,SumMin2,SumMax2),
	rindex(X,Y,Pos),
	heuristiqueCase(T,[X,Y],ValP1,1),
	heuristiqueCase(T,[X,Y],ValP2,2),
	SumMin is SumMin2+ValP1,
	SumMax is SumMax2+ValP2.
	

%%%%%%%%%%%%%%%%%%%%%%% START %%%%%%%%%%%%%%%%%%%%%%	
start2J:-
	T = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	tick2J(T, 1,[0,0]).
	
start1J:-
	T = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
	tick1J(T, 1, [0,0]).
	
start(Col):-
	T = [1,2,2,0,0,0,0,1,2,0,0,0,0,0,1,0,0,0,0,0,0,  0  ,0,0,0,0,0,0,  0  ,0,0,0,0,0,0,  0  ,0,0,0,0,0,0],
	%nbjetons(T, [0,0], [0,1], 1, X).
	%heuristiqueCase(T,[0,0],P1,1),
	%heuristiqueCase(T,[0,0],P2,2).
	%heuristique1(T,41,P1,P2).
	max(T,_,4,Col).


