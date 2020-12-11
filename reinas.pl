%reinas
%n=5         0 1 2 3 4
%renglones  [4,2,0,3,1]
%	     ^
%	     4+1=5  4-1=3, 5!=2 && 3!=2 ...
%imprimir tablero
% +---+---+---+
% |   |   | * |
% +---+---+---+

escribeR(_,C,N):-
	C=:=N,!.
escribeR(X,C,N):-
	not(X=C),
	write('   |'),
	C1 is C+1,
	escribeR(X,C1,N).
escribeR(X,C,N):-
	X=:=C,
	C1 is C+1,
	write(' R |'),
	escribeR(X,C1,N).

imprimeR([],N):-
	N4 is N*4,
	imprimeLinea(0,N4),nl,!.
imprimeR([X|Resto],N):-
	N4 is N*4,
	imprimeLinea(0,N4),nl,
	C is 0,write('|'),
	escribeR(X,C,N),nl,
	imprimeR(Resto,N).

imprimeLinea(X,N):-
	X>N,!.
imprimeLinea(X,N):-
	Mod is X mod 4,
	Mod=:=0,
	write('+'),
	X1 is X+1,
	imprimeLinea(X1,N).
imprimeLinea(X,N):-
	Mod is X mod 4,
	not(Mod=0),
	write('-'),
	X1 is X+1,
	imprimeLinea(X1,N).

itera(_,[],_):-!.
itera(X,[Y|Resto],N):-
	X1 is X-N,
	X2 is X+N,
	not(X1=Y),
	not(X2=Y),
	N1 is N+1,
	itera(X,Resto,N1).

checaR([_]):-!.
checaR([X,Y|Resto]):-
	X1 is X-1,
	X2 is X+1,
	not(X1=Y),
	not(X2=Y),
	checaR([Y|Resto]).

checaR2([]):-!.
checaR2([X|Resto]):-
	N is 1,
	itera(X,Resto,N),
	checaR2(Resto).

checa(L,N):-
	permuta(L,Ren),
	checaR2(Ren),nl,
	write(Ren),nl,
	imprimeR(Ren,N),fail.
checa(_,_).

reinas:-
	write('N: '),
	read(N),
	lista0(N,L),
	checa(L,N),!.