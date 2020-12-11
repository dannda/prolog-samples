/*
 Programa: varios.pl
 Rutinas varias
*/

%factorial
%%%%%%
factorial(0,1):-!.
factorial(N,F):-
	N>0,
	N1 is N-1,
	factorial(N1,F1),
	F is N*F1.
%%%%%%

%fibonacci
%%%%%%
fibonacci(0,0):-!.
fibonacci(1,1):-!.
fibonacci(N,F):-
	N>1,
	N1 is N-1,
	N2 is N-2,
	fibonacci(N1,F1),
	fibonacci(N2,F2),
	F is F1+F2.
%%%%%%

%hanoi
%%%%%%
hanoi:-
	write('Numero de discos: '),
	read(N),
	mover(N,1,2,3).

%guion bajo: variable anonima

mover(1,Ini,Fin,_):-
	write(Ini),write('->'),write(Fin),nl, !.
mover(N,Ini,Fin,Aux):-
	N1 is N-1,
	mover(N1,Ini,Aux,Fin),
	mover(1,Ini,Fin,Aux),
	mover(N1,Aux,Fin,Ini).
%%%%%%

%gcd
%%%%%%
gcd(M,N,N):-
	R is M mod N,
	R =:= 0, !.
gcd(M,N,X):-
	R is M mod N,
	gcd(N,R,X).
%%%%%%

%div
%%%%%%
div(N,1,N):-!.
div(N,C,X):-
	X<N,
	R is N mod X,
	R=:=0,!,
	X1 is X+1,
	div(N,C1,X1),
	C is C1+1.
div(N,C,X):-
	X<N,
	X1 is X+1,
	div(N,C,X1).
%%%%%%

%si numero es primo
%%%%%%
primo:-
	write('Numero: '),
	read(N),
	div(N,C,1),
	C=:=2,
	write('Es primo.'),nl,!.
primo:-
	write('No es primo.'),nl,!.
%%%%%%

%%%%%%
sigPrimo(N,X):-
	primo(N),
	X is N,!.
sigPrimo(N,X):-
	N1 is N+1,
	sigPrimo(N1,X).
%%%%%%

%%%%%%
listaprimos(0,[]):-!.
listaprimos(X,Y):-
	longitud(Y,L),
	L=:=X,!.
listaprimos(X,Y):-
	longitud(Y,L),
	L=:=0,
	sigPrimo(1,P),
	Y1 is P,
	listaprimos(X,Y1),
	Y is Y1.
%%%%%%

%insercion
%%%%%%
insercion([],[]):- !.
insercion([X|Resto],Resultado):-
	insercion(Resto,Result),
	inserta(X,Result,Resultado).

inserta(X,[],[X]):- !.
inserta(X,[Y|Resto],[X,Y|Resto]):-
	X=<Y, !.
inserta(X,[Y|Resto],[Y|Resultado]):-
	inserta(X,Resto,Resultado).
%%%%%%

%permuta
%%%%%%
permuta([],[]):- !.
permuta([X|Resto],Resultado):-
	permuta(Resto,Result),
	insertaTodas(X,Result,Resultado).

insertaTodas(X,[],[X]):- !.
insertaTodas(X,[Y|Resto],[X,Y|Resto]).
insertaTodas(X,[Y|Resto],[Y|Resultado]):-
	insertaTodas(X,Resto,Resultado).
%%%%%%

% seleccion() elige al menor/mayor y lo coloca al principio/final y se
% descarta
%%%%%%
elementoMinimo([X], X) :- !.
elementoMinimo([H|T], Min) :-
	elementoMinimo(T, MinT),
	Min is min(H, MinT).

elementoMaximo([X],X):- !.
elementoMaximo([H|T],Max):-
	elementoMaximo(T,Maxt),
	Max is max(H,Maxt).

eliminarPrimero([H|T], H, T) :- !.
eliminarPrimero([H|T], X, [H|RestT]) :-
	eliminarPrimero(T, X, RestT).

seleccion([],[]):- !.
seleccion(Lista, Ordenado) :-
    elementoMinimo(Lista, Min),
    eliminarPrimero(Lista, Min, Resto),
    seleccion(Resto, RestoOrdenado),
    Ordenado = [Min|RestoOrdenado].
%%%%%%
/*seleccion([X|Resto],Resultado):- !.*/

%lte - less than or equal
%%%%%%
lte(_,[],[]):- !.
lte(X,[Y|Resto],[Y|Otros]):-
	Y=<X,!,
	lte(X,Resto,Otros).
lte(X,[_|Resto],Otros):-
	lte(X,Resto,Otros).
%%%%%%

%gt - greater than
%%%%%%
gt(_,[],[]):- !.
gt(X,[Y|Resto],[Y|Otros]):-
	Y>X,!,
	gt(X,Resto,Otros).
gt(X,[_|Resto],Otros):-
	gt(X,Resto,Otros).
%%%%%%

%concatena
%%%%%%
concatena([],X,X):- !.
concatena([X|Resto],Y,[X|Resto1]) :-
	concatena(Resto,Y,Resto1).
%%%%%%

%union
%%%%%%
union([A|B], C, D) :-
	member(A, C), !,
	union(B, C, D).
union([A|B], C, [A|D]) :-
	union(B, C, D).
union([], X, X).
%%%%%%

%interseccion
%%%%%%
interseccion([], _, []).
interseccion([H1|T1], L2, [H1|Res]) :-
    member(H1, L2),
    interseccion(T1, L2, Res).

interseccion([_|T1], L2, Res) :-
    interseccion(T1, L2, Res).
%%%%%%

%simplificar
%%%%%%
simplificar([],[]).
simplificar([X],[X]).
simplificar([X,X|Xs],Zs) :-
	simplificar([X|Xs],Zs).
simplificar([X,Y|Ys],[X|Zs]) :-
	simplificar([Y|Ys],Zs).
%%%%%%

%suma
%%%%%%
suma([],0):- !.
suma([X|Resto],C):-
	suma(Resto,C1),
	C is C1+X, !.
%%%%%%

%longitud
%%%%%%
longitud([],0):-!.
longitud([_|Resto],L):-
       longitud(Resto,L1),
       L is L1+1.
%%%%%%

%mezcla
%%%%%%
mezcla([], X, X) :- !.
mezcla(X, [], X) :- !.
mezcla([H1|T1], [H2|T2], [H1|M]) :-
	H1 =< H2,
	mezcla(T1, [H2|T2], M),
	!.
mezcla([H1|T1], [H2|T2], [H2|M]) :-
	H1  > H2,
	mezcla([H1|T1], T2, M).
%%%%%%

%dividir
%%%%%%
dividir([], [], []) :- !.
dividir([X], [X], []) :- !.
dividir([H1,H2|T], [H1|T1], [H2|T2]) :-
	dividir(T, T1, T2).
%%%%%%

%rango
%%%%%%
rango([],0).
rango(X,Y):-
	elementoMaximo(X,Max),
	elementoMinimo(X,Min),
	Y is Max-Min+1.
%%%%%%

%promedio
%%%%%%
promedio([],0).
promedio(X,Y):-
	suma(X,Sum),
	longitud(X,L),
	Y is Sum/L.
%%%%%%

%escalar
%%%%%%
escalar([],[],0).
escalar([X|Resto1],[Y|Resto2],E):-
	longitud([X|Resto1],L1),
	longitud([Y|Resto2],L2),
	L1 = L2,
	escalar(Resto1,Resto2,E1),
	E is X*Y +E1.
%%%%%%

%factorial
%%%%%%
factorial(0,1):-!.
factorial(N,F):-
	N>0,
	N1 is N-1,
	factorial(N1,F1),
	F is N*F1.
%%%%%%

%x a la n
%%%%%%
xalan(_,0,1):- !.
xalan(X,N,Y):-
	N1 is N-1,
	xalan(X,N1,Y1),
	Y is Y1*X.
%%%%%%

%e a la x
%%%%%%
ealaX(0,_,1).
ealaX(_,N,Y):-
	N<0,
	Y=0, !.
ealaX(X,N,Y):-
	N1 is N-1,
	ealaX(X,N1,Y1),
	xalan(X,N,Xn),
	factorial(N,F),
	Y is Y1 + Xn/F.

ealax:-
	nl,write('x: '),
	read(X),nl,
	write('Numero de terminos: '),
	read(N),nl,
	ealaX(X,N,Y),
	write(Y),nl,!.
%%%%%%

%integra
%%%%%%
f(X,Y):-
	T1 is -X*X/2,
	T2 is exp(T1),
	T3 is sqrt(2*3.1415926),
	Y is T2/T3.

integra:-
	nl,write('limite inferior: '),
	read(A),nl,
	write('limite superior: '),
	read(B),nl,
	write('numero de rectangulos: '),
	read(N),
	X is (B-A)/N,
	area(A,X,N,Y),
	write(Y).

area(_,_,0,0):- !.
area(A,N,R,X):-
	R1 is R -1,
	area(A,N,R1,X1),
	Pmedio is ((A+(N*(R-1)))+N/2),
	f(Pmedio,Y),
	ARec is Y*N,
	X is X1+ARec.
%%%%%%

%barras
%%%%%%
escribeA(0,_):-!.
escribeA(X,Escala):-
	X1 is X-1,
	escribeA(X1,Escala),
	escala(Escala).

escala(0):- !.
escala(Escala):-
	write('*'),
	Escala1 is Escala -1,
	escala(Escala1).

ancho(_,0,_):- !.
ancho(X,Ancho,Escala):-
	escribeA(X,Escala),nl,
	Ancho1 is Ancho -1,
	ancho(X,Ancho1,Escala).

barras([],_,_):-!.
barras([X|Resto],Ancho,Escala):-
	ancho(X,Ancho,Escala),nl,
	barras(Resto,Ancho,Escala).
%%%%%%

%columnas([4,1,3,2])
%%%%%%
%*
%* *
%* **
%****
columna([]):-!.
columna(X):-
	elementoMaximo(X,Max),
	columnas(X,Max),!.

columnas([],_):-!.
columnas(_,0):-!.
columnas(X,Max):-
	nl,
	Max1 is Max-1,
	compCol(X,Max),
	columnas(X,Max1).

compCol([],_):-!.
compCol([X|Resto],Max):-
	X >= Max,
	write('*'),
	compCol(Resto,Max).
compCol([_|Resto],Max):-
	write(' '),
	compCol(Resto,Max).
%%%%%%

%suma2
%%%%%%
suma2([],0):- !.
suma2([X|Resto],C):-
	suma2(Resto,C1),
	C is C1+(X*X), !.
%%%%%%

%pendiente
%%%%%%
pendiente(N,SX,SY,SXY,SX2,P):-
	P1 is (N*SXY)-(SX)*(SY),
	P2 is (N*SX2)-(SX*SX),
	P is P1/P2.
%%%%%%

%ordenada
%%%%%%
ordenada(N,SX2,SY,SXY,SX,O):-
	O1 is (SX2*SY)-(SXY*SX),
	O2 is (N*SX2) - (SX*SX),
	O is O1/O2.
%%%%%%

svv(N,SV2,SV,Svv):-
	Svv is SV2 - ((SV*SV)/N).

sxy(N,SX,SY,SXY,Sxy):-
	Sxy is SXY - ((SX*SY)/N).

r(Sxx,Syy,Sxy,R):-
	R is (Sxy/(sqrt(Sxx*Syy))).

regresionlineal:-
	write('lista X: '),
	read(X),nl,
	write('lista Y: '),
	read(Y),nl,
	longitud(X,L1),
	longitud(Y,L2),
	L1 = L2,
	suma(X,SX),
	suma(Y,SY),
	suma2(X,SX2),
	suma2(Y,SY2),
	escalar(X,Y,SXY),
	pendiente(L1,SX,SY,SXY,SX2,P),
	ordenada(L1,SX2,SY,SXY,SX,O),
	svv(L1,SX2,SX,Sxx),
	svv(L2,SY2,SY,Syy),
	sxy(L1,SX,SY,SXY,Sxy),
	r(Sxx,Syy,Sxy,R),
	write('Pendiente: '),write(P),nl,
	write('Ordenada: '),write(O),nl,
	write('R: '),write(R),!.
%%%%%%

%%%%%%
agregainicio(X,Resto,[X|Resto]).

agregafinal(X,[],[X]):- !.
agregafinal(X,[Y|Resto],[Y|Resto1]):-
	agregafinal(X,Resto,Resto1).

lista(0,[]):-!.
lista(X,Y):-
	X1 is X-1,
	lista(X1,Y1),
	agregafinal(X,Y1,Y).

lista0(0,[]):-!.
lista0(X,Y):-
	X1 is X-1,
	lista0(X1,Y1),
	agregafinal(X1,Y1,Y).
%%%%%%

%%%%%%
compara(X,Y):-
	X > Y+1,!.
compara(_,_):-
	nl,
	lanza.

lanza:-
	random(R1),
	Y1 is (R1*100),
	X1 is ((round(Y1))mod(6))+1,
	random(R2),
	Y2 is (R2*100),
	X2 is ((round(Y2))mod(6))+1,
	write(X1),write(','),write(X2),
	compara(X1,X2).
%%%%%%

%separa final
%%%%%%
separafinal([],[]):-!.
separafinal([X|Resto],Y):-
	longitud(Resto,L),
	L=:=0,
	Y is X,!.
separafinal([_|Resto],Y):-
	longitud(Resto,L),
	L>0,
	separafinal(Resto,Y1),
	Y is Y1.
%%%%%%

%%%%%%
natural(0):-!.
natural(s(X)):- natural(X).
%%%%%%

%%%%%%
representacion(0,0):-!.
representacion(N,s(R)):-
	N1 is N-1,
	representacion(N1,R).

suman(0,X,X):-!.
suman(s(X),Y,s(Z)):-
	suman(X,Y,Z).

producto(0,_,0):-!.
producto(_,0,0):-!.
producto(s(X),Y,W):-
	producto(X,Y,Z),
	suman(Y,Z,W).
%%%%%%

%numero a romano
%%%%%%
aRomano(0):-!.
aRomano(N):-N<4,write('I'),N1 is N-1,aRomano(N1),!.
aRomano(N):-N=4,write('IV'),N1 is N-4,aRomano(N1),!.
aRomano(N):-N<9,write('V'),N1 is N-5,aRomano(N1),!.
aRomano(N):-N=9,write('IX'),N1 is N-9,aRomano(N1),!.
aRomano(N):-N<40,write('X'),N1 is N-10,aRomano(N1),!.
aRomano(N):-N<50,write('XL'),N1 is N-40,aRomano(N1),!.
aRomano(N):-N<90,write('L'),N1 is N-50,aRomano(N1),!.
aRomano(N):-N<100,write('XC'),N1 is N-90,aRomano(N1),!.
aRomano(N):-N<400,write('C'),N1 is N-100,aRomano(N1),!.
aRomano(N):-N<500,write('CD'),N1 is N-400,aRomano(N1),!.
aRomano(N):-N<900,write('D'),N1 is N-500,aRomano(N1),!.
aRomano(N):-N<1000,write('CM'),N1 is N-900,aRomano(N1),!.
aRomano(N):-N<4000,write('M'),N1 is N-1000,aRomano(N1),!.
%%%%%%

%elemento n
%%%%%%
elementoN([],_,[]):-!.
elementoN([P|_],N,X):-
	N=0,
	X is P,!.
elementoN([_|Resto],N,X):-
	N1 is N-1,
	elementoN(Resto,N1,X).
%%%%%%
