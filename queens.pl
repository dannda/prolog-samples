queens:-
	write('Numero de reinas: '),read(N),
	columnas(N),sol(N).
%columnas disponibles a utilizar

sol(N):-
	solucion(N,[],Solucion),
	imprimesolucion(Solucion),nl.

sol(N):- borra(N).

columnas(0):-!.
columnas(N):-
	assert(col(N)),N1 is N-1, columnas(N1).

borra(0):-!.
borra(N):-
	retract(col(N)),N1 is N-1, borra(N1).

noataca2(_,[],_):-!.
noataca2(Col,[X|Resto],Y):-
	not(Col=Y),
	Col1 is Col-Y,
	not(Col1=X),
	Col2 is Col+Y,
	not(Col2=X),
	Y1 is Y+1,
	noataca2(Col,Resto,Y1).

noataca(Col,Lista):-
	noataca2(Col,Lista,1).

imprimesolucion([]):-!.
imprimesolucion([Columna|Resto]):-
	imprimesolucion(Resto),
	write(Columna),write(' ').

solucion(N,Lista,Lista):-
	longitud(Lista,N1), N=:=N1,!.
solucion(N,ListaIn,ListaOut):-
	col(X),
	noataca(X,ListaIn),
	solucion(N,[X|ListaIn],ListaOut).

longitud([],0):-!.
longitud([_|Resto],N):-
	longitud(Resto,N1),
	N is N1+1.
