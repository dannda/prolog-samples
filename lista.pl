/*******************************
lista.pl predicados para listas
*******************************/

escribe([]):- !.
escribe([Primero|Resto]):-
	write(Primero),nl,escribe(Resto).

longitud([],0):-!.
longitud([_|Resto],L):-
	longitud(Resto,L1),
	L is L1+1.

pertenece(X,[X|_]):- !.
pertenece(X,[_|Resto]):-
	pertenece(X,Resto).

cuentarep(_,[],0):-!.
cuentarep(X,[X|Resto],C):-
	cuentarep(X,Resto,C1),
	C is C1+1, !.
cuentarep(X,[_|Resto],C):-
	cuentarep(X,Resto,C).

/*Solo se pueden separar elementos del inicio [primero|etc]*/
agregainicio(X,Resto,[X|Resto]).

agregafinal(X,[],[X]):- !.
agregafinal(X,[Y|Resto],[Y|Resto1]):-
	agregafinal(X,Resto,Resto1).

concatena([],X,X):- !.
concatena([X|Resto],Y,[X|Resto1]) :-
	concatena(Resto,Y,Resto1).

iguales([],[]):- !.
iguales([X|Resto1],[X|Resto2]):-
	iguales(Resto1,Resto2).
