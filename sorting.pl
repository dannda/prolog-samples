%%%% Quick sort %%%%
%quick sort elige un número como pivote y se ordena a su izq y der
qsort([],[]):- !.
qsort([X],[X]):- !.
qsort([X|Resto],Resultado):-
	lte(X,Resto,Parte1),
	gt(X,Resto,Parte2),
	qsort(Parte1,Resultado1),
	qsort(Parte2,Resultado2),
	concatena(Resultado1,[X|Resultado2],Resultado).

%%

lte(_,[],[]):- !.
lte(X,[Y|Resto],[Y|Otros]):-
	Y=<X,!,
	lte(X,Resto,Otros).
lte(X,[_|Resto],Otros):-
	lte(X,Resto,Otros).

gt(_,[],[]):- !.
gt(X,[Y|Resto],[Y|Otros]):-
	Y>X,!,
	gt(X,Resto,Otros).
gt(X,[_|Resto],Otros):-
	gt(X,Resto,Otros).

concatena([],X,X):- !.
concatena([X|Resto],Y,[X|Resto1]) :-
	concatena(Resto,Y,Resto1).

%%%%%%%%%%%%

%%%% Merge sort %%%%

% Predicado mergesort que tome primeros n/2 elementos de lista en
% Primeros tomar resto de elementos en Ultimos
% mergesort Primeros
% mergesort Ultimos
% mezcla
mergeSort([], []) :- !.
mergeSort([A], [A]) :- !.
mergeSort(In, Out) :-
    dividir(In, L1, L2),
    mergeSort(L1, S1),
    mergeSort(L2, S2),
    mezcla(S1, S2, Out).

%%

dividir([], [], []) :- !.
dividir([X], [X], []) :- !.
dividir([H1,H2|T], [H1|T1], [H2|T2]) :-
	dividir(T, T1, T2).
	
% Predicado mezcla recibe 2 listas ordenadas y regresa la mezcla de las
% listas ordenada
mezcla([], X, X) :- !.
mezcla(X, [], X) :- !.
mezcla([H1|T1], [H2|T2], [H1|M]) :-
	H1 =< H2,
	mezcla(T1, [H2|T2], M),
	!.
mezcla([H1|T1], [H2|T2], [H2|M]) :-
	H1  > H2,
	mezcla([H1|T1], T2, M).

%%%%%%%%%%%%