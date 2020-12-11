%Misioneros y caníbales
%miscan([e(3,3,1)],X).
%MMMCCC |
%MMCC	| MC
%mov(2,0).
%mov(0,2).
%mov(1,1).
%mov(1,0).
%mov(0,1).
%
/*member(H,[H|_]).
member(H,[_|T]) :-
	member(H,T).*/

sol(Estado,Path):-
	busca(Estado,[Estado],Path).

busca(EstadoActual,Path,Path):-
	esSolucion(EstadoActual),
	imprimeSol(Path),!.
busca(EstadoActual,EPath,Path):-
	intenta(EstadoActual,NuevoEstado),
	not(member(NuevoEstado,EPath)),
	valida(NuevoEstado),
	busca(NuevoEstado,[NuevoEstado|EPath],Path).

esSolucion(e(0,0,0)).

imprimeM(0,N,N):-!.
imprimeM(M,N,N2):-
	write('M'),
	M1 is M-1,
	N1 is N+1,
	imprimeM(M1,N1,N2).
imprimeC(0,N,N):-!.
imprimeC(C,N,N2):-
	write('C'),
	C1 is C-1,
	N1 is N+1,
	imprimeC(C1,N1,N2).

imprimeN(0):-!.
imprimeN(N):-
	write(' '),
	N1 is N-1,
	imprimeN(N1).

imprimeB(1):-
	write('|-__-      |'),!.
imprimeB(0):-
	write('|      -__-|'),!.

imprimeSol([]):-!.
imprimeSol([e(M,C,B)|Lista]):-
	imprimeSol(Lista),
	N is 0,
	M1 is 3-M,
	C1 is 3-C,
	imprimeM(M,N,N1),imprimeC(C,N1,N2),
	N3 is 6-N2, imprimeN(N3),imprimeB(B),
	imprimeM(M1,N,_),imprimeC(C1,N,_),nl.

%bote de lado inicial a final
intenta(e(M,C,1),e(NM,NC,0)):-
	movOF(M,C,2,NM,NC); %cruzan 2 personas
	movOF(M,C,1,NM,NC). %cruza 1 persona
intenta(e(M,C,0),e(NM,NC,1)):-
	movFO(M,C,2,NM,NC); %cruzan 2
	movFO(M,C,1,NM,NC). %cruza 1

movOF(M,C,0,M,C):-!.
movOF(M,C,N,NM,NC):-
	M1 is M-1, %cruza misionero
	M1>=0,
	N1 is N-1,
	movOF(M1,C,N1,NM,NC).
movOF(M,C,N,NM,NC):-
	C1 is C-1,
	C1>=0,
	N1 is N-1,
	movOF(M,C1,N1,NM,NC).

movFO(M,C,0,M,C):-!.
movFO(M,C,N,NM,NC):-
	M1 is M+1, %cruza misionero
	M1=<3,
	N1 is N-1,
	movFO(M1,C,N1,NM,NC).
movFO(M,C,N,NM,NC):-
	C1 is C+1,
	C1=<3,
	N1 is N-1,
	movFO(M,C1,N1,NM,NC).

valida(e(M,C,_)):-
	OM is 3-M,
	OC is 3-C,
	noCanibalismo(M,C),
	noCanibalismo(OM,OC).

noCanibalismo(0,_).
noCanibalismo(M,C):-
	C=<M. %menos canibales que misioneros


miscan([e(M,C,B)],X):-
	sol(e(M,C,B),X).