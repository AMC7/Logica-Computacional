%Antonio Martinez Cruz
%Numero de Cuenta 312229146
%antoniomartinezcruz@ciencias.unam.mx
% Relacion que define los naturales
nat(cero).
nat(suc(N)):-nat(N).

% 1.Predicado que es cierto cuando K = N+M
suma(cero,K,K).
suma(suc(N),M,suc(K)):-suma(N,M,K).

% 2.Predicado que es cierto cuando K = N*M
prod(suc(cero),K,K).
prod(suc(N),M,K):-prod(N,M,Z),suma(Z,M,K).

%3.Predicado que es cierto si M es el factorial de N
factorial(cero,suc(cero)).
factorial(suc(cero),suc(cero)).
factorial(suc(N),M):-factorial(N,Z),prod(suc(N),Z,M).

% 4.Predicado que es cierto si X^N = R
potencia(_,cero,suc(cero)).
potencia(X,suc(N),R):-potencia(X,N,Z),prod(X,Z,R).

% 5.Predicado que es cierto si N<M
menor(cero,suc(_)).
menor(suc(X),suc(Y)):- menor(X,Y).

%6.Predicado que es cierto si N=M
igual(cero,cero).
igual(suc(N),suc(M)):-igual(M,N).

%7.Predicado que es cierto si X es elemento de la lista L
elem(X,[X|_]).
elem(X,[_|Z]) :-elem(X,Z).

% Predicado que te decuelve la concatenacion de dos listas
concatenar([],L,L).
concatenar([A|B],L,[A|Z]):-concatenar(B,L,Z).

%8.Predicado que es cierto si la lista R es la reversa de la lista L
reversa([],[]).
reversa([A|B],X):-concatenar(Y,[A],X) , (reversa(B,Y)).

%9.Predicado que es ciertosi la lista es Palindroma
palindroma(X):- iguales(X,Y) , reversa(X,Y).

% Relacion que devuelve la cabeza de una constante lista
cabeza([],null).
cabeza([A|_],A).

% 10.Predicado que es cierto si x es el ultimo elemento de la
% lista l

ultimo(L,X):- cabeza(Y,X) , reversa(L,Y).

%Predicado que te dice si dos listas son iguales
iguales([],[]).
iguales([A|B],[A|C]):- iguales(B,C).

%11.Predicado que es cierto si N es la longitud de la lista L.
long([],0).
long([_|L],N) :- long(L,X), N is X+1.


% Predicado que es cierto si la lista L2 es la lista que resulta de
% eliminar todas las apariciones de X
%en la lista L1.

elimina(_,[],[]).
elimina(a,[a],[]).
elimina(a,[B],[B]).
elimina(a,[a|B],Y):-elimina(a,B,Y).
elimina(a,[A|B],Y):-concatenar([A],W,Y),elimina(a,B,W).











