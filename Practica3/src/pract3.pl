:- dynamic on/2.
/* 
Antonio Martínez Cruz
312229146
antoniomartinezcruz@ciencias.unam.mx
 Facultad de Ciencias UNAM - Lógica Computacional 2017-1
      Noé Salomón Hernández Sánchez
      Albert M. Orozco Camacho
      C. Moisés Vázquez Reyes
      Diego Murillo Albarrán
      José Roberto Piche Limeta  */

/*
encabeza X sobre la pila que encabeza W , por ejemplo, si tenemos las pilas abc y dg cuyos topes
son a y d, el resultado debe ser abcdg.
move reversed(X; W ) : si X y W estan en el tope de dos pilas de bloques, mueve toda la pila que
encabeza X sobre la pila que encabeza W en reversa, por ejemplo, si tenemos las pilas abc y dg
cuyos topes son a y d, el resultado debe ser cbadg.
*/

on(c,piso).
on(d,piso).
on(f,piso).
on(i,piso).
on(b,c).
on(e,f).
on(h,i).
on(a,b).
on(g,h).

/*Implemente las siguientes relaciones referentes al mundo de bloques.
blocked(X) : nos dira true si hay un bloque encima de X, false en otro caso.
*/
blocked(X):-on(_,X).

/*onTop(X) : nos dira true si no hay bloques encima de X, false en otro caso.*/
onTop(B):-not(blocked(B)).
/*movePiso(X) : mueve el bloque X al piso si el bloque X esta en el tope.
*/
movePiso(B):- onTop(B),retract(on(B,_)), assertz(on(B,piso)).
/*move(X; Y ) : mueve el bloque X encima del bloque Y si ambos estan en el tope.
*/
move(X,Y):-onTop(X),onTop(Y),assertz(on(X,Y)),retract(on(X,_)).

bottomaux(X,X):-on(X,piso).
bottomaux(X,Y):-on(X,Y).
bottomaux(X,Y):-on(X,Z),bottomaux(Z,Y).

/*bottom(X,Y) : si una pila de bloques tiene en el tope al bloque X y en el
fondo al bloque Y , debeocurrir que bottom(X; Z) regrese Z = Y .*/
bottom(X,Y):-onTop(X),on(Y,piso),bottomaux(X,Y).

/*Move ordered(X; W ) : si X y W estan en el tope de dos pilas de bloques, mueve toda la pila que encabeza X sobre la pila que encabeza W ,*/
move_ordered(X,W):-onTop(X),onTop(W),bottom(X,Y),assertz(on(Y,W)),retract(on(Y,_)).

/*si X y W estan en el tope de dos pilas de bloques, mueve toda la pila que
encabeza X sobre la pila que encabeza W en reversa, por ejemplo, si tenemos las pilas abc y dg
cuyos topes son a y d, el resultado debe ser cbadg.*/

move_reversed(X,Y):-onTop(X),onTop(Y),on(X,piso),move(X,Y).
move_reversed(X,Y):-onTop(X),onTop(Y),on(X,Z),move(X,Y),move_reversed(Z,X).

/* Computologos: la competecia*/

computologos([lourdes,susana,francisco,jose]).

competencia:- solucion(S,B),write('Reto de programacion = '),write(S),nl,
	write('Reto de demostracion = '),write(B),nl.

solucion(Prog,Dem):- computologos(C),permutacion(Prog,C),permutacion(Dem,C),
	          primer(Dem,PD),primer(Prog,PP),
		  segundo(Dem,_),segundo(Prog,SP),
		  tercero(Dem,TD),tercero(Prog,TP),
		  ultimo(Dem,UD),ultimo(Prog,UP),
		  PD\==jose,PP\==jose,
		  PP=TD,
		  SP=jose,TP=francisco,
		  PP=susana,
		  UD\==francisco,UP\==francisco,
	          PD==lourdes.

ultimo([X],X).
ultimo([_|XS],Y):- ultimo(XS,Y).

permutacion([],[]).
permutacion([X|XS],P):-member(X,P),elimina(X,P,R),permutacion(XS,R).

elimina(X,[X|XS],XS).
elimina(X,[Y|YS],[Y|ZS]):- elimina(X,YS,ZS).

primer([X,_,_,_],X).
segundo([_,X,_,_],X).
tercero([_,_,X,_],X).












