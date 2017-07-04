%Antonio Martinez Cruz
%antoniomartinezcruz@ciencias.unam.mx
%312229146
%Yo Antonio Jr soy el protagonista

conyugue(antoniojr,linda).
conyugue(antonio,lindajr).
madre(linda,lindajr).
madre(linda,beto).
madre(lindajr,antoniojr).
madre(lindajr,paty).
padre(antonio,antoniojr).
padre(antonio,paty).
padre(antoniojr,beto).
padre(antoniojr,lindajr).
hermano(antoniojr,paty).
hermano(beto,lindajr).



suegro(X,Y):-conyugue(Z,Y),padre(X,Z).
suegro(X,Y):-conyugue(Y,Z),padre(X,Z).
suegro(X,Y):-conyugue(Z,Y),madre(X,Z).
suegro(X,Y):-conyugue(Y,Z),madre(X,Z).

abuelo(X,Y):-padre(X,Z),padre(Z,Y).
abuelo(X,Y):-padre(X,Z),madre(Z,Y).

yerno(X,Y):-conyugue(X,Z),padre(Y,Z).
yerno(X,Y):-conyugue(Z,X),padre(Y,Z).
yerno(X,Y):-conyugue(X,Z),madre(Y,Z).
yerno(X,Y):-conyugue(Z,X),madre(Y,Z).


tio(X,Y):-hermano(X,Z),padre(Z,Y).
tio(X,Y):-hermano(X,Z),madre(Z,Y).
tio(X,Y):-hermano(Z,X),padre(Z,Y).
tio(X,Y):-hermano(Z,X),madre(Z,Y).

cuñado(X,Y):-hermano(X,Z),conyugue(Z,Y).
cuñado(X,Y):-hermano(X,Z),conyugue(Y,Z).
cuñado(X,Y):-hermano(Z,X),conyugue(Z,Y).
cuñado(X,Y):-hermano(Z,X),conyugue(Y,Z).


%Problema dos
%
nombres([donald,jordi,michael,stephen,frank]).
apellidos([miller,rosado,knuth,spivak,king]).
precios([2,3,4,5,6]).

solucion([[Nom1,Apell1,Precio1],
	  [Nom2,Apell2,Precio2],
	  [Nom3,Apell3,Precio3],
	  [Nom4,Apell4,Precio4],
	  [Nom5,Apell5,Precio5]
	 ]):-
	nombres(Nom),permutacion([Nom1,Nom2,Nom3,Nom4,Nom5],Nom),
	apellidos(Apell),permutacion([Apell1,Apell2,Apell3,Apell4,Apell5],Apell),
	precios(Prec),permutacion([Precio1,Precio2,Precio3,Precio4,Precio5],Prec),
	Nom1=stephen,Apell1=king,
	Nom2=donald,Apell3=spivak,Precio2 is Precio3 +1,Nom4 = frank,	Precio3 is	Precio4 +1,
	Nom5 = jordi,Precio2 is Precio5+3,
	Precio3 is Precio5*2,Apell5=rosado,Apell2=knuth.

permutacion([],[]).
permutacion([X|XS],P):-member(X,P),elimina(X,P,R),permutacion(XS,R).

elimina(X,[X|XS],XS).
elimina(X,[Y|YS],[Y|ZS]):- elimina(X,YS,ZS).

