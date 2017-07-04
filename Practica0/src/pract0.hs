import Data.List(words)
import Data.Char(digitToInt)
{-
Lógica computacional 2017-1
         Noé Salomón Hernández Sánchez
         Albert M. Orozco Camacho
         C. Moisés Vázquez Reyes
         Diego Murillo
-}

data Nat = Cero | S Nat deriving Show

suma::Nat->Nat->Nat
suma Cero n= n
suma (S m) n = S (suma m n)

prod::Nat->Nat->Nat
prod Cero n = Cero
prod (S n) m =suma m (prod n m) 

mayorQue::Nat->Nat->Bool
mayorQue Cero n = False
mayorQue (S n) Cero = True  
mayorQue (S n) (S m) =mayorQue n m 

igual::Nat->Nat->Bool
igual Cero Cero = True
igual Cero (S _) = False
igual (S _) Cero = False
igual (S n) (S m) = igual n m 

power::Int->Int->Int
power x 0 = 1
power x y = x * power x (y-1) 

power2::Int->Int->Int
power2 x y =  if (even y) then power (power x 2) (div y 2) else x * power x (y-1)

reversa::[a]->[a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

sumal::[Int]->Int
sumal [] = 0
sumal (x:xs) = x + sumal xs

toma::Int->[a]->[a]
toma n xs = if (n<0) then error "Diste un numero negativo"
                      else tomaAux n xs   
 
tomaAux:: Int ->[a]->[a]
tomaAux 0 xs = []
tomaAux n [] = []
tomaAux n (x:xs) = x:tomaAux (n-1) xs

                     
tira::Int->[a]->[a]
tira n xs = if (n<0) then error "Diste un numero negativo"
                      else tiraAux n xs
         

tiraAux::Int ->[a]->[a]
tiraAux 0 xs =xs
tiraAux n [] =[]
tiraAux n (x:xs) =tiraAux (n-1) xs
                                            
cuantas::Eq a=>a->[a]->Int
cuantas e [] = 0
cuantas e (x:xs) = if (x==e) then 1+ (cuantas e xs) else cuantas e xs

frec::Eq a=>[a]->[(a, Int)]
frec xs = quitaRep([(x,y)|x<-xs,let y =cuantas x xs])

quitaRep ::Eq a => [a] ->[a]
quitaRep [] =[]
quitaRep (x:xs) =if(elem x xs) then quitaRep xs else x :quitaRep xs


unaVez::Eq a=>[a]-> [a]
unaVez x =getFst (unaVez' x)

getFst::[(a,Int)]->[a]
getFst [] =[]
getFst (x:xs) = fst x: getFst xs  

unaVez'::Eq a=>[a]->[(a,Int)]
unaVez' [] = []
unaVez' xs =  [s| s <-frec xs,snd s == 1] 


compress1::String->String
compress1 [] = []
compress1 (x:xs) = compress1Aux (words(x:xs)) 

compress1Aux:: [String]->String
compress1Aux [] = []
compress1Aux (x:xs) = x !! 0 : compress1Aux xs

esNumero:: Char -> Bool
esNumero n = if (n=='0' ||n=='1'||n=='2'||n=='3'||n=='4'||n=='5'||n=='6'||n=='7'||n=='8'||n=='9')then True
                                                                             else False
obtendigitos::String -> String
obtendigitos [] =[]
obtendigitos (x:xs) =if esNumero(x) then x:obtendigitos xs else []

compress2Aux :: [String] ->String
compress2Aux [] = []
compress2Aux (x:xs)=let i = (read(obtendigitos (x))) in if(i<length x-1) 
                                                        then x !! (i+1)  : compress2Aux xs
                                                        else " "++compress2Aux xs

compress2::String->String
compress2 [] = []
compress2 xs = compress2Aux (words(xs)) 


juego::(Int,Int)->(Int,Int)
juego (a,b) = let listaDeSumandos = juego' (a,b) in
                 (length listaDeSumandos,(mod (prod'(listaDeSumandos)) ((power 10 9)+7)))
 
juego'::(Int,Int)->[(Int,Int)]
juego' x =  let y = getParejaConMinProd(seleccionaParejas (fst x) (0,fst x)) in
                if (fst y == snd y)
                    then juego' (((fst x)+1),snd x)
                       else if (fst x == snd x) 
                       then  [y] else  y: juego' (((fst x)+1),snd x)

--Recibe una [(Int ,Int)] "l" y te regresa (a,b) talque (a,b) estan en "l" y si
--(c,d) estan en "l" -> c*d<a*b 

getParejaConMinProd :: [(Int,Int)]-> (Int,Int)
getParejaConMinProd [] = (0,0)
getParejaConMinProd (x:[]) = x
getParejaConMinProd (x:(y:ys)) = if (fst x * snd x) < (fst y * snd y) 
                                    then getParejaConMinProd (x:ys)
                                    else getParejaConMinProd (y:ys)

--Dado un intervalo (a,b) y un entero n , te devuelve la lista de parejas(c,d) tal que 
--a<= c<=b y a<=d<= b y (c + d = n) y c,d son primos
seleccionaParejas ::Int -> (Int,Int)->[(Int,Int)]
seleccionaParejas n (x,y) = getNumeros n (getListaDePrimos (x,y))

--Funcion que devuelve la lista de parejas  "l" tal que  sea (x,y) una pareja en "l" x+y =n
getNumeros:: Int-> [Int] -> [(Int,Int)]
getNumeros n [] = []
getNumeros n (x:xs)= if (elem (n-x)(x:xs)) then (x,(n-x)):(getNumeros n xs) else (getNumeros n xs )

getListaDePrimos::(Int,Int) ->[Int]
getListaDePrimos (x,y) =if (x==y) then []
                                     else  
                                       if (esPrimo x) 
                                          then x:getListaDePrimos ((x+1),y) 
                                          else getListaDePrimos ((x+1),y)


esPrimo::Int ->Bool
esPrimo x = if (x<2) then False else esPrimo' x x

esPrimo'::Int->Int-> Bool
esPrimo' x 1 = True
esPrimo' x y = if (mod x y == 0)&&(x/=y) then False else esPrimo' x (y-1)  


prod' ::[(Int,Int)]->Int
prod' []=1
prod' (x:xs)=(fst x)*(snd x)*prod' xs

