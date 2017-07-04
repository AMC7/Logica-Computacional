--Antonio Martínez Cruz
--312229146
--correo : toritsman@gmail.com

miCuenta = 312229146

{-
   Lógica computacional 2017-1
      Noé Salomón Hernández Sánchez
      Albert M. Orozco Camacho
      C. Moisés Vázquez Reyes
      Diego Murillo Albarrán
      José Roberto Piche Limeta

   Práctica 1
-}

--INTERPRETACIONES

data Form =  Conj Form Form | Disy Form Form | Imp Form Form | DImp Form Form | Neg Form | Var String | T | F deriving (Show,Eq,Ord)
type Estado = [String]

interp::Estado->Form->Bool 
interp x T = True
interp x F = False
interp x (Var y)= elem y x  
interp x (Neg y)= not(interp x y)
interp x (DImp y z) = (interp x y)  == (interp x z) 
interp x (Imp y z) 
     |((interp x y) == True) && ((interp x z) == False)  = False
     | otherwise = True 
interp x (Disy y z)= (interp x y)||(interp x z)
interp x (Conj y z)= (interp x y)&&(interp x z)

interp1 = interp ["a","b"] $DImp (Var "a") (Var "b")
--False
interp2 = interp ["q","p"] $ Conj (Var "q") (Disy (Var "r") (Var "p"))
--False
interp3=interp ["p","q"] $ Conj (Var "q") (Disy (Var "r") (Var "p"))
--True
interp4 = interp ["a"] $ Imp (Var "a") (Var "b") 

vars::Form->[String]
vars x = quitaRep (vars' x)

quitaRep::(Eq a)=>[a]->[a]
quitaRep [] = []
quitaRep (x:xs) 
           |(elem x xs) = quitaRep xs
           |otherwise = x:quitaRep xs

vars'::Form->[String]
vars' T = []
vars' F = []
vars' (Var y)= [y]   
vars' (Neg y)= vars  y
vars' (DImp y z)= (vars  y)++(vars z) 
vars' (Imp y z) = (vars  y)++(vars z) 
vars' (Disy y z)= (vars  y)++(vars z) 
vars' (Conj y z)= (vars  y)++(vars z) 


vars1  = vars ( Conj (Var "q") (Disy (Var "r") (Var "p")) ) 
vars2  = vars $ Conj (Var "q") (Disy (Var "r") (Var "q"))
--["r","p","q"]

potencia::[a]->[[a]]
potencia [] = [[]]
potencia (x:xs)= map (x:) (potencia xs) ++ potencia xs

estados1=estados $ Conj (Var "p") T
--[[],["p"]]
estados2=estados $ Disy (Var "q") (Conj (Var "r") (Var "q"))
--[[],["q"]; ["r"]; ["q","r"]]

estados::Form->[Estado]
estados x = potencia $vars x 


tautologia::Form->Bool
tautologia x = tautologia' (estados x)  x 

tautologia'::[Estado]->Form->Bool 
tautologia' [] _ = True
tautologia' (x:xs) y = (interp x y) && (tautologia' xs y) 


tau1=tautologia $ Disy (Var "q") (Var "r")
--False
tau2=tautologia $ Disy (Var "p") (Neg $ Var "p")
--True

--TABLEUAX SEMÁNTICOS

data Tableaux = Void | R1 [Form] Tableaux | R2 Tableaux Tableaux | Tache | Bolita deriving Show
   

creaTableaux::[Form]->Tableaux
creaTableaux y = marcaHojas (creaTableaux' y)
									                               																	
tieneVarsCom::[Form]->Bool
tieneVarsCom [] = False
tieneVarsCom (x:xs) =if (elem (niega x)) xs then True else False 

niega::Form->Form
niega T = F
niega F = T
niega (Var p) = Neg (Var p)
niega (Neg x) = x
niega (Conj v1 v2) = (Disy (niega v1) (niega v2))
niega (Disy v1 v2) = (Conj (niega v1) (niega v2))
niega (Imp  v1 v2) = (Conj v1 (niega (v2)))
niega (DImp v1 v2) = niega (DImp (niega v1) v2)

a=[(Neg(Neg (Var "x")))] 
b=[(Conj (Disy (Var "p") (Var "b")) (Neg (Var "b")))]
c=[Conj (Var "p") (Neg(Var "p"))]
d=[(Disy (Var "x")(Var "y"))]
e=(Conj (Neg(Neg (Var "x"))) (Neg (Var "x")))

creaTableaux'::[Form]->Tableaux
creaTableaux' [] =Void
--Los casos base	
creaTableaux' (T:xs)            = R1 (T:xs)(creaTableaux' xs)
creaTableaux' (F:xs)            = R1 (F:xs)(Tache)
creaTableaux' ((Var y):xs)      = R1 ((Var y):xs) Void
creaTableaux' ((Neg(Neg x)):xs) = R1 ((Neg(Neg x)):xs)     (creaTableaux' (x:xs))
creaTableaux' (Neg(Var y):xs)   = R1 (Neg(Var y):xs) (creaTableaux' xs)
--Las alfa reglas
creaTableaux' ((Conj y z):xs)     = R1 ((Conj y z):xs)       (creaTableaux' (y:z:xs))
creaTableaux' ((Neg(Disy y z)):xs)= R1 ((Neg(Disy y z)):xs)  (creaTableaux' ((Neg(y)):(Neg(z)):xs))
creaTableaux' (Neg(Imp y z):xs)   = R1 (((Neg(Imp y z))):xs) (creaTableaux' (y:Neg(z):xs))
creaTableaux' ((DImp y z):xs)     = R1 (((DImp y z):xs))     (creaTableaux' ((Imp y z):(Imp z y):xs))
--las beta reglas
creaTableaux' (Neg(Conj y z):xs)  = R2((creaTableaux'  (Neg(y):xs)))        ((creaTableaux' (Neg(z):xs)))
creaTableaux' ((Disy y z):xs)     = R2((creaTableaux'  (y:xs)))             ((creaTableaux' (z:xs)))
creaTableaux' ((Imp y z):xs)      = R2((creaTableaux'  (Neg(y):xs)))        ((creaTableaux' (z:xs)))
creaTableaux' (Neg(DImp y z):xs)  = R2((creaTableaux'  (Neg (Imp y z):xs))) ((creaTableaux'(Neg (Imp z y):xs)))

marcaHojas::Tableaux-> Tableaux
marcaHojas Void = Void
marcaHojas Tache = Tache
marcaHojas Bolita = Bolita
marcaHojas (R1 (x:xs) Void ) = if (elem (niega(x))(xs))  then  R1 (x:xs) Tache else R1 (x:xs) Bolita
marcaHojas (R1 a b) = (R1 a (marcaHojas b))
marcaHojas (R2 a b )= (R2 (marcaHojas a) (marcaHojas b))
	
cerrado::Tableaux->Bool	
cerrado x = cerrado' x
                                       
cerrado'::Tableaux->Bool
cerrado' Void = False
cerrado' Tache = True
cerrado' Bolita = False
cerrado' (R1 (x:xs) Void ) = if (elem (niega(x))(xs))  then  True else  False
cerrado' (R1 a b) = cerrado' b
cerrado' (R2 a b )= (cerrado' a)&&(cerrado' b)
							  
                                                            
--data Tableaux = Void | R1 [Form] Tableaux | R2 Tableaux Tableaux | Tache | Bolita deriving Show

tautologia_tableaux::Form->Bool
tautologia_tableaux x = (cerrado (creaTableaux [x] ))

--Algunas fórmulas de prueba. 
--Puedes comprobar que f <--> f1 <--> f2
--Puedes comprobar que g <--> g1 <--> g2


f = Disy (Neg $ Imp (Var "w") (Var "e")) (Neg $ Disy (DImp (Neg $ Var "s") (Var "w")) (Conj (Var "e") (Var "s")))
f1 = Conj (Conj (Conj (Conj (Disy (Disy (Neg $ Var "e") (Var "s")) (Neg $ Var "w")) (Disy (Neg $ Var "s") (Var "w"))) (Disy (Disy (Neg $ Var "e") (Neg $ Var "s")) (Var "w"))) (Disy (Disy (Var "w") (Neg $ Var "e")) (Neg $ Var "s"))) (Disy (Neg $ Var "e") (Neg $ Var "s"))                                                             
f2 = Disy (Disy (Disy (Conj (Var "w") (Neg $ Var "e")) (Conj (Conj (Neg $ Var "w") (Neg $ Var "s")) (Neg $ Var "e"))) (Conj (Neg $ Var "w") (Neg $ Var "s"))) (Conj (Conj (Var "s") (Var "w")) (Neg $ Var "e"))

g = Disy (Neg $ Imp (Var "w") (Var "e")) (Neg $ Disy (DImp (Neg $ Var "s") (Var "w")) (Conj (Var "e") (Var "s")))
g1 = Conj (Conj (Conj (Conj (Disy (Var "w") (Neg $ Var "s")) (Disy (Disy (Neg $ Var "e") (Neg $ Var "s")) (Var "w"))) (Disy (Disy (Neg $ Var "e") (Neg $ Var "w")) (Var "s"))) (Disy (Disy (Var "w") (Neg $ Var "e")) (Neg $ Var "s"))) (Disy (Neg $ Var "e") (Neg $ Var "s"))
g2 = Disy (Disy (Disy (Conj (Var "w") (Neg $ Var "e")) (Conj (Conj (Neg $ Var "s") (Neg $ Var "w")) (Neg $ Var "e"))) (Conj (Neg $ Var "s") (Neg $ Var "w"))) (Conj (Conj (Var "w") (Var "s")) (Neg $ Var "e"))





