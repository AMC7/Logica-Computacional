(*312229146 Antonio Martinez Cruz antoniomartinezcruz@ciencias.unam.mx*)
(*Facultad de Ciencias UNAM - Lógica Computacional 2017-2
      Noé Salomón Hernández Sánchez
      Albert M. Orozco Camacho
      C. Moisés Vázquez Reyes
      Diego Murillo Albarrán
      José Roberto Piche Limeta*)


Section LogicaProposicional.

Variables p q r s t u w:Prop.

Lemma ejem1 : p -> p.
Proof.
intros.
apply H.
(*trivial.*)
Qed.


Lemma ejem2 : ((p -> q) /\ p) -> q.
Proof.
intros.
destruct H.
apply H.
apply H0.
Qed.



Lemma ejem3 : p -> q -> p/\q.
Proof.
intros.
split.
apply H.
apply H0.
Qed.


Lemma ejem4 : (p->q) /\ (q->r) -> (p -> r).
Proof.
intros.
destruct H.
apply H1.
apply H.
trivial.
Qed.

Lemma ejem5 : (p -> r) -> (q -> r) -> (p \/ q -> r).
Proof.
intros.
destruct H1.
apply H.
trivial.
apply H0.
trivial.
Qed.

Lemma Ejem6 : p \/ q -> q \/ p.
Proof.
intros.
destruct H.
right.
trivial.
left .
trivial.
Qed.





Theorem Arg1: (p -> q /\ r) -> 
              (r \/ ~q -> s /\ t) ->
              ( t <-> u) -> (p -> u).
Proof.
intros.
apply H1.
assert (s /\ t).
apply H0.
assert (q /\ r).
apply H.
trivial.
destruct H3.
left.
trivial.
destruct H3.
trivial.
Qed.



Lemma Contrapositiva: (~t -> ~s)->(s->t).
Admitted.



Theorem Arg3: (p <-> ~q /\ s) ->
              (p /\ (~t -> ~s)) -> ~q /\ t.
Proof.
intros.
destruct H0.
apply Contrapositiva in H1. 
split.
apply H in H0.
apply H0.
trivial.
apply H in H0.
apply H0.
Qed.


(*Ejercicios*)


Lemma ejer1 :( p /\ q -> q /\ p).
intros.
destruct H.
split.
trivial.
trivial.
Qed.

Lemma ejer2 : p /\ (q /\ r) -> (p /\ q) /\ r.
intros.
destruct H.
destruct H0.
split.
split.
trivial.
trivial.
trivial.
Qed.

Lemma ejer3 : (p -> q) /\ (p -> r) -> (p -> q /\ r).
intros.
destruct H.
split.
apply H.
trivial.
apply H1.
trivial.
Qed.

Lemma ejer4 : (p \/ q) -> (p -> r) -> (q -> r) -> r.
intros.
destruct H.
apply H0.
trivial.
apply H1.
trivial.
Qed.


Lemma ejer5 : (p -> q -> r) -> (p /\q -> r).
intros.
destruct H0.
apply H.
trivial.
trivial.
Qed.

Lemma ejer6 : (p \/ q) -> ( (p \/ (q -> r)) -> (p \/ r)).
intros.
destruct H0.
left.
trivial.
destruct H.
left.
trivial.
right.
apply H0.
trivial.
Qed.

Theorem ejer7: (p /\ q) -> 
              (r /\ ~s) ->
              (q -> p -> t) ->
              (t -> r -> s \/ w) -> w.  
intros.
destruct H.
destruct H0.
assert (p -> t).
apply H1.
trivial.
assert (r->s \/ w).
apply H2.
apply H5.
trivial.
assert (s \/ w).
apply H6.
trivial.
destruct H7.
assert(s\/w).
left.
trivial.
contradict H4.
trivial.
trivial.
Qed.


End LogicaProposicional.


Section LogicaDePredicados.
Variables (A:Type) (a b: A) (P Q R : A -> Prop).

Lemma ejemp1 : (forall x, P x) -> P a.
Proof.
intros.
apply H.
Qed.



Lemma ejemp2 : (forall x, P x -> Q x) /\ P b ->  P b /\ Q b.
Proof.
intros.
destruct H.
split.
trivial.
apply H.
trivial.
Qed.

Lemma ejemp3 : P a -> (exists x, P x).
Proof.
intros.
exists a.
trivial.
Qed.


Lemma ejemp4 : (P a /\ R a) -> (forall x, R x -> Q x) -> (exists x, P x /\ Q x).

Proof.
intros.
exists a.
split.
apply H.
apply H0.
apply H.
Qed.



Lemma ejemp5 : (forall x, P x -> Q x) /\ (exists x, R x /\ P x) -> (exists x, Q x /\ R x).
Proof.
intros.
destruct H.
destruct H0 as [c E].
exists c.
split.
apply H.
apply E.
apply E.
Qed.




(*Ejercicios*)
Lemma ejerc1 : (forall x, P x -> Q x) /\ (forall x, Q x -> R x) -> P a -> Q a.
intros.
destruct H.
apply H.
trivial.
Qed.


Lemma ejerc2: (forall x, P x -> Q x) -> (forall y, P y -> (Q y \/ R y)).
intros.
left.
apply H.
trivial.
Qed.

Lemma ejerc3 : (forall x, P x -> Q x -> R x) -> (P a /\ Q a) -> (exists y, R y).
intros.
destruct H0.
assert (Q(a)-> R(a)).
apply H.
trivial.
assert (R a).
apply H2.
trivial.
exists a.
trivial.
Qed.

Lemma ejerc4 : (exists x, P x /\ Q x) -> (exists x, P x /\ exists y, Q y).
intros.
destruct H.
destruct H.
exists x.
split.
trivial.
exists x.
trivial.
Qed.


Lemma ejerc5 : (forall x, P x -> R x) -> (forall x, P x /\ Q x -> R x).
intros.
destruct H0.
apply H.
trivial.
Qed.

Lemma ejerc6 : (forall x, P x \/ Q x) -> (forall x, P x)\/(forall x, Q x).
Admitted.


Lemma ejerc7 : (exists x, P x /\ Q a) /\ (forall x, P x -> R x) -> ( Q a /\ exists y, P y /\ R y).
intros.
split.
destruct H.
destruct H as [x H1].
destruct H1.
trivial.
destruct H.
destruct H as [x H1].
destruct H1.
assert (R x).
apply H0.
trivial.
exists x.
split.
trivial.
trivial.
Qed.


End LogicaDePredicados.




