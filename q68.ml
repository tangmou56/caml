type list_char=vide|cons of (char*int)*list_char;;
let liste=cons((`a`,20),cons((`c`,30),vide));;
let tete=function cons(a,b)->a|_-> failwith "impossible";;
let suite=function cons(a,b)->b|_->failwith "impossible";;
let rec ajoute=fun vide l2 -> l2
			|(cons(tete,reste)) l2 ->
				cons(tete,(ajoute reste l2));;
let rec f l=function (str,ent) as coup->if l=vide then vide
			else if fst(tete l)=str then cons((str,snd(tete l)+ent),f(suite l) coup)
			
			else if fst(tete l)<str && str >fst(tete (suite l)) then cons(tete l,cons(coup,(f(suite l) coup)))
			else cons((tete l),f(suite l) coup);;
snd(tete liste);;
f liste (`b`,15);;


