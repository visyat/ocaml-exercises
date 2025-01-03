(** Check if 2 Positive Integers are Coprime ... *)
let coprime a b = 
  let rec gcd f = 
    match f with 
    | f when ((a mod f)=0) && ((b mod f)=0) -> f 
    | f when f=a -> 1 
    | f when f=b -> 1 
    | f -> gcd (f+1) in 
  match (gcd 2) with 
  | 1 -> true 
  | _ -> false;;
let comprime_test0 = (coprime 13 27 = true);;
let comprime_test1 = (coprime 20536 7826 = false);;

(** Get List of Prime Numbers within Range ... *)
let rec all_primes low high = 
  let rec is_prime low target = 
    match low with 
    | low when low=target -> true 
    | low when (target mod low = 0) -> false 
    | _ -> is_prime (low+1) target in 
  match low with 
  | low when low=high+1 -> []
  | low when (is_prime 2 low) -> low::(all_primes (low+1) high) 
  | low -> all_primes (low+1) high;;
(* let primes_test0 = (List.length (all_primes 2 7920) = 1000);; *)