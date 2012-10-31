type ('a,'b) t = 
  {size : int ref;
  table : ((('a * 'b) list) array) ref;
  hash : ('a->int) }

let create (n:int) (h:'a -> int): ('a, 'b) t = 
  {size = ref 0; 
  table = ref (Array.make n []);
	hash=h}

let remove (ht:('a,'b) t) (key:'a):unit = 
	let i = (ht.hash key)mod (Array.length !(ht.table)) in
  let rec pop acc l = 
    match l with 
    | [] -> (false,List.rev acc)
    | (k',v)::t -> if k' = key then (true,List.rev acc @ t) else pop ((k',v)::acc) t in 
  let b,l' = pop [] !(ht.table).(i) in 
  !(ht.table).(i) <- l';
	if b then ht.size := !(ht.size) - 1 else ()
	
let rec resize (ht:('a,'b) t):unit =
  let n = Array.length !(ht.table) in
  let h' = create (2 * n) ht.hash in
  for i = 0 to n - 1 do
    List.iter
      (fun (k,v) -> add h' k v) 
      !(ht.table).(i)
  done;
  ht.table := !(h'.table)
	
and add (ht:('a,'b) t) (key:'a) (value:'b):unit =
	remove ht key;
	if !(ht.size) >= Array.length !(ht.table) * 2 then 
    resize ht;
  let i = (ht.hash key)mod (Array.length !(ht.table)) in 
  !(ht.table).(i) <- (key,value)::!(ht.table).(i);
  ht.size := !(ht.size) + 1
	
let find (ht:('a,'b) t) (key:'a): 'b = 
	let i = (ht.hash key)mod (Array.length !(ht.table))
	in (List.assoc key !(ht.table).(i)) 

let mem (ht:('a,'b) t) (key:'a):bool =
	let i = (ht.hash key)mod (Array.length !(ht.table)) in
	List.mem_assoc key !(ht.table).(i)
	
let iter (f:('a -> 'b -> unit)) (ht: ('a,'b)t):unit = 
	Array.iter (fun lst-> (List.iter (fun (a,b)-> f a b) lst)) !(ht.table)
	
let fold (f:('a -> 'b -> 'c -> 'c)) (ht:('a,'b) t) (init:'c): 'c= 
	let x= ref init in
	(Array.iter (fun lst-> x:=(List.fold_left (fun ac (a,b)-> f a b ac) !x lst)) !(ht.table));
	!x

let length (ht:('a,'b) t):int = !(ht.size)
