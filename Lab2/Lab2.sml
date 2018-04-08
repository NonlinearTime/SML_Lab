structure Lab2 = 
struct
datatype 'a tree = 
	Empty 
	| Node of 'a tree * 'a * 'a tree


	fun reverse (l : int list) : int list =
		let
			val l' = []
			fun rev (l : int list, l_rev : int list) : int list = 
				case l of
					[] => l_rev
					| r::L => rev(L, r::l_rev)
		in
			rev(l,l')
		end

	fun reverse'' (l : int list) : int list = 
		case l of 
			[] => []
			| (x::L) => reverse(L) @[x]

	fun reverse' (l : int list) : int list = 
		List.rev(l)

	fun interleave(L : int list ,R : int list) : int list = 
		case (L,R) of
			([],_) => R
			| (_,[]) => L
			| (l::L',r::R') => l::r::interleave(L',R') 

	fun split (L : int list) : int list * int * int list = 
		case L of [x] => ([],x,[])
			| (rr::Rr) =>
			let
				fun split_helper (L:int list , x: int , R: int list) : int list * int * int list = 
					if abs(List.length(L) - List.length(R)) <= 1 then (L,x,R) 
					else case R of [x] => ([],x,[])
								| (r::R') => split_helper(L@[x], r , R')
			in
				split_helper([],rr,Rr)
			end

	fun listToTree(L : int list) : tree = 
		case L of 
			[] => Empty
			| _ => 
			let
				val (L1,x,L2) = split L
			in
				Node(listToTree(L1),x,listToTree(L2))
			end

	fun trav Empty = [] 
		| trav (Node(t1,x,t2)) = trav t1 @ (x :: trav t2)

	fun revT Empty = Empty
		| revT (Node(t1,x,t2)) = Node(revT t2, x , revT t1)

	fun binarySearch Empty = false
		| binarySearch (Node(_,2,_)) = true
		| binarySearch (Node(t1,_,t2)) = binarySearch t1 orelse binarySearch t2

end