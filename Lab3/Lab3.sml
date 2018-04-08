structure Lab3 = 
struct
datatype 'a tree = 
	Empty 
	| Node of 'a tree * 'a * 'a tree

	fun double (0 : int) : int = 0
	    | double n = 2 + double (n - 1)

	fun square n = 
		let
			fun square_helper (x : int ) : int = 
				case x of 
					0 => 0
					| 1 => n
					| _ => double(n) + square_helper(x - 2)
		in
			square_helper(n)
		end

	fun thenAddOne f x y = (f x) + y

	fun mapList (f,[]) = []
		| mapList (f,[x]) = [f x]
		| mapList (f,(x::L)) = (f x) :: (mapList (f,L))

	fun mapList' f = (fn x => mapList (f,x))

	fun oddP (0 : int) : bool = false
 	    | oddP 1 = true
 	    | oddP n = oddP (n - 2)

	fun findOdd [] = NONE
		| findOdd (x::L) = if oddP x then SOME x else findOdd L


	fun exists f [] = false
		|exists f (x::L) = if (f x) then true else exists f L

	fun forall f [] = false
		|forall f [x] = (f x)
		|forall f (x::L) = if not (f x) then false else forall f L

	fun treeFilter f Empty = Empty
		| treeFilter f (Node(l,x,r)) = 
		case (f x) of
			true => Node(treeFilter f l, SOME x, treeFilter f r)
			| false => Node(treeFilter f l, NONE, treeFilter f r)

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

	fun listToTree L = 
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

	fun sum [] = 0
		| sum (x::L) = x + (sum L)

	fun dropAt L P = 
		let
			val head = List.take(L,P)
			val tail = List.drop(L,P + 1)
		in
			head @ tail
		end

	fun iterate f s = 
		let 
			fun iterate' s p = 
				case p of
					0 => NONE
					| _ => 
					let 
						val s' = dropAt s (p - 1)
						val t = f s'
					in
						case t of
							NONE => iterate' s (p - 1)
							| _ => t
					end
		in
			iterate' s (List.length(s))
		end

	fun subsetSumOption [] 0 = NONE
		| subsetSumOption L 0 = NONE
		| subsetSumOption [] S = NONE
		| subsetSumOption L S = 
		let
			fun helper S L = 
				case L of
					[] => NONE
					| _ => if sum L = S then SOME L else iterate (helper S) L

		in
			helper S L
		end				
end