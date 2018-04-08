structure Lab1 = 
struct
	fun sum [] = 0
		| sum (x::L) = x + (sum L)

	fun mul [] = 1
		| mul (x::L) = x * (mul L)

	fun mult' ([ ], a) = a
		  | mult' (x :: L, a) = mult' (L, x * a)

	fun Mult' ([ ], a) = a
	    | Mult' (r::R, a) = Mult'(R, mult'(r,a)) 

	fun double (0 : int) : int = 0
	    | double n = 2 + double (n - 1)

	fun square n = 
		let
			fun square_helper (x : int ) : int = 
				case (x) of 
					0 => 0
					| 1 => n
					| _ => double(n) + square_helper(x - 2)
		in
			square_helper(n)
		end

	fun divisibleByThree (x : int) : bool = 
		case x of
			0 => true
			| 1 => false
			| 2 => false
			| _ => divisibleByThree (x - 3)

	fun oddP (0 : int) : bool = false
	  	    | oddP 1 = true
	  	    | oddP n = oddP (n - 2)

	fun testAll() = 
		let
	 		val _ = print("Test mul [1,2,3,4,5]:\n")
	 		val _ = print(Int.toString(mul ([1,2,3,4,5])))
	 		val _ = print("\nTest Mult' ([[1,2],[3,4,5]],2):\n")
	 		val _ = print(Int.toString(Mult' ([[1,2],[3,4,5]],2)))
	 		val _ = print("\nTest square [1,2,3,4,5]:\n")
	 		val _ = print(Int.toString(square(5)))
	 		val _ = print("\nTest divisibleByThree 111:\n")
	 		val _ = print(Bool.toString(divisibleByThree(111)))
	 		val _ = print("\nTest oddP 111\n")
	 		val _ = print(Bool.toString(oddP(111)) ^ "\n")
	 	in
	 		()
	 	end
end