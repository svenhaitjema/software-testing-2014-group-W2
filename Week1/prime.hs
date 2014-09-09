prime0 n | n < 1 = error "not a positive integer" 
		| n == 1 = False
		| otherwise = ld n == n