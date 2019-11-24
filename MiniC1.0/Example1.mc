let x = ref (pred 1) in 
	ref 0 := suc 5;
	x := (!x)+3
end