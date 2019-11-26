let x = ref 2 in
let y = ref 3 in
let pot = ref (fun [n,m] =>
    	      	let res = ref (!n) in
		    while (not ((!m) == 1)) {
		    	  res := (!res)*(!n);
		    	  m := pred(!m)
		    }
		end)
	in !pot <+> x <+> y
end
end
end
