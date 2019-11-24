let swapf = ref 0 in
	swapf :=
	(funf swap [n,m] =>
		let x1 = ref n in
		let x2 = ref m in
		let x3 = ref (!x2) in
			x2 := !x1;
			x1 := !x3
		end
		end
		end);
	!swapf <+> 3 <+> 4
end