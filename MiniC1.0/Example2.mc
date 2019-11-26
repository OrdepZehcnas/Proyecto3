let swapf = ref (funf swap [n,m] =>
		let x1 = ref n in
		let x2 = ref m in
		let x3 = ref (!x2) in
			x2 := !x1;
			x1 := !x3
		end
		end
		end) in
	!swapf <+> 10 <+> 300
end
end
end
