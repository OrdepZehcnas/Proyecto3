let x = ref 0 in
	let y = ref 10 in
		let z = ref (!y) in
			y := !x;
			x := !z
		end
	end
end