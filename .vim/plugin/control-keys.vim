if $TERM !~ 'xterm' && (!exists("&t_kl") || &t_kl =~ '^\e\[')
	let s:arrows = { 'Up': 'a', 'Down': 'b', 'Right': 'c', 'Left': 'd' }
	for mapper in [ 'map', 'map!' ]
		for rhs in keys(s:arrows)
			let lhs=s:arrows[rhs]
			exe mapper." <ESC>O".lhs." <C-".rhs.">"
			exe mapper." <ESC>O1;5".toupper(lhs)." <C-".rhs.">"
		endfor
	endfor
endif
