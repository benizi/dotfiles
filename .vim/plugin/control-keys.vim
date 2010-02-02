" Fix 'application-mode' <C-arrow> keys on non-xterm terminal emulators
"  since often the terminfo has the 'normal-mode' keymaps
" Will run if TERM isn't xterm, and either:
"  no <Left> key is defined,
"  or the <Left> key (erroneously) starts with '^[[' (instead of '^[O')
if $TERM !~ 'xterm' && (!exists("&t_kl") || &t_kl =~ '^\e\[')
	let s:arrows = { 'Up': 'a', 'Down': 'b', 'Right': 'c', 'Left': 'd' }
	for mapper in [ 'map', 'map!' ]
		for rhs in keys(s:arrows)
			let lhs=s:arrows[rhs]
			" rxvt-unicode sends \eO[abcd]
			exe mapper." <ESC>O".lhs." <C-".rhs.">"
			" mlterm sends \eO1;5[ABCD]
			exe mapper." <ESC>O1;5".toupper(lhs)." <C-".rhs.">"
		endfor
	endfor
endif
