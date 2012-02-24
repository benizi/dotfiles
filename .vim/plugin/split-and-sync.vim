fun! SplitSync()
	let original = winnr()
	split
	for win in [original,winnr()]
		echomsg "Setting up winnr(" win ")"
		call setwinvar(win, '&scb', 1)
	endfor
endfun

nmap <F5> :call SplitSync()<CR>
