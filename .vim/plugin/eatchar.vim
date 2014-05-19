fun! Eatchar(...)
	let c = nr2char(getchar(0))
	return c =~ (a:0 ? a:1 : '\s') ? '' : c
endfun
