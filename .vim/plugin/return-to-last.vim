aug ReturnToLast
	au!
	au BufReadPost * if line("'\"") > 0 && line("'\"") <= line('$') | exe 'normal g`"' | endif
aug END
