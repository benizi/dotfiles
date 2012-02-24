fun! ToFlashCards()
	%s///ge
	%s/\ze–/\r/ge
	%s/–.\{-}\zs\s\+\([A-Za-z0-9][0-9]\?\.\)/\r\1/ge
	silent! g/^–$/d
	%s/ \([A-Za-z0-9][0-9]\?\. \)/\r\1/ge
	$s/\n\+\%$//e
endfun
