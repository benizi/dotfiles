if !exists("g:application_name")
	let g:application_name='vim'
endif
set title titlestring=%{join([g:application_name]+map(WindowsAndBuffers(),'pathshorten(v:val)'),\"\ -\ \")}
" set title titlestring=%{join(map(WindowsAndBuffers(),'fnamemodify(v:val,\":p:t\")')+[\"vim\"],\"\ -\ \")}
