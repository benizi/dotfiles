set title titlestring=%{join(map(WindowsAndBuffers(),'pathshorten(v:val)')+[\"vim\"],\"\ -\ \")}
" set title titlestring=%{join(map(WindowsAndBuffers(),'fnamemodify(v:val,\":p:t\")')+[\"vim\"],\"\ -\ \")}
