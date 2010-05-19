set title titlestring=%{join([\"vim\"]+map(WindowsAndBuffers(),'pathshorten(v:val)'),\"\ -\ \")}
" set title titlestring=%{join(map(WindowsAndBuffers(),'fnamemodify(v:val,\":p:t\")')+[\"vim\"],\"\ -\ \")}
