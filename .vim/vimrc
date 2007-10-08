map!  a. hbmmi?\<2h"zdt.@z^Mywmx`mP xi
map!  a. hbmmi/\<2h"zdt.@z^Mywmx`mP xi
set noexpandtab softtabstop=4 tabstop=4 shiftwidth=4
set list listchars=tab:\ \ ,trail:·
" autocmd FileType * set tabstop=4|set shiftwidth=4|set softtabstop=4|set expandtab
set background=light
set binary
set hidden
syntax enable
map  {gq}
source ~/.vim/CSSify.vim
if has("cscope")
	set csprg=/usr/local/bin/cscope
	set csto=0
	set cst
	set nocsverb
	" add any database in current directory
	if filereadable("cscope.out")
		cs add cscope.out
	" else add database pointed to by environment
	elseif $CSCOPE_DB != ""
		cs add $CSCOPE_DB
	endif
	set csverb
endif
