" map!  a. hbmmi?\<2h"zdt.@z^Mywmx`mP xi
" map!  a. hbmmi/\<2h"zdt.@z^Mywmx`mP xi
if filereadable(expand("~/.vim-at-usc"))
	let g:vim_at_work = 1
else
	let g:vim_at_work = 0
endif
if g:vim_at_work
	set expandtab softtabstop=3 tabstop=3 shiftwidth=3
	let disable_detectindent = 1 " Don't detect indent
else
	set noexpandtab softtabstop=4 tabstop=4 shiftwidth=4
endif
set list listchars=tab:\ \ ,trail:·
if $TERM =~ 'rxvt'
	if g:vim_at_work
		set t_Co=88
	else
		set t_Co=256
	endif
endif
if exists('$VIM_BACK')
	exec "set background=" . $VIM_BACK
else
	if $TERM == 'cygwin'
		set background=dark
	else
		set background=light
	endif
endif
set hidden
syntax enable
syntax sync maxlines=2000
filetype plugin on
set ofu=syntaxcomplete#Complete
imap <C-@> <C-Space>
" autocmd FileType * set tabstop=4|set shiftwidth=4|set softtabstop=4|set expandtab
autocmd BufRead *.thtml set syntax=thtml
autocmd FileType php noremap <C-M> :w!<CR>:!php %<CR>
" map  {gq}
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
let g:tex_flavor = "context"
"nmap <F5> :sil ! xpdf %:p:r.pdf<cr>
nmap <F5> :sil ! acroread %:p:r.pdf &<cr>
nmap <F6> :make %<cr>
"vv from http://items.sjbach.com/319/configuring-vim-right ***
set history=1000
set wildmenu
" set wildmode=list:longest
set ignorecase
set smartcase
if isdirectory(expand("~/.vim-tmp")) < 1
	if exists("*mkdir")
		call mkdir(expand("~/.vim-tmp"), "p", 0700)
	endif
endif
set backupdir=~/.vim-tmp//,~/.tmp//,~/tmp//,/tmp//
set directory=~/.vim-tmp//,~/.tmp//,~/tmp//,/tmp//
"^^ from http://items.sjbach.com/319/configuring-vim-right ***
