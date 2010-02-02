set noexpandtab softtabstop=4 tabstop=4 shiftwidth=4
set list listchars=tab:\ \ ,trail:·
if $TERM =~ 'rxvt'
	set mouse=a
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
colorscheme dual-converted
set hidden
set laststatus=2
filetype plugin indent on
syntax enable
syntax sync maxlines=2000
set ofu=syntaxcomplete#Complete
imap <C-@> <C-Space>
" autocmd FileType * set tabstop=4|set shiftwidth=4|set softtabstop=4|set expandtab
autocmd BufRead *.thtml set syntax=thtml
" map  {gq}
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
if filereadable(expand("~/.vimrc.local"))
	source ~/.vimrc.local
endif
