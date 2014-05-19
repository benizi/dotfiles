" Vim color file
" Maintainer: Andrés Suárez <ansuga@gmail.com> 
" Last Change: 2009/02/10 Tue 16:55
"     version: 1.2
" This color scheme uses a light background.

set background=light
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "fruidle2"

" Normal
hi Normal       ctermfg=238 ctermbg=231

" Search
hi IncSearch    cterm=UNDERLINE ctermfg=238 ctermbg=87
hi Search       cterm=NONE ctermfg=238 ctermbg=227

" Messages
hi ErrorMsg     cterm=NONE ctermfg=196 ctermbg=224
hi WarningMsg   cterm=NONE ctermfg=196 ctermbg=224
hi ModeMsg      cterm=NONE ctermfg=160 ctermbg=NONE
hi MoreMsg      cterm=NONE ctermfg=29 ctermbg=NONE
hi Question     cterm=NONE ctermfg=208 ctermbg=NONE

" Split area
hi StatusLine   cterm=BOLD ctermfg=231 ctermbg=238
hi StatusLineNC cterm=NONE ctermfg=248 ctermbg=238
hi VertSplit    cterm=NONE ctermfg=231 ctermbg=238
hi WildMenu     cterm=BOLD ctermfg=231 ctermbg=160

" Diff
hi DiffText     cterm=NONE ctermfg=167 ctermbg=224
hi DiffChange   cterm=NONE ctermfg=65 ctermbg=194
hi DiffDelete   cterm=NONE ctermfg=63 ctermbg=225
hi DiffAdd      cterm=NONE ctermfg=63 ctermbg=225

" Cursor
hi Cursor       cterm=NONE ctermfg=21 ctermbg=45
hi lCursor      cterm=NONE ctermfg=231 ctermbg=93
hi CursorIM     cterm=NONE ctermfg=231 ctermbg=93

" Fold
hi Folded       cterm=NONE ctermfg=23 ctermbg=152
hi FoldColumn   cterm=NONE ctermfg=72 ctermbg=255

" Other
hi Directory    cterm=NONE ctermfg=25 ctermbg=NONE
hi LineNr       cterm=NONE ctermfg=145 ctermbg=NONE
hi NonText      cterm=BOLD ctermfg=37 ctermbg=255
hi SpecialKey   cterm=NONE ctermfg=63 ctermbg=NONE
hi Title        cterm=NONE ctermfg=25 ctermbg=153
hi Visual       cterm=NONE ctermfg=238 ctermbg=254
" hi VisualNOS    cterm=NONE ctermfg=238 ctermbg=254

" Syntax group
hi Comment      cterm=NONE ctermfg=160 ctermbg=NONE
hi Constant     cterm=NONE ctermfg=26 ctermbg=NONE
hi Number       cterm=NONE ctermfg=126 ctermbg=NONE
hi Identifier   cterm=NONE ctermfg=26 ctermbg=NONE
hi Error        cterm=BOLD ctermfg=231 ctermbg=160
hi Ignore       cterm=NONE ctermfg=231 ctermbg=NONE
hi PreProc      cterm=BOLD ctermfg=166 ctermbg=NONE
hi Special      cterm=NONE ctermfg=64 ctermbg=NONE
hi Statement    cterm=NONE ctermfg=208 ctermbg=NONE
hi Todo         cterm=UNDERLINE ctermfg=197 ctermbg=225
hi Type         cterm=BOLD ctermfg=26 ctermbg=NONE
hi Underlined   cterm=UNDERLINE guifg=fg ctermbg=NONE
hi Include      cterm=BOLD ctermfg=160 ctermbg=NONE 
hi Function     cterm=BOLD ctermfg=34 ctermbg=NONE
hi String       cterm=NONE ctermfg=34 
