" local syntax file - set colors on a per-machine basis:
" vim: tw=0 ts=4 sw=4
" Vim color file
" Maintainer:	Dr. J. Pfefferl <johann.pfefferl@agfa.com>
" Source:	$Source: /MISC/projects/cvsroot/user/pfefferl/vim/colors/nuvola.vim,v $
" Id:	$Id: nuvola.vim,v 1.14 2003/08/11 14:03:28 pfefferl Exp $
" Last Change:	$Date: 2003/08/11 14:03:28 $

" Intro {{{1
set background=light
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "nuvola"

" Normal {{{1
hi Normal   ctermfg=16 ctermbg=231

" Search {{{1
hi IncSearch    cterm=UNDERLINE ctermfg=16 ctermbg=221
hi Search term=reverse    cterm=NONE ctermfg=16 ctermbg=221

" Messages {{{1
hi ErrorMsg cterm=BOLD ctermfg=160 ctermbg=NONE
hi! link WarningMsg ErrorMsg
hi ModeMsg cterm=BOLD ctermfg=27 ctermbg=NONE
hi MoreMsg ctermbg=NONE ctermfg=29
hi! link Question MoreMsg

" Split area {{{1
hi StatusLine term=BOLD,reverse    cterm=BOLD ctermbg=75 ctermfg=231
hi StatusLineNC cterm=NONE ctermbg=75 ctermfg=255
hi! link VertSplit StatusLineNC
hi WildMenu cterm=UNDERLINE ctermfg=75 ctermbg=255

" Diff {{{1
hi DiffText   cterm=NONE ctermfg=202 ctermbg=224
hi DiffChange cterm=NONE ctermfg=22 ctermbg=194
hi DiffDelete cterm=NONE ctermfg=21 ctermbg=194
hi! link DiffAdd DiffDelete

" Cursor {{{1
hi Cursor       cterm=none ctermfg=16 ctermbg=214
"hi lCursor      cterm=NONE ctermfg=231 ctermbg=93
hi CursorIM     cterm=NONE ctermfg=231 ctermbg=93

" Fold {{{1
hi Folded cterm=NONE ctermbg=157 ctermfg=16
"hi FoldColumn cterm=NONE ctermbg=151 ctermfg=16
hi! link FoldColumn Folded

" Other {{{1
hi Directory    cterm=NONE ctermfg=21 ctermbg=NONE
hi LineNr       cterm=NONE ctermfg=103 ctermbg=NONE
hi NonText      cterm=BOLD ctermfg=57 ctermbg=255
"hi SpecialKey   cterm=NONE ctermfg=130 ctermbg=NONE
hi Title        cterm=BOLD ctermfg=19 ctermbg=NONE
hi Visual term=reverse   cterm=NONE ctermfg=16 ctermbg=153
hi VisualNOS term=reverse   cterm=UNDERLINE ctermfg=16 ctermbg=153

" Syntax group {{{1
hi Comment term=BOLD  ctermfg=240
hi Constant term=UNDERLINE  ctermfg=125
hi Error term=REVERSE   ctermbg=196 ctermfg=231
hi Identifier term=UNDERLINE  ctermfg=21
hi Number   term=UNDERLINE  cterm=NONE ctermfg=34
hi PreProc term=UNDERLINE  ctermfg=26
hi Special term=BOLD  ctermfg=196
hi Statement term=BOLD  cterm=NONE ctermfg=202
hi Tag term=BOLD  ctermfg=22
hi Todo term=STANDOUT   ctermfg=21 ctermbg=226
hi Type term=UNDERLINE  cterm=NONE ctermfg=21
hi! link String	Constant
hi! link Character	Constant
hi! link Boolean	Constant
hi! link Float		Number
hi! link Function	Identifier
hi! link Conditional	Statement
hi! link Repeat	Statement
hi! link Label		Statemengreen
hi! link Operator	Statement
hi! link Keyword	Statement
hi! link Exception	Statement
hi! link Include	PreProc
hi! link Define	PreProc
hi! link Macro		PreProc
hi! link PreCondit	PreProc
hi! link StorageClass	Type
hi! link Structure	Type
hi! link Typedef	Type
hi! link SpecialChar	Special
hi! link Delimiter	Special
hi! link SpecialComment Special
hi! link Debug		Special

" HTML {{{1
hi htmlLink                 cterm=UNDERLINE ctermfg=21 ctermbg=NONE
hi htmlBold                 cterm=BOLD
hi htmlBoldItalic           cterm=BOLD,ITALIC
hi htmlBoldUnderline        cterm=BOLD,UNDERLINE
hi htmlBoldUnderlineItalic  cterm=BOLD,UNDERLINE,ITALIC
hi htmlItalic               gui=ITALIC
hi htmlUnderline            cterm=UNDERLINE
hi htmlUnderlineItalic      cterm=UNDERLINE,ITALIC

" vim600:foldmethod=marker
