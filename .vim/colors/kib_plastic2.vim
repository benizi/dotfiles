" Vim color file
" Name: kib_plastic
" Maintainer: Kib² <kib2@free.fr>
" Version: 0.1
" Last Time Modified: 02.01.2007

" Couleur du fond d'ecran
set background=light
if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

let g:colors_name="kib_plastic"

" Help : type ':highlight' and read...

" ======================================================
" ==================================== normal Mode:
" ======================================================

" ==== Some constants : 

" Normal : foreground and background
hi Normal	ctermbg=255 ctermfg=239

" ==== Titles : 
hi Title	ctermfg=64


" ==== Comments : any comment
hi Comments	ctermfg=27 " any comment

" ===== Constants :
hi Constants ctermfg=68 " any constant   
hi String ctermfg=74  " a string constant: "this is a string"
hi Character ctermfg=61  " a character constant: 'c', '\n'
hi Number ctermfg=26  " a number constant: 234, 0xff
hi Boolean ctermfg=143  " a boolean constant: TRUE, false
hi Float ctermfg=143  " a floating point constant: 2.3e10

" ===== Identifier : any variable name
hi Identifier	ctermfg=208   " function name (also: methods for classes)

" ===== Statements : Les mots cles de chaque language
hi Statement	ctermfg=92 " par défaut
hi Conditional ctermfg=64 " if, then, else, endif, switch, etc.
hi Repeat ctermfg=76 " boucles for, do, while, etc.
hi Label ctermfg=113 " case, default, etc.
hi Operator ctermfg=215  " "sizeof", "+", "*", etc.
hi Keyword ctermfg=208  " any other keyword
hi Exception ctermfg=166  " try, catch, throw

" ==== PreProc : generic Preprocessor
hi PreProc	ctermfg=94 " generic Preprocessor
hi Include ctermfg=94  " preprocessor #include
hi Define ctermfg=94  " preprocessor #define
hi Macro ctermfg=94  " same as Define
hi PreCondit ctermfg=94  " preprocessor #if, #else, #endif, etc.

" ==== Types : int, long, char, etc.
hi Type		ctermfg=16   " int, long, char, etc.
hi StorageClass ctermfg=16  " static, register, volatile, etc.
hi Structure ctermfg=16  " struct, union, enum, etc.
hi Typedef ctermfg=16  " A typedef
 
" ==== Special : any special symbol
hi Special	ctermfg=64 " any special symbol
hi SpecialChar ctermfg=64   " special character in a constant
hi Tag ctermfg=64   " you can use CTRL-] on this
hi Delimiter ctermfg=64   " character that needs attention
hi SpecialComment ctermfg=64   " special things inside a comment
hi Debug ctermfg=64   " debugging statements

" ==== Underlined : text that stands out, liens HTML
hi Underlined	ctermfg=39 cterm=none

" ==== Ignore :
hi Ignore ctermfg=208  " left blank, hidden

" ==== Error : any erroneous construct
hi Error ctermfg=88  "

" ==== Todo :
hi Todo		ctermbg=166 ctermfg=255 " keywords TODO FIXME and XXX

" ======================================================
" ==================================== End Normal mode
" ======================================================

" ======================================================
" ==================================== Start Python Mode:
" ==== Dmitry Vasiliev as written a very good Python.vim
" ==== syntax file, wich brings more features. See:
" ==== http://www.hlabs.spb.ru/vim/python.vim
" ======================================================

" pythonSync: 

" pythonStatement:
hi pythonStatement ctermfg=64 " Python Statement : break, continue, del

" pythonImport:
hi pythonImport ctermfg=92 " Python Imports : break, continue, del

" pythonRepeat:
hi pythonRepeat ctermfg=64 " Python Repeat : for, while, etc.

" pythonConditional:
hi pythonConditional ctermfg=172 " Python conditonnal : if, elif, else, then

" pythonPrecondit:
hi pythonPrecondit ctermfg=196 " python Precondit : import from

" pythonOperator: 
hi pythonOperator ctermfg=196 " pythonOperator : and, in, is, not, or

" pythonComment:
hi pythonComment ctermfg=27 " Python comments

" pythonEscape:


" pythonRawString:
hi pythonRawString ctermfg=139 " Python raw strings

" pythonBuiltin: True False bool enumerate set frozenset help


" pythonException:

" ===== Linked 
" pythonFunction: links to Function

" pythonTodo: links to Todo

" pythonDecorator: links to Define 

" pythonString: links to String

" pythonNumber: links to Number

" pythonSpaceError: links to Error

" ======================================================
" ==================================== End Python Mode:
" ======================================================
hi Cursor ctermbg=203 ctermfg=231
"hi CursorIM
hi Directory	ctermfg=152
"hi DiffAdd
"hi DiffChange
"hi DiffDelete
"hi DiffText
"hi ErrorMsg
hi VertSplit	ctermbg=102 ctermfg=236 cterm=none
hi Folded	    ctermbg=250 ctermfg=16             " How the text appears once folded
hi FoldColumn	ctermbg=250 ctermfg=240 
hi LineNr	    ctermbg=255 ctermfg=215             " Line numbers
hi MatchParen	ctermbg=250 ctermfg=236
hi ModeMsg	    ctermfg=208
hi MoreMsg	    ctermfg=208
hi NonText	    ctermbg=255 ctermfg=240
hi Question	    ctermfg=146
hi Search	    ctermbg=221 ctermfg=178
hi IncSearch	ctermbg=178 ctermfg=221
hi SpecialKey	ctermfg=166
hi StatusLine	ctermbg=102 ctermfg=236 cterm=none
hi StatusLineNC	ctermbg=102 ctermfg=221 cterm=none
hi Visual	    ctermbg=203 ctermfg=16             " Selection of text in Visual Mode
"hi VisualNOS
hi WarningMsg	guifg=salmon
hi WildMenu
hi Menu        ctermbg=203 ctermfg=16
"hi Scrollbar   guibg=grey30 guifg=tan
"hi Tooltip
hi Pmenu	    ctermbg=250 ctermfg=240
hi PmenuSel	    ctermbg=255 ctermfg=236
hi CursorLine	ctermbg=235


"  {{{ terminal
" TODO
" }}}

"vim: sw=4
