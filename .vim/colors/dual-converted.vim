"==============================================================================.
"        File: dual.vim                                                        |
"     License: Public Domain, FREE as LOVE.                                    |
" Description: The Dual colorscheme you've been longing for. On VIM, it uses   |
"              dark colors. On GVIM, it uses bright colors without bold,       |
"              italic nor underline font styles (except for spell checking     |
"              and some GUI elements).                                         |
"        Note: - Only standard (for all languages) highlight-groups are set.   |
"              - This colorscheme _should_ look nice everywhere.               |
"==============================================================================|
"      Author: drachenkiraa, {_drachen_kiraa_}@{_gmail_}.{com_}  (remove: _{}) |
" Last Change: 2009 Jul 1                                                      |
"     Version: 1.1                                                             |
"==========================================================================={{{1
" Changes:                                                                     |
"  1.0: Public release.                                                        |
"  1.1: A LOT of changes, if you are interested to know what really changed:   |
"       Changed 'CursorLine', 'CursorColumn' to use 'cterm=underline' instead of |
"         using 'guibg' color.                                                 |
"       Made 'PMenuSel' guibg color more darker.                               |
"       Changed  'SpellRare' guisp color to DarkMagenta.                       |
"       Tab pages, Status and Split bars now use gray-blue and blue-white      |
"         as colors (for both terminal and gui).                               |
"       Changed 'MoreMsg' guifg color from Gray30 to DarkGreen.                |
"       Changed 'Question' gui option to italic.                               |
"       Changed 'Directory' ctermfg from Yellow to Blue.                       |
"       Changed all Diff colors to use their darker versions.                  |
"       Made 'LineNr' transparent and brighter.                                |
"       Made 'SignColumn' transparent.                                         |
"       Made 'Folded' and 'FoldColumn' darkblue over transparent for cterm,    |
"         and made their GUI colors more brighter.                             |
"       Changed 'IncSearch' ctermbg and guibg colors from                      |
"         LighMagenta to Green.                                                |
"       Added 'ctermfg'and 'guifg' options to 'Search'.                        |
"       Made 'Comment' guifg and ctermfg colors more brighter.                 |
"       Linked 'Number', 'Float' and 'Boolean' to 'Constant'.                  |
"       Changed 'Constant' ctermfg color from Magenta to Yellow,               |
"         and made guifg colors more darker.                                   |
"       Changed 'Character' ctermfg color from Magenta to DarkRed,             |
"         and made guifg colors more darker.                                   |
"       Made 'String' ctermfg color more darker.                               |
"       Linked 'Function' to 'Identifier', and changed their ctermfg and guifg |
"         to Magenta and a dark violet respectively.                           |
"       Linked 'Conditional', 'Repeat', 'Operator', 'Keyword', 'Label' and     |
"         'Exception' to 'Statement.                                           |
"       Changed 'Statement' ctermfg color from Green to Blue.                  |
"       Linked 'StorageClass', 'Structure' and 'Typedef' to 'Type'.            |
"       Changed 'Type' ctermfg color from Cyan to Blue.                        |
"       Linked 'Include', 'Define' and 'Macro' to 'PreProc'.                   |
"       Changed 'PreProc' ctermfg color to a darker version.                   |
"       Changed 'PreCondit' ctermfg color from Blue to Yellow and guifg color  |
"         from Red to Gold like.                                               |
"       Changed 'Special', 'SpecialChar', 'Tag' and 'Delimiter' ctermfg color  |
"         from Red to Cyan and changed their guifg color to sky blue.          |
"       Changed 'MatchParen' guibg color from light magenta to DarkBlue.       |
"       Changed 'Todo' ctermbg and guibg colors to Green and DarkGreen.        |
"       Made 'Underlined'ctermbg transparent.                                  |
"       Set 'NonText' guibg color to the same as the 'FoldColumn' guibg color. |
"==============================================================================|
"  Color Test: :he group-name                                                  |
"              :so $VIMRUNTIME/syntax/hitest.vim                               |
"   Tested On: - Linux (gvim v6.3, v6.4);                                      |
"              - Standard Linux Terminal (vim v6.3, v6.4);                     |
"              - Xterm, Rxvt, Konsole, gnome-terminal (vim v6.3, v6.4).        |
"              - Windows (gvim v7.1, v7.2);                                    |
"              - DOS (vim v7.1, v7.2).                                         |
"        TODO: * Test this colorscheme on newer versions of vim/gvim for Linux |
"                and other systems (*BSD, Mac, Amiga?).                        |
"              * Are all the has("feature") checks really worth?               |
"                Please enlighten me if I'm wrong.                             |
"==============================================================================|
" Random Tips:                                                                 |
" * If your terminal supports more than 8 colors (which is the case of most    |
"   modern xterms, rxvts, and others), then it is worth adding the following   |
"   lines somewhere into your .vimrc:                                          |
"       if &term =~ "xterm"                                                    |
"         set t_Co=16                                                          |
"       endif                                                                  |
"   That'll make this colorscheme look a lot better on such terminals.         |
"   For further help checkout:                                                 |
"       :he term-dependent-settings                                            |
"       :he term                                                               |
"===========================================================================}}}1
" Initial setup stuff {{{1
" Remove existing highlighting
" if has("gui_running")
"   set background=light
" else
"   set background=dark
" endif
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "dual-converted"
hi Normal   ctermbg=231 ctermfg=16 cterm=none

" Vim >= 7.0 specific colors {{{1
if v:version >= 700
  " Cursor colors {{{2
  hi Cursor    ctermbg=18 ctermfg=bg cterm=none
  hi CursorLine   ctermbg=bg cterm=underline
  hi CursorColumn   ctermbg=bg cterm=underline
  " only for Win32, IME status
  if has('multi_byte_ime')
    hi CursorIM ctermbg=53 ctermfg=NONE cterm=none
  endif

  " Auto-completion Popup Menu colors {{{2
  hi Pmenu    ctermbg=192 ctermfg=fg cterm=none
  hi PmenuSel    ctermbg=58 ctermfg=bg cterm=bold
  hi PmenuSbar    ctermbg=113 ctermfg=fg cterm=none
  hi PmenuThumb    ctermbg=65 ctermfg=bg cterm=none

  " Tab colors {{{2
  if has("windows")
    hi TabLine   ctermbg=250 ctermfg=24 cterm=none
    hi TabLineFill   ctermbg=250 ctermfg=24 cterm=none
    hi TabLineSel   ctermbg=24 ctermfg=bg cterm=bold
  endif

  " Spell checking colors {{{2
  if has("spell")
    hi SpellBad    guisp=Red gui=undercurl
    hi SpellCap    guisp=Blue gui=undercurl
    hi SpellLocal    guisp=DarkCyan gui=undercurl
    hi SpellRare    guisp=DarkMagenta gui=undercurl
  endif

endif "}}}1
" Messages and other texts' colors {{{1
hi WarningMsg    ctermbg=bg ctermfg=160 cterm=none
hi ErrorMsg    ctermbg=160 ctermfg=bg cterm=none
hi ModeMsg    ctermbg=bg ctermfg=fg cterm=none
hi MoreMsg    ctermbg=bg ctermfg=22 cterm=none
hi Question    ctermbg=bg ctermfg=239 gui=italic
hi Directory    ctermbg=bg ctermfg=21 cterm=none
hi Title    ctermbg=bg ctermfg=21 cterm=bold

" Diff colors {{{1
if has("diff")
  hi DiffAdd    ctermbg=194 cterm=none
  hi DiffChange    ctermbg=189 cterm=none
  hi DiffDelete    ctermbg=224 cterm=none
  hi DiffText    ctermbg=195 cterm=none
endif

" Outline, Fold & Sign columns colors {{{1
hi LineNr   ctermbg=bg ctermfg=246 cterm=none
if has("folding")
  hi Folded    ctermbg=255 ctermfg=241 cterm=none
  hi FoldColumn    ctermbg=255 ctermfg=241 cterm=none
endif
if has("signs")
  hi SignColumn    ctermbg=255 ctermfg=27 cterm=none
endif

" Search & Special characters' colors {{{1
if has("extra_search")
  hi Search    ctermbg=226 ctermfg=fg cterm=none
  hi IncSearch    ctermbg=46 ctermfg=fg cterm=none
endif
hi NonText   ctermbg=255 ctermfg=248 cterm=none
hi SpecialKey   ctermbg=bg ctermfg=95 cterm=none

" Window Bars, Status line & Visual mode colors {{{1
hi StatusLine    ctermbg=24 ctermfg=bg cterm=none
if has("windows")
  hi StatusLineNC    ctermbg=250 ctermfg=24 gui=italic
endif
if has("vertsplit")
  hi VertSplit    ctermbg=250 ctermfg=24 cterm=bold
endif

if has("wildmenu")
  hi WildMenu   ctermbg=bg ctermfg=24 cterm=bold
endif

if has("visual")
  hi Visual    ctermbg=18 ctermfg=bg cterm=none
  hi VisualNOS    ctermbg=18 ctermfg=bg cterm=none
endif

" Syntax highlighting colors {{{1
hi Comment   ctermbg=bg ctermfg=28 cterm=none
hi link SpecialComment Comment

hi Character   ctermbg=bg ctermfg=160 cterm=none
hi String   ctermbg=bg ctermfg=160 cterm=none
hi Constant   ctermbg=bg ctermfg=202 cterm=none
hi link Number Constant
hi link Float Constant
hi link Boolean Constant

hi Identifier   ctermbg=bg ctermfg=90 cterm=none
hi link Function Identifier

hi Statement   ctermbg=bg ctermfg=20 cterm=none
hi link Conditional Statement
hi link Repeat Statement
hi link Operator Statement
hi link Keyword Statement
hi link Label Statement
hi link Exception Statement

hi Type   ctermbg=bg ctermfg=20 cterm=none
hi link StorageClass Type
hi link Structure Type
hi link Typedef Type

hi PreProc   ctermbg=bg ctermfg=58 cterm=none
hi PreCondit   ctermbg=bg ctermfg=100 cterm=none
hi link Include PreProc
hi link Define PreProc
hi link Macro PreProc

hi Special   ctermbg=bg ctermfg=62 cterm=none
hi SpecialChar   ctermbg=bg ctermfg=21 cterm=none
hi Tag   ctermbg=bg ctermfg=62 cterm=none
hi Delimiter   ctermbg=bg ctermfg=62 cterm=none
hi Debug   ctermbg=bg ctermfg=248 cterm=none

hi MatchParen    ctermbg=18 ctermfg=bg cterm=none
hi Error    ctermbg=bg ctermfg=196 cterm=none
hi Ignore    ctermbg=bg ctermfg=bg cterm=none
hi Todo    ctermbg=22 ctermfg=bg cterm=none
hi Underlined    ctermbg=bg ctermfg=21 cterm=underline
"}}}1
"==========================================================================={{{1
" vim: set et sw=2 sts=2 ts=8 nowrap:
" vim600: set fdc=2 fdm=marker:
