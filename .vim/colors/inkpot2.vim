" Vim color file
" Name:       inkpot.vim
" Maintainer: Ciaran McCreesh <ciaran.mccreesh@blueyonder.co.uk>
" This should work in the GUI, rxvt-unicode (88 colour mode) and xterm (256
" colour mode). It won't work in 8/16 colour terminals.
"
" To use a black background, :let g:inkpot_black_background = 1

set background=dark
hi clear
if exists("syntax_on")
   syntax reset
endif

let colors_name = "inkpot"

" map a urxvt cube number to an xterm-256 cube number
fun! <SID>M(a)
    return strpart("0135", a:a, 1) + 0
endfun

" map a urxvt colour to an xterm-256 colour
fun! <SID>X(a)
    if &t_Co == 88
        return a:a
    else
        if a:a == 8
            return 237
        elseif a:a < 16
            return a:a
        elseif a:a > 79
            return 232 + (3 * (a:a - 80))
        else
            let l:b = a:a - 16
            let l:x = l:b % 4
            let l:y = (l:b / 4) % 4
            let l:z = (l:b / 16)
            return 16 + <SID>M(l:x) + (6 * <SID>M(l:y)) + (36 * <SID>M(l:z))
        endif
    endif
endfun

if ! exists("g:inkpot_black_background")
    let g:inkpot_black_background = 1
endif

    if ! g:inkpot_black_background
        hi Normal         cterm=NONE   ctermfg=181   ctermbg=234
    else
        hi Normal         cterm=NONE   ctermfg=181   ctermbg=16
    endif

    hi IncSearch      cterm=BOLD   ctermfg=236   ctermbg=173
    hi Search         cterm=NONE   ctermfg=236   ctermbg=173
    hi ErrorMsg       cterm=BOLD   ctermfg=231   ctermbg=167
    hi WarningMsg     cterm=BOLD   ctermfg=231   ctermbg=173
    hi ModeMsg        cterm=BOLD   ctermfg=103   ctermbg=NONE
    hi MoreMsg        cterm=BOLD   ctermfg=103   ctermbg=NONE
    hi Question       cterm=BOLD   ctermfg=220   ctermbg=NONE

    hi StatusLine     cterm=BOLD   ctermfg=250   ctermbg=238
    hi User1          cterm=BOLD   ctermfg=48   ctermbg=238
    hi User2          cterm=BOLD   ctermfg=61   ctermbg=238
    hi StatusLineNC   cterm=NONE   ctermfg=250   ctermbg=238
    hi VertSplit      cterm=NONE   ctermfg=250   ctermbg=238

    hi WildMenu       cterm=BOLD   ctermfg=255   ctermbg=61

    hi MBENormal                 ctermfg=181   ctermbg=236
    hi MBEChanged                ctermfg=255   ctermbg=236
    hi MBEVisibleNormal          ctermfg=252   ctermbg=60
    hi MBEVisibleChanged         ctermfg=255   ctermbg=60

    hi DiffText       cterm=NONE   ctermfg=230   ctermbg=238
    hi DiffChange     cterm=NONE   ctermfg=230   ctermbg=60
    hi DiffDelete     cterm=NONE   ctermfg=230   ctermbg=238
    hi DiffAdd        cterm=NONE   ctermfg=230   ctermbg=238

    hi Cursor         cterm=NONE   ctermfg=238   ctermbg=105
    hi lCursor        cterm=NONE   ctermfg=238   ctermbg=120
    hi CursorIM       cterm=NONE   ctermfg=238   ctermbg=105

    hi Folded         cterm=NONE   ctermfg=252   ctermbg=54
    hi FoldColumn     cterm=NONE   ctermfg=104   ctermbg=236

    hi Directory      cterm=NONE   ctermfg=48   ctermbg=NONE
    hi LineNr         cterm=NONE   ctermfg=104   ctermbg=236
    hi NonText        cterm=BOLD   ctermfg=104   ctermbg=NONE
    hi SpecialKey     cterm=BOLD   ctermfg=135   ctermbg=NONE
    hi Title          cterm=BOLD   ctermfg=131   ctermbg=NONE
    hi Visual         cterm=NONE   ctermfg=255   ctermbg=60

    hi Comment        cterm=NONE   ctermfg=172   ctermbg=NONE
    hi Constant       cterm=NONE   ctermfg=222   ctermbg=NONE
    hi String         cterm=NONE   ctermfg=222   ctermbg=238
    hi Error          cterm=NONE   ctermfg=231   ctermbg=238
    hi Identifier     cterm=NONE   ctermfg=213   ctermbg=NONE
    hi Ignore         cterm=NONE
    hi Number         cterm=NONE   ctermfg=215   ctermbg=NONE
    hi PreProc        cterm=NONE   ctermfg=66   ctermbg=NONE
    hi Special        cterm=NONE   ctermfg=140   ctermbg=NONE
    hi SpecialChar    cterm=NONE   ctermfg=140   ctermbg=238
    hi Statement      cterm=NONE   ctermfg=105   ctermbg=NONE
    hi Todo           cterm=BOLD   ctermfg=236   ctermbg=179
    hi Type           cterm=NONE   ctermfg=213   ctermbg=NONE
    hi Underlined     cterm=BOLD   ctermfg=178   ctermbg=NONE
    hi TaglistTagName cterm=BOLD   ctermfg=105   ctermbg=NONE

    hi perlSpecialMatch   cterm=NONE ctermfg=140   ctermbg=238
    hi perlSpecialString  cterm=NONE ctermfg=140   ctermbg=238

    hi cSpecialCharacter  cterm=NONE ctermfg=140   ctermbg=238
    hi cFormat            cterm=NONE ctermfg=140   ctermbg=238

    hi doxygenBrief                 cterm=NONE ctermfg=215   ctermbg=NONE
    hi doxygenParam                 cterm=NONE ctermfg=222   ctermbg=NONE
    hi doxygenPrev                  cterm=NONE ctermfg=222   ctermbg=NONE
    hi doxygenSmallSpecial          cterm=NONE ctermfg=222   ctermbg=NONE
    hi doxygenSpecial               cterm=NONE ctermfg=222   ctermbg=NONE
    hi doxygenComment               cterm=NONE ctermfg=136   ctermbg=NONE
    hi doxygenSpecial               cterm=NONE ctermfg=215   ctermbg=NONE
    hi doxygenSpecialMultilineDesc  cterm=NONE ctermfg=130   ctermbg=NONE
    hi doxygenSpecialOnelineDesc    cterm=NONE ctermfg=130   ctermbg=NONE

    if v:version >= 700
        hi Pmenu          cterm=NONE   ctermfg=255   ctermbg=60
        hi PmenuSel       cterm=BOLD   ctermfg=255   ctermbg=236
        hi PmenuSbar      cterm=BOLD   ctermfg=255   ctermbg=61
        hi PmenuThumb     cterm=BOLD   ctermfg=255   ctermbg=61

        hi SpellBad     gui=undercurl guisp=#cc6666
        hi SpellRare    gui=undercurl guisp=#cc66cc
        hi SpellLocal   gui=undercurl guisp=#cccc66
        hi SpellCap     gui=undercurl guisp=#66cccc

        hi MatchParen   cterm=NONE      ctermfg=238   ctermbg=120
    endif

" vim: set et :
