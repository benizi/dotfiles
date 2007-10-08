" mark.vim - from Text::VimColor
" Plugin-ified by benizi

let s:save_cpo = &cpo
set cpo&vim
if exists("markymark")
 finish
endif
let markymark = 1
if !hasmapto('<Plug>CSSify')
 map <unique> <Leader>h <Plug>CSSify
endif
noremap <unique> <script> <Plug>CSSify <SID>CSSit
noremenu <script> Plugin.CSSify <SID>CSSit
noremap <SID>CSSit :call <SID>CSSit()<CR>

function s:CSSit()
 if !strlen(&filetype)
   filetype detect
 endif
 syn on
 let s:tabstop = &tabstop
 let s:old_title = &title
 let s:old_icon = &icon
 let s:old_et = &l:et
 let s:old_report = &report
 set notitle noicon
 setlocal et
 set report=1000000

 if expand("%") == ""
  new untitled.html
 else
  new %.html
 endif
 set modifiable
 let s:old_paste = &paste
 set paste
 set isprint+=9
 %d
 exe "normal a\<div class=\"code\"\>\n\e"
 wincmd p
 for s:lnum in range(1,line("$"))
  exe s:lnum."retab!"
  let s:line = getline(s:lnum)
  let s:len = strlen(s:line)
  let s:new = ""
  let s:col = 1
  while s:col <= s:len
   let s:startcol = s:col " The start column for processing text
   let s:id = synID(s:lnum, s:col, 1)
   let s:col = s:col + 1
   while s:col <= s:len && s:id == synID(s:lnum, s:col, 1) | let s:col = s:col + 1 | endwhile
   let s:id = synIDtrans(s:id)
   let s:name = synIDattr(s:id, 'name')
   let s:new = s:new . '<span class="' . s:name . '">' . substitute(substitute(substitute(strpart(s:line, s:startcol - 1, s:col - s:startcol), '&', '\&amp;', 'g'), '<', '\&lt;', 'g'), '>', '\&gt;', 'g') . '</span>'
   if s:col > s:len
    break
   endif
  endwhile
  exe "normal \<C-W>pa" . strtrans(s:new) . "\n\e\<C-W>p"
  let s:lnum = s:lnum + 1
  undo
  +
 endfor
 wincmd p
 exe "normal a\</div\>\n\e"
 try
  exe "normal :%s:\\s\\+$::e\n"
 catch /E486:/
 endtry
 normal dd
 wincmd p
 let &title = s:old_title
 let &icon = s:old_icon
 let &l:et = s:old_et
 let &report = s:old_report
 wincmd p
 let &paste = s:old_paste
endfunction

if !exists(":CSSify")
 command -nargs=0 CSSify :call s:CSSit()
endif
