let g:loaded_runproject=1
if exists('g:loaded_runproject')
   finish
endif

function! s:SetupProject()
   if !exists("s:project_command")
      let s:project_command = &ft." %:p"
   endif
   let s:project_command = input("Project command: ", s:project_command)
   let &l:makeprg = s:project_command
   "join(split(substitute(s:project_command,'[ &:%=\\]','\\&','g'),'\\\@<!|'),'\\\\\\|')
endfunction

function! s:ResetProject()
   unlet! s:project_command
   call s:SetupProject()
endfunction

function! s:RunProject(...)
   if !exists("s:project_command") || (a:0==1 && a:1=='alter')
      call s:SetupProject()
   endif
   return ':!'.s:project_command."\n"
endfunction

map <F10> :call <SID>ResetProject()<CR>
map <expr> <F11> <SID>RunProject()
map <expr> <F12> <SID>RunProject('alter')
