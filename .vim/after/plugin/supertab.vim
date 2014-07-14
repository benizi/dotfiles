let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabContextDefaultCompletionType = '<c-n>'
let g:SuperTabCompletionContexts = ['s:ContextMarkup']
" reset default completion type after leaving 'insert' mode
let g:SuperTabRetainCompletionType = 2
inoremap <C-t> <C-v><Tab>
