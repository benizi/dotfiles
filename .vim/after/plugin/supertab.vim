let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabContextDefaultCompletionType = '<c-n>'
let g:SuperTabCompletionContexts = ['s:ContextMarkup']
" reset default completion type after leaving 'insert' mode
let g:SuperTabRetainCompletionType = 2
inoremap <C-t> <C-v><Tab>
let endwise = ScriptNumber('plugin/endwise.vim')
exe 'imap <script> <CR> '.maparg('<CR>','i').'<SNR>'.endwise.'_DiscretionaryEnd'
