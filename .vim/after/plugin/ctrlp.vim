fun! s:CtrlpNoFilter(...) dict
  return systemlist(printf('find %s -type f -printf %%P\\n', shellescape(getcwd())))
endf

let g:ctrlp_no_filter = {'init': function('s:CtrlpNoFilter')}

cal add(g:ctrlp_ext_vars, {
  \ 'init': 'g:ctrlp_no_filter.init()',
  \ 'accept': 'ctrlp#acceptfile',
  \ 'act_farg': 'dict',
  \ 'lname': 'unfiltered',
  \ 'sname': 'unf',
  \ 'type': 'path',
  \ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

com! -n=? CtrlPNoFilter cal ctrlp#init(s:id, {'dir': <q-args>})
