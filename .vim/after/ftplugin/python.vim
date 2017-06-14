let python_highlight_all = 1

" Arch suffixes its Python 2 binaries with '2'
if executable('yapf2')
  Glaive codefmt yapf_executable='yapf2'
end
