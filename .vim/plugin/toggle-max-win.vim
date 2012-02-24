" from: http://vim.wikia.com/wiki/Window_zooming_convenience
fun! ToggleMaxWins()
   let g:window_max = 1 - get(g:, 'window_max', 0)
   if g:window_max
      aug ToggleMaxWins
         au! WinEnter * wincmd _
      aug END
      wincmd _
   else
      au! ToggleMaxWins
      wincmd =
   endif
endfun
map \] :call ToggleMaxWins()<CR>
