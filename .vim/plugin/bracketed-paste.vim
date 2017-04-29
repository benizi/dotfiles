" Bail if we're using a Vim that already has bracketed-paste support.
if exists('&t_PS') && strlen(&t_PS)
  finish
end

" minimal bracketed-paste handling
nn <Esc>[200~ :se paste<CR>a
ino <Esc>[200~ <C-\><C-o>:se paste<CR>
nn [201~ :se nopaste<CR>a
