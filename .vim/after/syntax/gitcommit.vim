let s:comment = strpart(system('git config core.commentChar'), 0, 1)
sil! if strlen(s:comment)
  syn clear gitcommitComment
  exe 'syn match gitcommitComment /^'.s:comment.'.*/'
end
