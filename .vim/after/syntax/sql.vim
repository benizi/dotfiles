" hilite too-long comments as errors
" max length is 60, so 61+ is an error
match Error /\c\<comment\> '\%([^']\|''\)\{61,}'\=/
