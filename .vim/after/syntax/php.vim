if version >= 600
	syn clear phpHereDoc
	syn case match
	syn region  phpHereDoc  matchgroup=Delimiter start="\(<<<\)\@<=\z(\I\i*\)$" end="^\z1\(;\=$\)\@=" contained contains=phpIdentifier,phpIdentifierSimply,phpIdentifierComplex,phpSpecialChar,phpMethodsVar keepend extend fold
" including HTML,JavaScript,SQL even if not enabled via options
	syn region  phpHereDoc  matchgroup=Delimiter start="\(<<<\)\@<=\z(\(\I\i*\)\=\(html\)\c\(\i*\)\)$" end="^\z1\(;\=$\)\@="  contained contains=@htmlTop,phpIdentifier,phpIdentifierSimply,phpIdentifierComplex,phpSpecialChar,phpMethodsVar keepend extend fold
	syn region  phpHereDoc  matchgroup=Delimiter start="\(<<<\)\@<=\z(\(\I\i*\)\=\(sql\)\c\(\i*\)\)$" end="^\z1\(;\=$\)\@=" contained contains=@sqlTop,phpIdentifier,phpIdentifierSimply,phpIdentifierComplex,phpSpecialChar,phpMethodsVar keepend extend fold
	syn region  phpHereDoc  matchgroup=Delimiter start="\(<<<\)\@<=\z(\(\I\i*\)\=\(javascript\)\c\(\i*\)\)$" end="^\z1\(;\=$\)\@="  contained contains=@htmlJavascript,phpIdentifierSimply,phpIdentifier,phpIdentifierComplex,phpSpecialChar,phpMethodsVar keepend extend fold
	syn case ignore
endif
match Error /\/\*\*@#[\-+]/

sil! syn clear phpComment
sil! syn clear phpMultiLineComment
sil! syn clear phpDocComment
sil! syn clear phpDocCommentShared
sil! syn clear phpDocCommentSharedTag

syn region phpDocCommentShared matchgroup=phpDocCommentSharedTag start=~/\*\*#@+\_.\{-}\*/~ end=~/\*\*#@-\*/~ contains=@phpClTop containedin=phpRegion transparent fold

syn region phpDocComment start=+/\*\*\(#@[+\-]\)\@!+ end=+\*/+ contained containedin=phpRegion contains=phpTodo,phpDocTag fold

syn region phpMultiLineComment start=+/\*\*\@!+ end=+\*/+ contained containedin=phpRegion contains=phpTodo fold
syn match phpMultiLineComment +/\*\*/+ contained containedin=phpRegion

syn match phpComment /#.\{-}\(?>\|$\)\@=/ contained contains=phpTodo
syn match phpComment +//.\{-}\(?>\|$\)\@=+ contained contains=phpTodo

syn match phpDocTag /\ \@<=@\w\+/ contained

syn cluster phpClTop add=phpDocComment,phpDocCommentShared,phpMultiLineComment

hi def link phpDocComment Special
hi def link phpMultiLineComment Comment
hi def link phpDocTag Keyword
hi def link phpDocCommentSharedTag Identifier
