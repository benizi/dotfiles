let s:fg = {}
let s:bg = {}
let s:i = 1
while s:i <= 8
	let s:fg[s:i] = (30+s:i)."m"
	let s:bg[s:i] = (40+s:i)."m"
	let s:i += 1
endwhile
while s:i <= 255
	let s:fg[s:i] = "38;5;".s:i."m"
	let s:bg[s:i] = "48;5;".s:i."m"
	let s:i += 1
endwhile
for fgbg in ['fg','bg']
	for [n,term] in items(s:{fgbg})
		exe 'syn region '.fgbg.'color'.n.' start=/\e\['.term.'/ end=/\(\e\[0m\)\@=/ contains=ALL'
		exe 'hi '.fgbg.'color'.n.' cterm'.fgbg.'='.n
	endfor
endfor
