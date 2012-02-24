fun! service#Exists(svc)
	let fn = '/etc/init.d/'.a:svc
	return filereadable(fn) ? fn : ''
endfun

fun! service#Do(svc, action)
	let fn = service#Exists(a:svc)
	if ! strlen(fn)
		return 0
	end
	if executable('service') > 0
		exe ':!service' a:svc a:action
	else
		exe ':!'.fn a:action
	endif
endfun
