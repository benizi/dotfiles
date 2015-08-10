(define fkey
  (lambda (fk outkey)
    (xbindkey fk (string-append "xdotool keyup " fk " key " outkey))))

(fkey "F13" "ctrl+z")
(fkey "F14" "E")
(fkey "F15" "p")
(fkey "F16" "plus")
(fkey "F17" "minus")
