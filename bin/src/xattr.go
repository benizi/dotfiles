package main

import (
	"bytes"
	"encoding/base64"
	"encoding/hex"
	"flag"
	"fmt"
	"log"
	"strings"
	"syscall"
	"unicode"

	// for syscalls
	"unsafe"
)

/*

sh$ awk /xattr/ /usr/include/asm/unistd_64.h

#define __NR_setxattr 188
#define __NR_lsetxattr 189
#define __NR_fsetxattr 190

#define __NR_getxattr 191
#define __NR_lgetxattr 192
#define __NR_fgetxattr 193

#define __NR_listxattr 194
#define __NR_llistxattr 195
#define __NR_flistxattr 196

#define __NR_removexattr 197
#define __NR_lremovexattr 198
#define __NR_fremovexattr 199

ssize_t listxattr(const char *path, char *list, size_t size);
ssize_t llistxattr(const char *path, char *list, size_t size);
ssize_t flistxattr(int fd, char *list, size_t size);

ssize_t getxattr(const char *path, const char *name, void *value, size_t size);
ssize_t lgetxattr(const char *path, const char *name, void *value, size_t size);
ssize_t fgetxattr(int fd, const char *name, void *value, size_t size);

int setxattr(const char *path, const char *name, const void *value, size_t size,
             int flags);
int lsetxattr(const char *path, const char *name, const void *value,
              size_t size, int flags);
int fsetxattr(int fd, const char *name, const void *value, size_t size,
              int flags);

int removexattr(const char *path, const char *name);
int lremovexattr(const char *path, const char *name);
int fremovexattr(int fd, const char *name);

*/

const (
	sys_setxattr     = 188
	sys_lsetxattr    = 189
	sys_fsetxattr    = 190
	sys_getxattr     = 191
	sys_lgetxattr    = 192
	sys_fgetxattr    = 193
	sys_listxattr    = 194
	sys_llistxattr   = 195
	sys_flistxattr   = 196
	sys_removexattr  = 197
	sys_lremovexattr = 198
	sys_fremovexattr = 199
)

type xattr struct {
	name    string
	value   []byte
	present bool
}

func errnoToError(errno syscall.Errno) error {
	if errno == 0 {
		return nil
	}
	return errno
}

func getstring(buf []byte) (s string, err error) {
	idx := bytes.IndexByte(buf, 0)
	if idx >= 0 {
		s = string(buf[:idx])
	} else {
		err = fmt.Errorf("No NUL-terminated string found")
	}
	return
}

func getstrings(buf []byte) []string {
	var strs []string
	for len(buf) > 0 {
		idx := bytes.IndexByte(buf, 0)
		if idx < 0 {
			break
		}
		strs = append(strs, string(buf[:idx]))
		buf = buf[1+idx:]
	}
	return strs
}

func listxattr(path string, link bool) (names []string, err error) {
	var errno syscall.Errno
	var pathptr *byte
	var r1 uintptr
	trap := sys_listxattr
	if link {
		trap = sys_llistxattr
	}
	pathptr, err = syscall.BytePtrFromString(path)
	if err != nil {
		return
	}
	var sz int
	r1, _, errno = syscall.Syscall(uintptr(trap),
		uintptr(unsafe.Pointer(pathptr)), uintptr(0), uintptr(0))
	sz = int(r1)
	err = errnoToError(errno)
	if sz == 0 || err != nil {
		return
	}
	buf := make([]byte, sz)
	r1, _, errno = syscall.Syscall(uintptr(trap),
		uintptr(unsafe.Pointer(pathptr)),
		uintptr(unsafe.Pointer(&buf[0])),
		uintptr(sz))
	err = errnoToError(errno)
	if err != nil {
		return
	}
	if int(r1) != sz {
		err = fmt.Errorf("List of xattrs changed between calls: expected %d, got %d", sz, r1)
		return
	}
	names = getstrings(buf)
	return
}

func getxattr(path, name string, link bool) (attr xattr, err error) {
	var errno syscall.Errno
	var pathptr, nameptr *byte
	var r1 uintptr
	trap := sys_getxattr
	if link {
		trap = sys_lgetxattr
	}
	pathptr, err = syscall.BytePtrFromString(path)
	if err != nil {
		return
	}
	nameptr, err = syscall.BytePtrFromString(name)
	if err != nil {
		return
	}
	var sz int
	r1, _, errno = syscall.Syscall6(uintptr(trap),
		uintptr(unsafe.Pointer(pathptr)),
		uintptr(unsafe.Pointer(nameptr)),
		uintptr(0),
		uintptr(0),
		uintptr(0), uintptr(0))
	sz = int(r1)
	err = errnoToError(errno)
	if err != nil {
		return
	}
	if sz == 0 {
		attr.name = name
		attr.present = true
		return
	}
	buf := make([]byte, sz)
	r1, _, errno = syscall.Syscall6(uintptr(trap),
		uintptr(unsafe.Pointer(pathptr)),
		uintptr(unsafe.Pointer(nameptr)),
		uintptr(unsafe.Pointer(&buf[0])),
		uintptr(sz),
		uintptr(0), uintptr(0))
	err = errnoToError(errno)
	if err != nil {
		return
	}
	if int(r1) != sz {
		err = fmt.Errorf("xattr changed between calls?: expected %d, got %d", sz, r1)
		return
	}
	attr.name = name
	attr.value = buf
	attr.present = true
	return
}

func encode(val []byte, style format) string {
	switch style {
	case f_text:
		return string(val)
	case f_hex:
		return hex.EncodeToString(val)
	case f_base64:
		return base64.StdEncoding.EncodeToString(val)
	}
	return ""
}

type optionalString struct {
	value    string
	provided bool
}

// flag.Value member
func (o *optionalString) String() string {
	switch o.value {
	case "\t":
		return "<Tab>"
	case "\n":
		return "<LF>"
	}
	return o.value
}

// flag.Value member
func (o *optionalString) Set(val string) error {
	o.value = val
	o.provided = true
	return nil
}

func ostring(defaultValue string) optionalString {
	return optionalString{value: defaultValue}
}

type sections struct {
	header, file, blank, name, value bool
}

func defaultSections() sections {
	return sections{
		header: false,
		file:   false,
		name:   true,
		value:  false,
		blank:  false,
	}
}

// flag.Value member
func (s *sections) String() string {
	sectionchar := func(set bool, s string) string {
		if set {
			return strings.ToUpper(s)
		}
		return s
	}
	return strings.Join([]string{
		sectionchar(s.header, "h"),
		sectionchar(s.file, "f"),
		sectionchar(s.name, "n"),
		sectionchar(s.value, "v"),
		sectionchar(s.blank, "b"),
	}, "")
}

// flag.Value member
func (s *sections) Set(val string) error {
	for _, c := range "hfnvb" + val {
		set := unicode.IsUpper(c)
		switch c {
		case 'H', 'h':
			s.header = set
		case 'F', 'f':
			s.file = set
		case 'N', 'n':
			s.name = set
		case 'V', 'v':
			s.value = set
		case 'B', 'b':
			s.blank = set
		default:
			return fmt.Errorf("Invalid section %c in 'show' value: %s", c, val)
		}
	}
	return nil
}

type format int

const (
	f_text format = iota
	f_hex
	f_base64
)

func parseFormat(name string) (f format, err error) {
	switch name {
	case "text", "string", "raw":
		f = f_text
	case "hex":
		f = f_hex
	case "base64", "b64", "b", "base":
		f = f_base64
	default:
		err = fmt.Errorf("Unrecognized format: %s", name)
	}
	return
}

// for flag.Value
func (f format) String() string {
	switch f {
	case f_text:
		return "text"
	case f_hex:
		return "hex"
	case f_base64:
		return "base64"
	}
	return "invalid"
}

type optionalFormat struct {
	f        format
	provided bool
}

// flag.Value member
func (f *optionalFormat) String() string {
	if !f.provided {
		return ""
	}
	return f.f.String()
}

// flag.Value member
func (f *optionalFormat) Set(name string) error {
	val, err := parseFormat(name)
	if err != nil {
		return err
	}
	f.f = val
	f.provided = true
	return nil
}

func oformat(f format) optionalFormat {
	return optionalFormat{f: f}
}

type formats struct {
	file, name, value format
}

type outflags struct {
	show      sections
	fmts      formats
	sep, term string
}

func defaultOutflags() outflags {
	return outflags{
		show: defaultSections(),
		sep:  "\t",
		term: "\n",
	}
}

type mode int

const (
	m_dump mode = iota
	m_list
	m_get
)

func parseMode(name string) (m mode, err error) {
	switch name {
	case "dump":
		m = m_dump
	case "list":
		m = m_list
	case "get":
		m = m_get
	default:
		err = fmt.Errorf("Unknown mode: %s", name)
	}
	return
}

// for flag.Value
func (m mode) String() string {
	switch m {
	case m_dump:
		return "dump"
	case m_list:
		return "list"
	case m_get:
		return "get"
	}
	return "invalid"
}

type optionalMode struct {
	m        mode
	provided bool
}

// flag.Value member
func (m *optionalMode) Set(name string) error {
	val, err := parseMode(name)
	if err != nil {
		return err
	}
	m.m = val
	m.provided = true
	return nil
}

// flag.Value member
func (m *optionalMode) String() string {
	if !m.provided {
		return ""
	}
	return m.m.String()
}

func main() {
	link := false
	modeflag := optionalMode{}
	all_fmt_flag := oformat(f_hex)
	file_fmt_flag := oformat(f_text)
	name_fmt_flag := oformat(f_text)
	value_fmt_flag := oformat(f_hex)
	sepflag := ostring("\t")
	termflag := ostring("\n")
	nul := false
	show := defaultSections()

	flag.BoolVar(&link, "L", link, "Act on symbolic links (rather than referents)")

	flag.Var(&modeflag, "mode", "Mode of operation")

	flag.Var(&all_fmt_flag, "format", "Output format (for filenames, names, and values)")
	flag.Var(&file_fmt_flag, "format-file", "Output format (for filenames)")
	flag.Var(&file_fmt_flag, "ff", "(Alias for --format-file)")
	flag.Var(&name_fmt_flag, "format-name", "Output format (for names)")
	flag.Var(&name_fmt_flag, "fn", "(Alias for --format-name)")
	flag.Var(&value_fmt_flag, "format-value", "Output format (for values)")
	flag.Var(&value_fmt_flag, "fv", "(Alias for --format-value)")
	flag.Var(&value_fmt_flag, "vals", "(Alias for --format-value)")

	flag.Var(&sepflag, "sep", "Name-value separator")
	flag.Var(&termflag, "term", "Name-value pair terminator")
	flag.BoolVar(&nul, "0", nul, "Use NUL as separator and terminator")
	flag.BoolVar(&nul, "nul", nul, "(Alias for -0)")
	flag.BoolVar(&nul, "null", nul, "(Alias for -0)")
	flag.BoolVar(&nul, "z", nul, "(Alias for -0)")

	flag.BoolVar(&show.header, "showheader", show.header, "Show header line for every file")
	flag.BoolVar(&show.header, "header", show.header, "(Alias for --showheader)")
	flag.BoolVar(&show.header, "H", show.header, "(Alias for --showheader)")
	flag.BoolVar(&show.file, "showfile", show.file, "Show filename on every line")
	flag.BoolVar(&show.file, "filename", show.file, "(Alias for --showfile)")
	flag.BoolVar(&show.file, "F", show.file, "(Alias for --showfile)")
	flag.BoolVar(&show.name, "showname", show.name, "Print xattr name")
	flag.BoolVar(&show.name, "N", show.name, "(Alias for --showname)")
	flag.BoolVar(&show.value, "showval", show.value, "Print xattr value")
	flag.BoolVar(&show.blank, "showblank", show.blank, "Show blank attrs when missing")

	flag.Var(&show, "show", "Select what fields to show")

	flag.Parse()

	nArgs := flag.NArg()
	hasArgs := nArgs > 0
	args := flag.Args()

	cmd := m_dump
	out := defaultOutflags()
	out.show = show

	switch {
	case modeflag.provided:
		cmd = modeflag.m
	case hasArgs:
		val, err := parseMode(args[0])
		if err == nil {
			cmd = val
			args = args[1:]
		}
	}

	out.fmts = formats{
		file:  file_fmt_flag.f,
		name:  name_fmt_flag.f,
		value: value_fmt_flag.f,
	}
	if all_fmt_flag.provided {
		f := all_fmt_flag.f
		out.fmts = formats{file: f, name: f, value: f}
	} else {
		if name_fmt_flag.provided {
			out.fmts.name = name_fmt_flag.f
			out.fmts.file = name_fmt_flag.f
		}
		if file_fmt_flag.provided {
			out.fmts.file = file_fmt_flag.f
		}
		if value_fmt_flag.provided {
			out.fmts.value = value_fmt_flag.f
		}
	}

	switch {
	case sepflag.provided:
		out.sep = sepflag.String()
	case nul:
		out.sep = "\x00"
	}

	switch {
	case termflag.provided:
		out.term = termflag.String()
	case nul:
		out.term = "\x00"
	}

	printline := func(fields ...string) {
		fmt.Printf("%s%s", strings.Join(fields, out.sep), out.term)
	}

	doList := false
	doGet := false

	switch cmd {
	case m_list:
		doList = true
	case m_dump:
		doList = true
		doGet = true
	case m_get:
		doGet = true
	}

	// doesn't make sense to get values, but not show them
	if doGet {
		out.show.value = true
	}

	var paths []string
	if doList {
		if !hasArgs {
			log.Fatalf("Usage: %s filename [filename...]", cmd)
		}
		paths = args
	}
	if doGet && !doList {
		if nArgs < 2 {
			log.Fatalf("Usage: %s attr-name filename [filename...]", cmd)
		}
		paths = args[1:]
	}

	for _, path := range paths {
		encpath := encode([]byte(path), out.fmts.file)
		if out.show.header {
			fields := []string{}
			if out.show.name {
				fields = append(fields, "#")
			}
			fields = append(fields, encpath)
			printline(fields...)
		}

		var names []string
		var attrs []xattr

		if doList {
			var err error
			names, err = listxattr(path, link)
			if err != nil {
				log.Printf("Error trying to list xattrs: %v", err)
				continue
			}
		} else {
			names = []string{args[0]}
		}

		for _, name := range names {
			var attr xattr
			if doGet {
				var err error
				attr, err = getxattr(path, name, link)
				if err != nil {
					if !out.show.blank {
						log.Printf("Error trying to get xattr %s for %s: %v", name, path, err)
						continue
					}
					attr = xattr{name: name}
				}
			} else {
				attr.name = name
			}
			attrs = append(attrs, attr)
		}

		for _, attr := range attrs {
			fields := []string{}
			if out.show.file {
				fields = append(fields, encpath)
			}
			if out.show.name {
				fields = append(fields, encode([]byte(attr.name), out.fmts.name))
			}
			if out.show.value && attr.present {
				fields = append(fields, encode(attr.value, out.fmts.value))
			}
			printline(fields...)
		}
	}
}
