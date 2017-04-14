package main

import (
	"flag"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

func main() {
	var template, base string
	placeholder := "__tmpdir__"
	flag.StringVar(&template, "template", template, "Template")
	flag.StringVar(&template, "t", template, "")
	flag.StringVar(&base, "base", base, "Base directory")
	flag.StringVar(&placeholder, "placeholder", placeholder,
		"Command string to replace with directory name")
	flag.StringVar(&placeholder, "I", placeholder, "")
	flag.Parse()

	tmpdir, err := ioutil.TempDir(base, template)
	if err != nil {
		log.Fatal(err)
	}
	defer os.RemoveAll(tmpdir)

	var args []string
	for _, arg := range flag.Args() {
		args = append(args, strings.Replace(arg, placeholder, tmpdir, -1))
	}
	if len(args) == 0 {
		log.Fatal("No cmd specified")
	}
	if err := exec.Command(args[0], args[1:]...).Run(); err != nil {
		if exiterr, ok := err.(*exec.ExitError); ok {
			if status, ok := exiterr.Sys().(syscall.WaitStatus); ok {
				os.Exit(status.ExitStatus())
			}
		}
		log.Fatal(err)
	}
}
