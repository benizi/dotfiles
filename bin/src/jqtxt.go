package main

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"syscall"
)

func pipejq(data []byte, args ...string) {
	if len(args) == 0 {
		args = []string{"."}
	}
	stdincmd(data, "jq", args...)
}

func stdincmd(data []byte, arg0 string, args ...string) {
	cmd := exec.Command(arg0, args...)
	cmd.Stdin = bytes.NewReader(data)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	if err := cmd.Start(); err != nil {
		panic(err)
	}
	if err := cmd.Wait(); err != nil {
		if exiterr, ok := err.(*exec.ExitError); ok {
			if status, ok := exiterr.Sys().(syscall.WaitStatus); ok {
				os.Exit(status.ExitStatus())
			}
		}
	}
}

func main() {
	var jqargs, txtargs []string

	for _, arg := range os.Args[1:] {
		switch {
		// `--` indicates the end of JQ args
		case arg == "--" && txtargs == nil:
			txtargs = []string{}
		// If no `--` yet, add to JQ args
		case txtargs == nil:
			jqargs = append(jqargs, arg)
		// Otherwise, add to text args
		default:
			txtargs = append(txtargs, arg)
		}
	}

	data, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		panic(err)
	}
	decoder := json.NewDecoder(bytes.NewReader(data))
	for decoder.More() {
		var obj interface{}
		err := decoder.Decode(&obj)
		if err != nil {
			goto dump
		}
	}
	pipejq(data, jqargs...)
	return

dump:
	switch {
	case len(txtargs) > 0:
		stdincmd(data, txtargs[0], txtargs[1:]...)
	default:
		os.Stdout.Write(data)
	}
}
