package main

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
	"syscall"
)

func pipejq(data []byte, cmd string, custom bool, args ...string) {
	if len(args) == 0 && !custom {
		args = []string{"."}
	}
	stdincmd(data, cmd, args...)
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
	cmdarg := "--cmd"
	cmdargval := cmdarg + "="
	jq := "jq"
	setjq := false
	objectarg := "--object"
	onlyobject := false

	args := os.Args[1:]
	for i := 0; i < len(args); i++ {
		arg := args[i]
		switch {
		// Accept --object before any `--`
		case arg == objectarg && txtargs == nil:
			onlyobject = true
		// Accept --cmd=cmd
		case !setjq && strings.HasPrefix(arg, cmdargval):
			jq = strings.TrimPrefix(arg, cmdargval)
			setjq = true
		// Accept --cmd cmd if an arg is available
		case !setjq && arg == cmdarg && i < len(args)-1:
			jq = args[i+1]
			setjq = true
			i++
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
	var any bool
	for decoder.More() {
		var obj interface{}
		err := decoder.Decode(&obj)
		if err != nil {
			goto dump
		}
		_, object := obj.(map[string]interface{})
		switch {
		case !onlyobject || (object && !any):
			// ok
		default:
			goto dump
		}
		any = true
	}
	pipejq(data, jq, setjq, jqargs...)
	return

dump:
	switch {
	case len(txtargs) > 0:
		stdincmd(data, txtargs[0], txtargs[1:]...)
	default:
		os.Stdout.Write(data)
	}
}
