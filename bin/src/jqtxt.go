package main

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"syscall"
)

func pipe(data []byte) {
	var args []string
	if len(os.Args) > 1 {
		args = os.Args[1:]
	} else {
		args = []string{"."}
	}

	cmd := exec.Command("jq", args...)
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
	pipe(data)
	return

dump:
	os.Stdout.Write(data)
}
