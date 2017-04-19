package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"strings"
	"syscall"
)

func readProc(pid, sub string) (string, error) {
	file := fmt.Sprintf("/proc/%s/%s", pid, sub)
	bytes, err := ioutil.ReadFile(file)
	if err != nil {
		return "", err
	}
	return string(bytes), nil
}

func main() {
	recurse := false
	flag.BoolVar(&recurse, "recurse", recurse, "Keep finding parent pid til 0")
	flag.BoolVar(&recurse, "r", recurse, "== -recurse")
	flag.Parse()

	pid := fmt.Sprintf("%d", syscall.Getpid())
	skip := 1 // skip calling process's PID
	for {
		stat, err := readProc(pid, "stat")
		if err != nil {
			break
		}
		cmdline, err := readProc(pid, "cmdline")
		if err != nil {
			break
		}
		args := strings.SplitN(cmdline, "\000", -1)
		if len(args) > 2 && (strings.HasSuffix(args[0], "/go") || args[0] == "go") && args[1] == "run" {
			skip++
		}
		parts := strings.SplitAfter(string(stat), ") ")
		if len(parts) < 2 {
			break
		}
		parts = strings.SplitN(parts[1], " ", 3)
		if len(parts) < 3 {
			break
		}
		pid = parts[1]
		if pid == "0" {
			break
		}
		skip--
		if skip >= 0 {
			continue
		}
		fmt.Printf("%s\n", pid)
		if !recurse {
			break
		}
	}
}
