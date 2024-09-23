package main

// Annoyed that `sed -i [ext] [cmds]` vs `sed -i[ext] [cmds]` is non-portable
// So, move the `-i` out of the `sed`
// Example usage: `-i path/to/whatever.txt sed -e /badline/d`
// Keeps the original if the cmd exits non-zero

import (
	"bytes"
	"io"
	"log"
	"os"
	"os/exec"
	"strings"
)

func main() {
	args := os.Args[1:]
	file := ""
	ext := ".-i"
	keep := os.Getenv("KEEP") != ""
	dbg := os.Getenv("DEBUG") != ""

args:
	for len(args) > 0 {
		arg := args[0]
		switch {
		case strings.HasPrefix(arg, "."):
			ext = arg
			keep = true
		case arg == "-k" || arg == "--keep":
			keep = true
		case file == "":
			file = arg
		default:
			break args
		}
		args = args[1:]
	}

	if file == "" || len(args) == 0 {
		log.Fatalf("Usage: %s file cmd [args]", os.Args[0])
	}
	bakfile := file + ext

	if dbg {
		log.Printf("file: %v", file)
		log.Printf("bakfile: %v", bakfile)
		log.Printf("CMD: %#+v", args)
	}

	cmd := exec.Command(args[0], args[1:]...)
	data, err := os.ReadFile(file)
	if err != nil {
		log.Fatalf("Failed to read %v: %v", file, err)
	}
	stdin := bytes.NewReader(data)
	var stdout bytes.Buffer
	cmd.Stdin = stdin
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	if err != nil {
		log.Fatalf("Failed to run: %v", err)
	}
	bak, err := os.Create(bakfile)
	if err != nil {
		log.Fatalf("Failed to create backup: %v", err)
	}
	io.Copy(bak, bytes.NewReader(data))
	outf, err := os.OpenFile(file, os.O_WRONLY, 0666)
	if err != nil {
		log.Fatalf("Failed to open file %v: %v", file, err)
	}
	_, err = io.Copy(outf, bytes.NewReader(stdout.Bytes()))
	if err != nil {
		log.Fatalf("Failed to write file: %v", err)
	}
	err = outf.Close()
	if err != nil {
		log.Fatalf("Failed to close file: %v", err)
	}
	if !keep {
		err = os.Remove(bakfile)
		if err != nil {
			log.Printf("Failed to remove backup: %v", err)
		}
	}
}
