package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"syscall"
)

// Print a usage message to the specified writer
func printUsage(w io.Writer) {
	fmt.Fprintf(w, "Usage: %v cmd [args]\n", os.Args[0])
}

func main() {
	// Require at least one cmd arg
	if len(os.Args) < 2 {
		printUsage(os.Stderr)
		os.Exit(1)
	}

	// Drop the program name
	args := os.Args[1:]

	// Check for `-h`/`--help`/`help`
	switch args[0] {
	case "-h", "--help", "help":
		printUsage(os.Stdout)
		return
	}

	// Tee stdin to stdout, but also create a reader the external cmd can use
	r := io.TeeReader(os.Stdin, os.Stdout)

	// When running the command, read from the tee, and pipe all output to stderr
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = r
	cmd.Stdout = os.Stderr
	cmd.Stderr = os.Stderr

	// Try to run it
	if err := cmd.Start(); err != nil {
		log.Fatalf("Error running cmd [%v]: %v", cmd, err)
	}

	// Wait for it to finish, exiting with an error if the cmd did
	if err := cmd.Wait(); err != nil {
		if exiterr, ok := err.(*exec.ExitError); ok {
			if status, ok := exiterr.Sys().(syscall.WaitStatus); ok {
				os.Exit(status.ExitStatus())
			}
		}
	}
}
