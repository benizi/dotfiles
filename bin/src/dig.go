package main

import (
	"fmt"
	"net"
	"os"
	"syscall"
)

func main() {
	for _, arg := range os.Args {
		if arg == "+short" {
			continue
		}
		ips, err := net.LookupIP(arg)
		if err != nil {
			continue
		}
		fmt.Println(ips[0].String())
		syscall.Exit(0)
	}
	syscall.Exit(1)
}
