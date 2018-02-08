package main

import (
	"fmt"
	"net"
	"os"
	"syscall"
)

func main() {
	var short bool
	var hosts []string
	for _, arg := range os.Args[1:] {
		switch arg {
		case "+short":
			short = true
		default:
			hosts = append(hosts, arg)
		}
	}
	for _, host := range hosts {
		ips, err := net.LookupIP(host)
		if err != nil {
			if !short {
				fmt.Fprintln(os.Stderr, err)
			}
			continue
		}
		fmt.Println(ips[0].String())
		syscall.Exit(0)
	}
	syscall.Exit(1)
}
