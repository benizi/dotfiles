package main

import (
	"flag"
	"fmt"
	"log"
	"time"

	// Currently needs local fork for default timeout value to work
	"github.com/wirepair/gcd"
)

func main() {
	// Flag defaults
	timeout := time.Millisecond * 100
	host := "localhost"
	port := 9222
	url := ""
	refer := ""

	// Parse commandline
	flag.DurationVar(&timeout, "timeout", timeout, "Default time to wait for a connection")
	flag.StringVar(&host, "host", host, "Remote debugger host")
	flag.IntVar(&port, "port", port, "Remote debugger port")
	flag.StringVar(&url, "url", url, "URL to load in a new tab")
	flag.StringVar(&refer, "refer", refer, "URL to show as Referer [sic] header")
	flag.Parse()

	// Set up variables
	urls := []string{}
	if url != "" {
		urls = append(urls, url)
	}
	urls = append(urls, flag.Args()...)
	instancePort := fmt.Sprintf("%d", port)

	// Connect to debugger
	dbg := gcd.NewChromeDebugger()
	dbg.SetTimeout(timeout)
	dbg.ConnectToInstance(host, instancePort)

	// Open tabs for each URL (if any)
	for _, url := range urls {
		target, err := dbg.NewTab()
		if err != nil {
			log.Fatalln(err.Error())
		}
		if url == "" {
			continue
		}
		target.Page.Enable()
		target.Page.Navigate(url, refer)
	}
}
