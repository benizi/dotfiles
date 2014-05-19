package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
)

func main() {
	var port int
	defaultPort := 8080

	dir := "."

	flag.IntVar(&port, "p", defaultPort, "listening port")
	flag.IntVar(&port, "port", defaultPort, "listening port")

	flag.Parse()

	if flag.NArg() > 0 {
		dir = flag.Arg(0)
	}

	addr := fmt.Sprintf(":%d", port)
	handler := http.FileServer(http.Dir(dir))
	log.Fatal(http.ListenAndServe(addr, handler))
}
