package main

import (
	"bufio"
	"image/jpeg"
	"os"
)

func main() {
	_, err := jpeg.Decode(bufio.NewReader(os.Stdin))
	if err != nil {
		os.Exit(1)
	}
}
