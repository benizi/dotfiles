package main

import(
	"bufio"
	"bytes"
	"log"
	"os"
	"os/exec"
	"strings"

	//"golang.org/x/crypto/ssh"
	//sshagent "golang.org/x/crypto/ssh/agent"
)

func check(err error) {
	if err != nil {
		log.Fatal(err)
	}
}

func findIds(host string) []string {
	var files []string
	out, err := exec.Command("ssh", "-G", host).Output()
	check(err)
	scanner := bufio.NewScanner(bytes.NewReader(out))
	idPrefix := "identityfile "
	for scanner.Scan() {
		line := scanner.Text()
		if !strings.HasPrefix(line, idPrefix) {
			continue
		}
		file := line[len(idPrefix):len(line)]
		if file[0] == '~' {
			file = os.Getenv("HOME") + file[1:len(file)]
		}
		files = append(files, file)
	}
	return files
}

func main() {
	var idx = 0
	args := os.Args
	for i, arg := range args {
		if i == 0 {
			continue
		}
		if arg == "--" && idx == 0 {
			idx = i
		}
	}
	if idx < 2 || idx == len(args) - 1 {
		log.Fatalf("Usage: %s host [hosts] -- cmd [args]", args[0])
	}
	hosts, cmd, args := args[1:idx], args[idx+1], args[idx+1:len(args)]
	var idfiles []string
	for _, host := range hosts {
		idfiles = append(idfiles, findIds(host)...)
	}
	log.Println(cmd)
	log.Printf("IDS: %#+v", idfiles)
}
