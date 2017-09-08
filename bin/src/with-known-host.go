package main

import (
	"bytes"
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"syscall"
)

const SYS_SETNS = 308

func showns(pid int, kind string) {
	file := fmt.Sprintf("/proc/%d/ns/%s", pid, kind)
	cmd := exec.Command("sudo", "readlink", file)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Run()
	line := out.String()
	line = line[0:len(line)-1]
	fmt.Printf("%s %d\n", line, pid)
}

func main() {
	var otherpid int
	nstype := "uts"
	var nsfile string

	flag.IntVar(&otherpid, "p", otherpid, "Other PID")
	flag.StringVar(&nstype, "t", nstype, "Type of namespace")
	flag.StringVar(&nsfile, "f", nsfile, "File linked from namespace")
	flag.Parse()

	if nsfile == "" {
		nsfile = fmt.Sprintf("/proc/%d/ns/%s", otherpid, nstype)
	}

	ns, err := os.Open(nsfile)
	if err != nil {
		log.Fatal(err)
	}
	log.Printf("Opened %s", nsfile)
	r1, r2, err := syscall.Syscall(SYS_SETNS, ns.Fd(), 0, 0)
	log.Printf("r1=%v r2=%v err=%v\n", r1, r2, err)
	showns(os.Getpid(), nstype)
	showns(otherpid, nstype)
	showns(1, nstype)
}
