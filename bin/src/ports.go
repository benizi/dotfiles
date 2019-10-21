package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"os/exec"
	"sort"
	"strconv"
	"strings"
)

type jsonobj map[string]interface{}

func (j jsonobj) get(ks ...string) (interface{}, bool) {
	o := j
	for i, k := range ks {
		v, found := o[k]
		if !found {
			return o, false
		}
		if i == len(ks)-1 {
			return v, true
		}
		jv, ok := v.(jsonobj)
		if !ok {
			return v, false
		}
		o = jv
	}
	return o, false
}

func (j jsonobj) getint(ks ...string) (int, bool) {
	v, found := j.get(ks...)
	if !found {
		return 0, false
	}
	i, ok := v.(int)
	return i, ok
}

func (j jsonobj) getstring(ks ...string) (string, bool) {
	v, found := j.get(ks...)
	if !found {
		return "", false
	}
	s, ok := v.(string)
	return s, ok
}

type files []jsonobj

func (fs files) sort() {
	sort.Slice(fs, func(i, j int) bool {
		// Sort by port number
		pi, foundi := fs[i].getint("file", "port")
		pj, foundj := fs[j].getint("file", "port")
		if foundi && foundj {
			return pi < pj
		}

		// Then by bind address
		hi, foundi := fs[i].getstring("file", "host")
		hj, foundj := fs[j].getstring("file", "host")
		if foundi && foundj {
			return hi < hj
		}

		return false
	})
}

func lsof() (files, error) {
	args := strings.Split("lsof -Pni -sTCP:LISTEN -F0", " ")
	out, err := exec.Command("sudo", args...).Output()
	if err != nil {
		return nil, err
	}
	ret := []jsonobj{}
	var process jsonobj
	for _, line := range strings.Split(string(out), "\n") {
		if line == "" {
			continue
		}

		file := jsonobj{}
		tcptpi := jsonobj{}
		add := false
		var target *jsonobj
		switch line[0] {
		case 'p':
			process = jsonobj{}
			target = &process
		case 'f':
			target = &file
			add = true
		default:
			return nil, fmt.Errorf("Unparsed line: [%s]", line)
		}

		for _, kv := range strings.Split(line, "\x00") {
			if kv == "" {
				continue
			}

			k, v := kv[0:1], kv[1:]

			switch k {
			case "T":
				tcptpi[v[0:2]] = v[3:]
				continue
			default:
				(*target)[k] = kv[1:]
			}

			switch k {
			case "n":
				if host, portstring, err := net.SplitHostPort(v); err == nil {
					file["host"] = host
					if port, err := strconv.Atoi(portstring); err == nil {
						file["port"] = port
					}
				}
			}
		}

		if add {
			ret = append(ret, jsonobj{
				"process": process,
				"file":    file,
				"tcptpi":  tcptpi,
			})
		}
	}
	return ret, nil
}

func main() {
	printJson := false

	flag.BoolVar(&printJson, "json", printJson, "Output JSON-encoded text")

	flag.Parse()

	files, err := lsof()
	if err != nil {
		log.Fatal(err)
	}
	files.sort()

	if printJson {
		enc := json.NewEncoder(os.Stdout)
		for _, f := range files {
			enc.Encode(f)
		}
		return
	}

	rows := [][]string{}
	fieldLengths := []int{}
	for _, f := range files {
		row := []string{}
		for _, path := range []string{"process.c", "file.port", "file.host"} {
			v, ok := f.get(strings.Split(path, ".")...)
			if !ok {
				v = fmt.Sprintf("(%s)", path)
			}
			s := fmt.Sprintf("%v", v)
			row = append(row, s)
			l := len(s)
			if len(fieldLengths) >= len(row) {
				if fieldLengths[len(row)-1] < l {
					fieldLengths[len(row)-1] = l
				}
			} else {
				fieldLengths = append(fieldLengths, l)
			}
		}
		rows = append(rows, row)
	}
	for _, row := range rows {
		for i, f := range row {
			if i > 0 {
				fmt.Printf(" ")
			}
			fmt.Printf("%*s", -fieldLengths[i], f)
		}
		fmt.Printf("\n")
	}
}
