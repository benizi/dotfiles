package main

import (
	"fmt"
	"net"
	"os"
	"strings"
	"syscall"

	"github.com/miekg/dns"
)

// A resolver consists of a "spec" (as the server was specified) and an "addr"
// to which the server was resolved.  defaultport is only used for printing the
// resolver back out (port is omitted if it was defaulted).
type resolver struct {
	spec, addr  string
	defaultport bool
}

// String prints the resolver's specification, along with what it resolved to,
// if it differs from the spec.
func (r resolver) String() string {
	resolved := ""
	host, port, err := net.SplitHostPort(r.addr)
	if err != nil {
		resolved = "???"
	} else if r.spec != host {
		if r.defaultport {
			resolved = host
		} else {
			resolved = net.JoinHostPort(host, port)
		}
	}
	if resolved == r.spec {
		resolved = ""
	}
	if resolved != "" {
		resolved = fmt.Sprintf("=(%s)", resolved)
	}
	return r.spec + resolved
}

// newResolver returns a server spec and its locally-resolved address.
func newResolver(server string) (resolver, error) {
	r := resolver{spec: server}
	host, port, err := net.SplitHostPort(server)
	if err != nil {
		host = server
		port = "53"
		r.defaultport = true
	}
	if port == "domain" {
		r.defaultport = true
	}
	if net.ParseIP(host) == nil {
		ips, err := net.LookupIP(host)
		if err != nil {
			return r, err
		}
		host = ips[0].String()
	}
	r.addr = net.JoinHostPort(host, port)
	_, _, err = net.SplitHostPort(r.addr)
	return r, err
}

// resolve a host via the given server
func query(host string, server resolver) ([]net.IP, error) {
	fqhost := dns.Fqdn(host)
	client := dns.Client{}
	m := dns.Msg{}
	m.SetQuestion(fqhost, dns.TypeA)
	res, _, err := client.Exchange(&m, server.addr)
	if err == nil && (res == nil || res.Answer == nil) {
		err = fmt.Errorf("No results for [%s] from [%s]", fqhost, server)
	}
	if err != nil {
		return nil, err
	}
	var addrs []net.IP
	for _, answer := range res.Answer {
		if record, valid := answer.(*dns.A); valid {
			addrs = append(addrs, record.A)
		}
	}
	return addrs, nil
}

// Fetch IPs for the given host. If servers is empty, resolve it locally.
func lookupIPs(host string, servers []resolver) ([]net.IP, error) {
	if servers == nil {
		return net.LookupIP(host)
	}
	var addrs []net.IP
	var failures []error
	addFailure := func(err error) {
		if err != nil {
			failures = append(failures, err)
		}
	}
	for _, server := range servers {
		ips, err := query(host, server)
		addrs = append(addrs, ips...)
		addFailure(err)
	}
	if addrs == nil && failures == nil {
		addFailure(fmt.Errorf("No results for [%s] from [%v]", host, servers))
	}
	var err error
	if failures != nil {
		var lines []string
		for _, failure := range failures {
			lines = append(lines, fmt.Sprintf("%s", failure))
		}
		err = fmt.Errorf("%s", strings.Join(lines, "\n"))
	}
	return addrs, err
}

func filterByVersion(ips []net.IP, ipv4OK, ipv6OK bool) ([]net.IP, error) {
	filtered := []net.IP{}
	var err error
	if len(ips) != 0 {
		for _, ip := range ips {
			if (ip.To4() != nil && ipv4OK) || (ip.To16() != nil && ipv6OK) {
				filtered = append(filtered, ip)
			}
		}
		if len(filtered) == 0 {
			wanted := 4
			if ipv6OK {
				wanted = 6
			}
			err = fmt.Errorf("No IPv%d addresses found", wanted)
		}
	}
	return filtered, err
}

func preferIPv4(ips []net.IP) []net.IP {
	tot := len(ips)
	ret := make([]net.IP, tot)
	off, lastsix := 0, tot
	for i, ip := range ips {
		if ip.To4() != nil {
			ret[off] = ip
			off += 1
		} else {
			lastsix = tot - 1 - i + off
			ret[lastsix] = ip
		}
	}
	for {
		mirr := (tot - 1) - (off - lastsix)
		if off >= mirr || off >= tot {
			break
		}
		ret[off], ret[mirr] = ret[mirr], ret[off]
		off += 1
	}
	return ret
}

func main() {
	var short bool
	var hosts []string
	var servers []resolver
	var anyOK bool
	var ipv4OK, ipv6OK, ipvSpecified bool
	for _, arg := range os.Args[1:] {
		switch arg {
		case "-4":
			ipvSpecified = true
			ipv4OK = true
		case "-6":
			ipvSpecified = true
			ipv6OK = true
		case "+short":
			short = true
		default:
			if strings.HasPrefix(arg, "@") {
				server, err := newResolver(arg[1:])
				if err != nil {
					fmt.Fprintf(os.Stderr, "Error resolving server: %s\n", err)
					goto failed
				}
				servers = append(servers, server)
			} else {
				hosts = append(hosts, arg)
			}
		}
	}
hosts:
	for _, host := range hosts {
		ips, err := lookupIPs(host, servers)
		ips = preferIPv4(ips)
		if err == nil && ipvSpecified {
			ips, err = filterByVersion(ips, ipv4OK, ipv6OK)
		}
		if err != nil {
			if !short {
				fmt.Fprintln(os.Stderr, err)
			}
			continue
		}
		anyOK = true
		for _, ip := range ips {
			fmt.Println(ip.String())
			if short {
				break hosts
			}
		}
	}
	if anyOK {
		syscall.Exit(0)
	}
failed:
	syscall.Exit(1)
}
