// Find local, non-loopback IP address
//
// Skips docker bridge address and IPv6 addresses, unless it can't find another
package main

import (
  "flag"
  "fmt"
  "io/ioutil"
  "log"
  "net"
  "net/http"
  "os"
  "sort"
  "strings"
)

const (
  fallbackService = "http://benizi.com/ip?raw=1"
)

func getAddresses(external bool) (addrs []net.Addr) {
  if external {
    addrs = getExternal()
  } else {
    got, err := net.InterfaceAddrs()
    if err != nil {
      os.Exit(1)
    }
    addrs = got
  }
  return
}

func getExternal() []net.Addr {
  res, err := http.Get(fallbackService)
  if err != nil {
    os.Exit(1)
  }
  defer res.Body.Close()

  ip, err := ioutil.ReadAll(res.Body)
  if err != nil {
    os.Exit(1)
  }

  _, network, err := net.ParseCIDR(fmt.Sprintf("%s/32", ip[0:len(ip)-1]))
  if err != nil {
    os.Exit(1)
  }

  return []net.Addr{network}
}

func succeed(addr string) {
  fmt.Println(addr)
  os.Exit(0)
}

type foundAddr struct {
  ip net.IP
  preferred bool
  rejected bool
  loopback bool
  v6 bool
}

func xor(a, b bool) bool {
  return (a && !b) || (!a && b)
}

type ByAttributes struct {
  addrs []foundAddr
  prefer6 bool
}

func (v ByAttributes) Len() int { return len(v.addrs) }
func (v ByAttributes) Swap(i, j int) { v.addrs[i], v.addrs[j] = v.addrs[j], v.addrs[i] }
func (v ByAttributes) Less(i, j int) bool {
  a := v.addrs[i]
  b := v.addrs[j]
  if xor(a.v6, b.v6) {
    return !xor(a.v6, v.prefer6)
  }
  if xor(a.preferred, b.preferred) {
    return a.preferred
  }
  if xor(a.rejected, b.rejected) {
    return !a.rejected
  }
  if xor(a.loopback, b.loopback) {
    return !a.loopback
  }
  return a.ip.String() < b.ip.String()
}

func anyContains(networks []net.IPNet, ip net.IP) bool {
  for _, network := range networks {
    if network.Contains(ip) {
      return true
    }
  }
  return false
}

func parseNetwork(cidr string) (*net.IPNet, error) {
  _, network, err := net.ParseCIDR(cidr)
  if err == nil {
    return network, nil
  }

  // Try parsing it as an octet or octets
  // e.g. "10" => "10.0.0.0/8", "192.168" => "192.168.0.0/16"
  dots := strings.Count(cidr, ".")
  needed := 3 - dots
  if needed < 0 {
    return nil, err
  }
  cidr = fmt.Sprintf("%s%s/%d", cidr, strings.Repeat(".0", needed), 32 - 8 * needed)
  _, network, e := net.ParseCIDR(cidr)
  if e == nil {
    return network, nil
  }

  // return the original error
  return nil, err
}

func main() {
  only6 := flag.Bool("6", false, "Limit to IPv6")
  external := flag.Bool("x", false, "Fetch external address")
  docker := "172.17.0.0/16"
  flag.Parse()

  _, dockerNet, err := net.ParseCIDR(docker)
  if err != nil {
    log.Fatalln("Failed to parse Docker network", docker)
  }

  var acceptable []net.IPNet
  var rejectable []net.IPNet

  for _, arg := range flag.Args() {
    if len(arg) == 0 {
      continue
    }

    addTo := &acceptable
    if arg[0] == '!' || arg[0] == 'x' {
      addTo = &rejectable
      arg = arg[1:len(arg)]
    }

    network, err := parseNetwork(arg)
    if err != nil {
      log.Fatal(err)
    }

    *addTo = append(*addTo, *network)
  }

  if len(rejectable) == 0 {
    rejectable = append(rejectable, *dockerNet)
  }

  found := make([]foundAddr, 0)

  addrs := getAddresses(*external)
  for _, addr := range addrs {
    network, ok := addr.(*net.IPNet)
    if !ok {
      continue
    }

    found = append(found, foundAddr{
      ip: network.IP,
      preferred: anyContains(acceptable, network.IP),
      rejected: anyContains(rejectable, network.IP),
      loopback: network.IP.IsLoopback(),
      v6: network.IP.To4() == nil,
    })
  }

  sort.Sort(ByAttributes{found, *only6})

  if len(found) > 0 {
    succeed(found[0].ip.String())
  }

  os.Exit(1)
}
