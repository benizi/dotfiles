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

type foundAddr struct {
  ip net.IP
  preferred bool
  rejected bool
  loopback bool
  isRfc1918 bool
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
    return a.v6 == v.prefer6
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
  if xor(a.isRfc1918, b.isRfc1918) {
    return !a.isRfc1918
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
  only6 := false
  external := false
  excludeDocker := true
  docker := "172.17.0.0/16"
  printAll := false

  flag.BoolVar(&only6, "6", only6, "Limit to IPv6")
  flag.BoolVar(&external, "x", external, "Fetch external address")
  flag.BoolVar(&excludeDocker, "nodocker", excludeDocker, "Exclude Docker interface")
  flag.StringVar(&docker, "dockernet", docker, "Docker network to exclude")
  flag.BoolVar(&printAll, "all", printAll, "Print all addresses")
  flag.Parse()

  var acceptable []net.IPNet
  var rejectable []net.IPNet
  rfc1918 := []net.IPNet{}

  for _, cidr := range []string{"10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16"} {
    _, parsed, err := net.ParseCIDR(cidr)
    if err != nil {
      log.Fatalln("Failed to parse RFC 1918 network", cidr)
    }
    rfc1918 = append(rfc1918, *parsed)
  }

  if excludeDocker {
    _, dockerNet, err := net.ParseCIDR(docker)
    if err != nil {
      log.Fatalln("Failed to parse Docker network", docker)
    }

    rejectable = append(rejectable, *dockerNet)
  }

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

  found := make([]foundAddr, 0)

  addrs := getAddresses(external)
  for _, addr := range addrs {
    network, ok := addr.(*net.IPNet)
    if !ok {
      continue
    }

    found = append(found, foundAddr{
      ip: network.IP,
      preferred: anyContains(acceptable, network.IP),
      rejected: anyContains(rejectable, network.IP),
      isRfc1918: anyContains(rfc1918, network.IP),
      loopback: network.IP.IsLoopback(),
      v6: network.IP.To4() == nil,
    })
  }

  if len(found) == 0 {
    os.Exit(1)
  }

  sort.Sort(ByAttributes{found, only6})

  for _, addr := range found {
    fmt.Println(addr.ip.String())
    if !printAll {
      break
    }
  }
}
