// Find local, non-loopback IP address
//
// Skips docker bridge address and IPv6 addresses, unless it can't find another
package main

import (
  "flag"
  "fmt"
  "io/ioutil"
  "net"
  "net/http"
  "os"
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

func main() {
  only6 := flag.Bool("6", false, "Limit to IPv6")
  external := flag.Bool("x", false, "Fetch external address")
  flag.Parse()

  addrs := getAddresses(*external)
  for _, addr := range addrs {
    network, ok := addr.(*net.IPNet)
    if !ok || network.IP.IsLoopback() {
      continue
    }

    // skip IPv6 unless we want it
    is6 := network.IP.To4() == nil
    if (!*only6 && is6) || (*only6 && !is6) {
      continue
    }

    // skip Docker address
    if network.Contains(net.IPv4(172, 17, 0, 1)) {
      continue
    }

    succeed(network.IP.String())
  }

  // print the IPv6 or Docker address if it's the only one
  if len(addrs) > 0 {
    succeed(addrs[0].String())
  }

  os.Exit(1)
}
