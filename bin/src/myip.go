// Find local, non-loopback IP address
//
// Skips docker bridge address and IPv6 addresses, unless it can't find another
package main

import (
  "encoding/json"
  "flag"
  "fmt"
  "io/ioutil"
  "log"
  "net"
  "net/http"
  "os"
  "sort"
  "strings"
  "text/template"
)

const (
  fallbackService = "http://benizi.com/ip?raw=1"
)

type namedAddr struct {
  name string
  ip   net.IP
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

func findIPs(addrs []net.Addr) []net.IP {
  ips := []net.IP{}
  for _, addr := range addrs {
    if network, ok := addr.(*net.IPNet); ok {
      ips = append(ips, network.IP)
    }
  }
  return ips
}

func namedIPs(name string, addrs []net.Addr) []namedAddr {
  named := []namedAddr{}
  for _, ip := range findIPs(addrs) {
    named = append(named, namedAddr{name: name, ip: ip})
  }
  return named
}

func getExternalNamedAddrs() []namedAddr {
  return namedIPs("external", getExternal())
}

func getInterfaceNamedAddrs() []namedAddr {
  namedAddrs := []namedAddr{}
  ifis, err := net.Interfaces()
  if err != nil {
    if true {
      return []namedAddr{{ip: net.IP{}, name: err.Error()}}
    }
    return namedAddrs
  }
  for _, ifi := range ifis {
    addrs, err := ifi.Addrs()
    if err != nil {
      continue
    }
    namedAddrs = append(namedAddrs, namedIPs(ifi.Name, addrs)...)
  }
  return namedAddrs
}

type foundAddr struct {
  IP net.IP
  preferred bool
  rejected bool
  Loopback bool
  isRfc1918 bool
  V6 bool
  original int
  Name string
  Wireless bool
}

var wirelessCache = make(map[string]bool)

func isWirelessInterface(dev string) bool {
  isWireless, cached := wirelessCache[dev]
  if !cached {
    isWireless = isWirelessInterfaceImpl(dev)
    wirelessCache[dev] = isWireless
  }
  return isWireless
}

// Dumb, Linux-specific detection of whether a network interface is wireless
func isWirelessInterfaceImpl(dev string) bool {
  stat, err := os.Stat(fmt.Sprintf("/sys/class/net/%s/wireless", dev))
  return err == nil && stat.Mode().IsDir()
}

func xor(a, b bool) bool {
  return (a && !b) || (!a && b)
}

type ByAttributes struct {
  addrs []foundAddr
}

func (v ByAttributes) Len() int { return len(v.addrs) }
func (v ByAttributes) Swap(i, j int) { v.addrs[i], v.addrs[j] = v.addrs[j], v.addrs[i] }
func (v ByAttributes) Less(i, j int) bool {
  a := v.addrs[i]
  b := v.addrs[j]
  if a.rejected != b.rejected {
    return !a.rejected
  }
  if a.preferred != b.preferred {
    return a.preferred
  }
  if a.Loopback != b.Loopback {
    return !a.Loopback
  }
  if a.V6 != b.V6 {
    return !a.V6
  }
  if a.isRfc1918 != b.isRfc1918 {
    return !a.isRfc1918
  }
  if a.Wireless != b.Wireless {
    return a.Wireless
  }
  if a.original != b.original {
    return a.original < b.original
  }
  return a.IP.String() < b.IP.String()
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
  print4 := false
  print6 := false
  external := false
  iface := false
  excludeDocker := true
  docker := "172.16.0.0/12"
  findAll := false
  printName := false
  format := ""
  raw := false
  asJson := false

  flag.BoolVar(&print4, "4", print4, "Print IPv4")
  flag.BoolVar(&print6, "6", print6, "Print IPv6")
  flag.BoolVar(&external, "x", external, "Fetch external address")
  flag.BoolVar(&iface, "i", iface, "Fetch addresses per interface")
  flag.BoolVar(&excludeDocker, "nodocker", excludeDocker, "Exclude Docker interface")
  flag.StringVar(&docker, "dockernet", docker, "Docker network to exclude")
  flag.BoolVar(&printName, "name", printName, "Print interface name")
  flag.BoolVar(&printName, "n", printName, "Print interface name (alias)")
  flag.BoolVar(&findAll, "all", findAll, "Keep going after first match")
  flag.BoolVar(&findAll, "a", findAll, "Keep going after first match (alias)")
  flag.StringVar(&format, "fmt", format, "Output format")
  flag.BoolVar(&raw, "raw", raw, "Accept format string as-is (no newline)")
  flag.BoolVar(&asJson, "json", asJson,
    "Output as JSON objects (same as -fmt='{{json .}}')")
  flag.BoolVar(&asJson, "j", asJson, "Output as JSON objects (alias)")
  flag.Parse()

  if !external && !iface {
    iface = true
  }

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

  var namedAddrs []namedAddr
  if external {
    namedAddrs = getExternalNamedAddrs()
  }
  if iface {
    namedAddrs = append(namedAddrs, getInterfaceNamedAddrs()...)
  }

  for _, addr := range namedAddrs {
    ip := addr.ip
    v6 := ip.To4() == nil
    if xor(print4, print6) && xor(print6, v6) {
      continue
    }
    found = append(found, foundAddr{
      IP: ip,
      preferred: anyContains(acceptable, ip),
      rejected: anyContains(rejectable, ip),
      isRfc1918: anyContains(rfc1918, ip),
      Loopback: ip.IsLoopback(),
      V6: v6,
      original: len(found),
      Name: addr.name,
      Wireless: isWirelessInterface(addr.name),
    })
  }

  if len(found) == 0 {
    os.Exit(1)
  }

  sort.Sort(ByAttributes{found})

  if format == "" && asJson {
    format = "{{json .}}"
  }
  if format == "" {
    if printName {
      format = "{{.Name}}\t"
    }
    format += "{{.IP}}"
  }
  if !raw {
    format += "\n"
  }

  toJson := func(v interface{}) string {
    b, e := json.Marshal(v)
    if e != nil {
      return e.Error()
    }
    return string(b)
  }

  tmpl, err := template.New("line").Funcs(template.FuncMap{
    "json": toJson,
  }).Parse(format)
  if err != nil {
    log.Fatal("Error in template: ", err)
  }

  for _, addr := range found {
    err := tmpl.Execute(os.Stdout, addr)
    if err != nil {
      log.Fatal(err)
    }
    if !findAll {
      break
    }
  }
}
