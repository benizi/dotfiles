package main

import (
  "bytes"
  "encoding/json"
  "flag"
  "fmt"
  "io/ioutil"
  "log"
  "net"
  "net/http"
  "os"
  "os/exec"
  "reflect"
  "regexp"
  "sort"
  "strings"
  "text/template"
  "unicode"

  // TODO: Dammit, Golang, I just want to reflect unexported fields.
  "unsafe"
)

const (
  fallbackService = "http://benizi.com/ip?raw=1"
)

type namedAddr struct {
  name string
  ip   net.IP
  mask net.IPMask
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

func namedIPs(name string, addrs []net.Addr) []namedAddr {
  named := []namedAddr{}
  for _, addr := range addrs {
    if network, ok := addr.(*net.IPNet); ok {
      named = append(named, namedAddr{
        name: name,
        ip: network.IP,
        mask: network.Mask,
      })
    }
  }
  return named
}

func getExternalNamedAddrs() []namedAddr {
  return namedIPs("external", getExternal())
}

func getInterfaceNamedAddrs() ([]namedAddr, error) {
  namedAddrs := []namedAddr{}
  ifis, err := net.Interfaces()
  if err != nil {
    return nil, err
  }
  for _, ifi := range ifis {
    addrs, err := ifi.Addrs()
    if err != nil {
      continue
    }
    namedAddrs = append(namedAddrs, namedIPs(ifi.Name, addrs)...)
  }
  return namedAddrs, nil
}

type foundAddr struct {
  IP net.IP
  Network string
  preferred bool
  rejected bool
  Loopback bool
  Local bool
  isRfc1918 bool
  Docker bool
  V6 bool
  original int
  Name string
  Wireless bool
}

var quiet = false

func (addr foundAddr) MarshalJSON() ([]byte, error) {
  ret := map[string]interface{}{}
  // TODO: workaround for unexported fields
  val := reflect.ValueOf(&addr).Elem()
  t := reflect.TypeOf(addr)
  for i := 0; i < val.NumField(); i++ {
    field := t.Field(i)
    if field.PkgPath != "" && quiet {
      continue
    }
    // TODO: workaround for unexported fields
    fval := val.Field(i)
    ftype := fval.Type()
    obj := reflect.NewAt(ftype, unsafe.Pointer(fval.UnsafeAddr())).Elem()
    key := snakeCase(field.Name)
    ret[key] = obj.Interface()
  }
  return json.Marshal(ret)
}

func snakeCase(s string) string {
  var b bytes.Buffer
  justUp := true
  for _, r := range s {
    upper := unicode.IsUpper(r)
    if upper {
      if !justUp {
        b.WriteRune('_')
      }
      b.WriteRune(unicode.ToLower(r))
    } else {
      b.WriteRune(r)
    }
    justUp = upper
  }
  return b.String()
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
  if a.Local != b.Local {
    return !a.Local
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

type netfilter struct {
  label    string
  networks []net.IPNet
}

func newNetfilter(label string) *netfilter {
  return &netfilter{label, nil}
}

func (f *netfilter) addCIDR(cidr string) error {
  _, network, err := net.ParseCIDR(expandCIDR(cidr))
  if err != nil {
    return fmt.Errorf("Failed to parse %s network: %s", f.label, cidr)
  }
  f.networks = append(f.networks, *network)
  return nil
}

func (f *netfilter) contains(ip net.IP) bool {
  for _, network := range f.networks {
    if network.Contains(ip) {
      return true
    }
  }
  return false
}

var (
  partialCIDR = regexp.MustCompile(`^(\d+(?:\.\d+){0,2})(?:/(\d+))?$`)
)

// expandCIDR takes a partial CIDR string and expands it to the correct IPv4
// length.
//
// Examples:
//   expandCIDR("10") == "10.0.0.0/8"
//   expandCIDR("192.168") == "192.168.0.0/16"
//   expandCIDR("172.16/12") == "172.16.0.0/12"
//
func expandCIDR(cidr string) string {
  parts := partialCIDR.FindStringSubmatch(cidr)
  if len(parts) == 0 {
    return cidr
  }
  octets := strings.Split(parts[1], ".")
  for len(octets) < 4 {
    octets = append(octets, "0")
  }
  class := parts[2]
  if class == "" {
    n := 32
    for i := 3; i >= 0; i-- {
      if octets[i] != "0" {
        break
      }
      n -= 8
    }
    class = fmt.Sprintf("%d", n)
  }
  return fmt.Sprintf("%s/%s", strings.Join(octets, "."), class)
}

func findDockerNets() ([]string, error) {
  cmdNets := exec.Command("docker", "network", "ls", "-q", "--no-trunc")
  out, err := cmdNets.Output()
  if err != nil {
    return nil, err
  }
  netIDs := strings.Fields(string(out))
  args := []string{"network", "inspect"}
  args = append(args, "--format", "{{range .IPAM.Config}}{{.Subnet}}{{end}}")
  args = append(args, netIDs...)
  cmdSubs := exec.Command("docker", args...)
  subout, err := cmdSubs.Output()
  if err != nil {
    return nil, err
  }
  return strings.Fields(string(subout)), nil
}

func main() {
  print4 := false
  print6 := false
  external := false
  iface := false
  docker := ""
  findAll := false
  reallyAll := false
  printName := false
  printMask := false
  skipDocker := false
  skipPrivate := false
  keepLoop := false
  keepLocal := false
  keepRejected := false
  keepAll := false
  onlyIfs := ""
  format := ""
  raw := false
  asJson := false

  flag.BoolVar(&print4, "4", print4, "Print IPv4")
  flag.BoolVar(&print6, "6", print6, "Print IPv6")
  flag.BoolVar(&external, "x", external, "Fetch external address")
  flag.BoolVar(&iface, "i", iface, "Fetch addresses per interface")
  flag.BoolVar(&skipDocker, "nodocker", skipDocker, "Omit Docker networks")
  flag.StringVar(&docker, "dockernet", docker, "Docker network to exclude")
  flag.BoolVar(&printName, "name", printName, "Print interface name")
  flag.BoolVar(&printName, "n", printName, "Print interface name (alias)")
  flag.BoolVar(&printMask, "cidr", printMask, "Print address in CIDR notation")
  flag.BoolVar(&printMask, "mask", printMask,
    "Print address in CIDR notation (alias)")
  flag.BoolVar(&findAll, "all", findAll, "Keep going after first match")
  flag.BoolVar(&findAll, "a", findAll, "Keep going after first match (alias)")
  flag.BoolVar(&skipPrivate, "nopriv", skipPrivate, "Omit RFC1918 addresses")
  flag.BoolVar(&keepLoop, "l", keepLoop,
    "Print loopback addresses (don't omit, just penalize)")
  flag.BoolVar(&keepLocal, "ll", keepLocal,
    "Print link-local addresses (don't omit, just penalize)")
  flag.BoolVar(&keepRejected, "rejected", keepRejected,
    "Print rejected addresses (don't omit, just penalize)")
  flag.BoolVar(&keepAll, "d", keepAll,
    "Print all addresses (overrides all but -4/-6; uses others to sort)")
  flag.BoolVar(&reallyAll, "A", reallyAll, "Alias for -a + -n")
  flag.StringVar(&format, "fmt", format, "Output template")
  flag.BoolVar(&raw, "raw", raw, "Accept template string as-is (no newline)")
  flag.BoolVar(&asJson, "json", asJson,
    "Output as JSON objects (same as -fmt='{{json .}}')")
  flag.BoolVar(&asJson, "j", asJson, "Output as JSON objects (alias)")
  flag.BoolVar(&quiet, "q", quiet,
    "Quiet output (currently: don't add unexported fields to JSON)")
  flag.StringVar(&onlyIfs, "ifs", onlyIfs,
    "Only show IPs on these interface names (comma-separated)")
  flag.Parse()

  switch {
  case !reallyAll:
  case skipDocker, onlyIfs != "":
    log.Println("-A overrides -skip-docker + -ifs")
  }

  if reallyAll {
    findAll = true
    printName = true
  }

  if docker != "" {
    skipDocker = true
  }

  if !external && !iface {
    iface = true
  }

  preferNets := newNetfilter("preferred")
  rfc1918 := newNetfilter("RFC 1918")
  dockerNets := newNetfilter("Docker")
  rejectNets := newNetfilter("rejected")

  var parseErrors []error

  addNet := func(filter *netfilter, cidr string) {
    err := filter.addCIDR(cidr)
    if err != nil {
      parseErrors = append(parseErrors, err)
    }
  }

  for _, cidr := range []string{"10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16"} {
    addNet(rfc1918, cidr)
  }

  if skipDocker {
    toSkip := strings.Fields(docker)
    cidrs, err := findDockerNets()
    if err == nil {
      toSkip = append(toSkip, cidrs...)
    } else {
      parseErrors = append(parseErrors, err)
    }
    for _, cidr := range toSkip {
      addNet(dockerNets, cidr)
    }
  }

  filterInterfaceNames := false
  okInterface := map[string]bool{}
  addInterface := func(ifi string) {
    filterInterfaceNames = true
    okInterface[ifi] = true
  }

  if onlyIfs != "" {
    for _, ifi := range strings.Split(onlyIfs, ",") {
      addInterface(ifi)
    }
  }

  for _, arg := range flag.Args() {
    if arg == "" {
      continue
    }
    if arg[0] == '%' {
      addInterface(arg[1:])
    } else if arg[0] == '!' || arg[0] == 'x' {
      addNet(rejectNets, arg[1:len(arg)])
    } else {
      addNet(preferNets, arg)
    }
  }

  if len(parseErrors) > 0 {
    for _, e := range parseErrors {
      log.Println(e)
    }
    log.Fatalln("Exiting: couldn't parse all network arguments.")
  }

  found := make([]foundAddr, 0)

  var namedAddrs []namedAddr
  if external {
    namedAddrs = getExternalNamedAddrs()
  }
  if iface {
    addrs, err := getInterfaceNamedAddrs()
    if err != nil {
      log.Fatalln(err)
    }
    namedAddrs = append(namedAddrs, addrs...)
  }

  for _, addr := range namedAddrs {
    ip := addr.ip
    v6 := ip.To4() == nil
    isPreferred := preferNets.contains(ip)
    isDocker := dockerNets.contains(ip)
    isPrivate := rfc1918.contains(ip)
    isRejected := rejectNets.contains(ip)
    isLoop := ip.IsLoopback()
    isLocal := ip.IsLinkLocalMulticast() || ip.IsLinkLocalUnicast()

    if xor(print4, print6) && xor(print6, v6) {
      continue
    }
    if filterInterfaceNames && !okInterface[addr.name] {
      continue
    }
    if !keepAll {
      if isPrivate && skipPrivate {
        continue
      }
      if isDocker && skipDocker {
        continue
      }
      if isRejected && !keepRejected {
        continue
      }
      if isLoop && !keepLoop {
        continue
      }
      if isLocal && !keepLocal {
        continue
      }
    }
    network := net.IPNet{IP: ip, Mask: addr.mask}
    found = append(found, foundAddr{
      IP: ip,
      Network: network.String(),
      preferred: isPreferred,
      rejected: isRejected,
      Docker: isDocker,
      isRfc1918: isPrivate,
      Loopback: isLoop,
      Local: isLocal,
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
    if printMask {
      format += "{{.Network}}"
    } else {
      format += "{{.IP}}"
    }
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
