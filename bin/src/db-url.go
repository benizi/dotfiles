package main

import (
	"flag"
	"fmt"
	"gopkg.in/yaml.v2"
	"io/ioutil"
	"log"
	"net/url"
	"os"
	"os/exec"
	"strconv"
	"strings"
)

var (
	environment string
	configPath string
	dbURL string
	herokuApp string
	herokuEnvVar = "DATABASE_URL"
)

type Config map[string]interface{}
type Configs map[string]Config

// Pull environment from:
// 1. explicitly-set or previously-cached string
// 2. $ENV environment variable
// 3. $RAILS_ENV environment variable
// 4. default: "development"
func env() string {
	if len(environment) != 0 {
		return environment
	}
	environment = os.Getenv("ENV")
	if len(environment) != 0 {
		return environment
	}
	environment = os.Getenv("RAILS_ENV")
	if len(environment) != 0 {
		return environment
	}
	return "development"
}

// Pull config from:
// 1. explicitly-set string
// 2. config/database.yml
func path() string {
	if len(configPath) != 0 {
		return configPath
	}
	return "config/database.yml"
}

// Pull database URL from:
// 1. explicitly-set string
// 2. Heroku app config var
// 3. $DATABASE_URL environment variable
func uri() string {
	if len(dbURL) != 0 {
		return dbURL
	}

	if len(herokuApp) != 0 {
		return fetchHerokuConfig()
	}

	dbURL = os.Getenv("DATABASE_URL")
	return dbURL
}

func fetchHerokuConfig() string {
	args := []string{"config:get", herokuEnvVar, "-a", herokuApp}
	out, err := exec.Command("heroku", args...).Output()
	if err == nil {
		return string(out[0:len(out)-1]) // strip trailing newline
	}
	return ""
}

// Only Ruby can parse embedded Ruby (ERB)
func parseERB(in []byte) []byte {
	if !strings.Contains(string(in), "<%") {
		return in
	}

	var err error
	// consolidate tedious error handling
	errHandle := func() {
		if err != nil {
			log.Panic(err)
		}
	}

	cmd := exec.Command("ruby", "-rerb", "-e", "puts ERB.new(ARGF.read).result")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		log.Panic(err)
	}
	stdout, err := cmd.StdoutPipe()
	errHandle()
	err = cmd.Start()
	errHandle()
	stdin.Write(in)
	stdin.Close()
	out, err := ioutil.ReadAll(stdout)
	errHandle()
	err = cmd.Wait()
	errHandle()
	return out
}

// Read the file, parse out the ERB (if needed), and return the mappings
func parseDatabaseConfig(filename string) Configs {
	var parsed Configs
	body, err := ioutil.ReadFile(filename)
	if err != nil {
		log.Panic(err)
	}
	withoutERB := parseERB(body)
	err = yaml.Unmarshal(withoutERB, &parsed)
	if err != nil {
		log.Panic(err)
	}
	return parsed
}

// Read the database config and pull out the specified environment's settings
func fromPath(filename, env string) Config {
	configs := parseDatabaseConfig(filename)
	if config, found := configs[env]; found {
		return config
	}
	return nil
}

func fromURL(uri string) Config {
	parsed, err := url.Parse(uri)
	if err != nil {
		log.Panic(err)
	}

	ret := Config{}

	if len(parsed.Scheme) > 0 {
		ret["adapter"] = parsed.Scheme
	}

	if len(parsed.Host) > 0 {
		hostPort := strings.Split(parsed.Host, ":")
		switch len(hostPort) {
		case 1:
			ret["host"] = hostPort[0]
		case 2:
			port, err := strconv.ParseInt(hostPort[1], 0, 0)
			if err == nil {
				ret["host"] = hostPort[0]
				ret["port"] = port
			} else {
				ret["host"] = parsed.Host
			}
		default:
			ret["host"] = parsed.Host
		}
	}

	if len(parsed.Path) > 1 {
		// remove leading slash
		ret["database"] = parsed.Path[1:len(parsed.Path)]
	}

	if parsed.User != nil {
		ret["username"] = parsed.User.Username()
		pass, present := parsed.User.Password()
		if present {
			ret["password"] = pass
		}
	}

	for k, strs := range(parsed.Query()) {
		if len(strs) != 1 {
			ret[k] = strs
		} else {
			s := strs[0]
			num, err := strconv.ParseInt(s, 0, 0)
			if err == nil {
				ret[k] = num
			} else {
				ret[k] = s
			}
		}
	}

	return ret
}

func mappedAdapter(adapter string) string {
	switch adapter {
	case "pg", "postgresql":
		return "postgres"
	default:
		return adapter
	}
}

func (config Config) ToURL() *url.URL {
	ret := &url.URL{}
	hostPort := []string{}
	user, pass := "", ""
	hasUser, hasPass := false, false
	params := &url.Values{}
	hasParams := false

	for k, v := range(config) {
		var val string
		switch t := v.(type) {
		case string:
			val = t
		case interface{String() string}:
			val = t.String()
		case nil:
			val = ""
		default:
			val = fmt.Sprintf("%v", v)
		}

		switch k {
		case "adapter":
			ret.Scheme = mappedAdapter(val)
		case "host", "hostname":
			hostPort = append([]string{val}, hostPort...)
		case "port":
			hostPort = append(hostPort, val)
		case "database":
			ret.Path = fmt.Sprintf("/%s", val)
		case "user", "username":
			user = val
			hasUser = true
		case "pass", "password":
			pass = val
			hasPass = true
		default:
			params.Add(k, val)
			hasParams = true
		}
	}

	if len(hostPort) != 0 {
		ret.Host = strings.Join(hostPort, ":")
	}

	if hasPass {
		ret.User = url.UserPassword(user, pass)
	} else if hasUser {
		ret.User = url.User(user)
	}

	if hasParams {
		ret.RawQuery = params.Encode()
	}

	return ret
}

func inputConfig(useURL, useYAML bool) Config {
	if useURL {
		return fromURL(uri())
	}
	return fromPath(path(), env())
}

func outputConfig(cfg Config, asURL, asYAML bool) {
	if asYAML {
		yml, err := yaml.Marshal(&Configs{env(): cfg})
		if err != nil {
			log.Panic(err)
		}
		os.Stdout.Write(yml)
	} else {
		fmt.Println(cfg.ToURL().String())
	}
}

func (c Config) Transform(name string) {
	if len(name) > 0 {
		c["database"] = name
	}
}

func main() {
	// Input options
	parseYAML := false
	parseURL := false

	flag.BoolVar(&parseYAML, "fromyaml", parseYAML, "Parse starting config from YAML")
	flag.BoolVar(&parseURL, "fromurl", parseURL, "Parse starting config from URL")
	flag.StringVar(&herokuApp, "fromheroku", herokuApp, "Heroku app name")
	flag.StringVar(&herokuEnvVar, "herokuvar", herokuEnvVar, "Heroku environment variable name")

	// Output options
	emitURL := false
	emitYAML := false

	flag.BoolVar(&emitURL, "url", emitURL, "Output a URL")
	flag.BoolVar(&emitYAML, "yaml", emitYAML, "Output YAML")

	// Selection options
	flag.StringVar(&environment, "env", environment, "Environment name")
	flag.StringVar(&configPath, "path", configPath, "database.yml file to parse")
	flag.StringVar(&dbURL, "dburl", dbURL, "database URL to parse")

	// Transformation options
	dbName := ""

	flag.StringVar(&dbName, "dbname", dbName, "Use this as the database name")

	flag.Parse()

	if len(herokuApp) != 0 {
		parseURL = true
		if len(environment) == 0 {
			environment = herokuApp
		}
	}

	cfg := inputConfig(parseURL, parseYAML)
	cfg.Transform(dbName)
	outputConfig(cfg, emitURL, emitYAML)
}
