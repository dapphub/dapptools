package main

import (
	"bytes"
	"syscall"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"regexp"
	"path/filepath"
	"io/fs"
	"encoding/json"

	"github.com/urfave/cli/v2"
)

// -- app --

func main() {
	app := &cli.App{
		Name:  "depend",
		Usage: "dependency management and build orchestration for ethereum smart contract projects",
		Commands: []*cli.Command{
			{
				Name:  "install",
				Usage: "install a new dependency",
				Action: func(c *cli.Context) error {
					name := c.Args().Get(1)
					repo, version, err := parseUrl(c.Args().Get(0))
					if err != nil {
						return err
					}

					os.Mkdir("lib", 0755)
					prefix := fmt.Sprintf("lib/%s@%s", name, version)

					if exists(prefix) {
						fmt.Printf("%s@%s is already installed", name, version)
						return nil
					}

					_, stderr, code := run("git", "subtree", "add", "--squash", "--prefix", prefix, repo, version)
					if code != 0 {
						return errors.New(fmt.Sprintln("git subtree:", stderr))
					}

					fmt.Println("installed", c.Args().Get(0), "to", prefix)
					return nil
				},
			},
			{
				Name:  "uninstall",
				Usage: "uninstall an existing dependency",
				Action: func(c *cli.Context) error {
					path := c.Args().Get(0)

					stdout, stderr, code := run("git", "status", "-u", "--porcelain")
					if code != 0 {
						return errors.New(fmt.Sprintln("git status:", stderr))
					}
					if stdout != "" {
						return errors.New("repository is dirty or contains untracked files. please commit or stash.")
					}

					_, stderr, code = run("git", "rm", "-r", path)
					if code != 0 {
						return errors.New(fmt.Sprintln("git rm:", stderr))
					}

					_, stderr, code = run("git", "commit", "-am", fmt.Sprintln("uninstalled:", path))
					if code != 0 {
						return errors.New(fmt.Sprintln("git commit:", stderr))
					}

					return nil
				},
			},
			{
				Name:  "plan",
				Usage: "produce compiler inputs",
				Action: func(c *cli.Context) error {
					// gather all the files in src
					srcs, err := getFilesByExt("src", ".sol")
					if err != nil { return err }

					libs, err := getFilesByExt("lib", ".sol")
					if err != nil { return err }

					remppings, err := mkRemappings("lib")
					if err != nil { return err }

					// for each output file build a compiler input json
					inputs := map[string]InputJSON{}
					for _, out := range srcs {
						inputs[out] = mkInputJSON(append(srcs, libs...), remppings, out)
					}

					// write the inputs as json
					for k, v := range inputs {
						j, e := json.Marshal(v)
						if e != nil { return e }

						e = writeFile(fmt.Sprintf("%s/%s.json", "plans", k), j)
						if e != nil { return e }
					}

					return nil
				},
			},
			{
				Name:  "build",
				Usage: "run all build plans",
				Action: func(c *cli.Context) error {
					// TODO: make me parallel
					// TODO: make me incremental
					// TODO: handle multiple solc versions

					plans, err := getFilesByExt("plans", ".json")
					if err != nil { return err }

					for _, plan := range plans {
						//fmt.Println(strings.Split(plan)[1:].))
						out := fmt.Sprintf("out/%s", filepath.Join(strings.Split(plan, "/")[1:]...))

						stdout, stderr, code := run("solc", "--allow-paths", ".", "--standard-json", plan)
						if code != 0 {
							return errors.New(fmt.Sprintln("solc:", stderr))
						}

						err = writeFile(out, []byte(stdout))
						if err != nil { return err }
					}

					return nil
				},
			},
		},
	}

	err := app.Run(os.Args)
	if err != nil {
		fmt.Println("error:", err)
		os.Exit(1)
	}
}

// -- types --

type SourceFile struct {
	Urls []string `json:"urls"`
}

type InputJSON struct {
    Language string `json:"language"`
	Sources map[string]SourceFile `json:"sources"`
	Settings SolcSettings `json:"settings"`
}

type SolcSettings struct {
	Remappings []string `json:"remappings"`
	Optimizer OptimizerSettings `json:"optimizer"`
	EvmVersion string `json:"evmVersion"`
	Libraries LibrarySettings `json:"libraries"`
	OutputSelection OutputSelection `json:"outputSelection"`
}

type LibrarySettings map[string](map[string]string)
type OutputSelection map[string](map[string][]string)

type OptimizerSettings struct {
	Enabled bool `json:"enabled"`
	Runs uint `json:"runs"`
}

// -- parsing --

func parseUrl(url string) (string, string, error) {
	parsed := strings.Split(url, "@")
	if len(parsed) != 2 {
		return "", "", errors.New("could not parse url")
	}
	repo := parsed[0]
	version := parsed[1]

	_, _, code := run("git", "ls-remote", "--exit-code", "--heads", repo, version)
	if code == 0 {
		return "", "", errors.New("installing from branches is forbidden")
	}

	_, _, code = run("git", "ls-remote", "--exit-code", "--tags", repo, version)
	if code != 0 && !isGitHash(version) {
		return "", "", errors.New(fmt.Sprintln(version, "is not a valid git hash"))
	}

	return repo, version, nil
}

// -- planning --

func mkSources(sources []string) map[string]SourceFile {
	out := map[string]SourceFile{}
	for _, s := range sources {
		urls := []string{ s }
		out[s] = SourceFile{ Urls: urls }
	}
	return out
}

func mkInputJSON(srcs []string, remappings []string, output string) InputJSON {
	return InputJSON{
		Language: "Solidity",
		Sources: mkSources(srcs),
		Settings: SolcSettings {
			Remappings: remappings,
			Optimizer: OptimizerSettings{
				Enabled: true,
				Runs: 200,
			},
			EvmVersion: "berlin",
			Libraries: LibrarySettings{},
			OutputSelection: OutputSelection{
				output: map[string][]string {
					"*": []string { "*" },
					"": []string { "*" },
				},
			},
		},
	}
}

func mkRemappings(path string) ([]string, error) {
	// list dirs in path
	dirs, err := listSubDirs(path)
	if err != nil { return nil, err }

	remappings := []string{}
	for _, dir := range dirs {
		parsed := strings.Split(dir, "@")
		if len(parsed) != 2 {
			return nil, errors.New(fmt.Sprintln("could not parse dependency name:", dir))
		}
		name := parsed[0]
		version := parsed[1]
		if isGitHash(version) {
			version = version[0:8]
		}

		// build a remapping from `name@version/` to `lib/name@version/src/`
		// git commits are truncated to 8 chars
		r := fmt.Sprintf("%s@%s/=lib/%s/src/", name, version, dir)
		remappings = append(remappings, r)
	}

	return remappings, nil
}

// -- utils --

func listSubDirs(path string) ([]string, error) {
	files, err := os.ReadDir(path)
	if err != nil { return nil, err }

	dirs := []string{}
	for _, file := range files {
		if file.IsDir() {
			dirs = append(dirs, file.Name())
		}
	}

	return dirs, nil
}

func run(name string, args ...string) (string, string, int) {
	cmd := exec.Command(name, args...)

    var stdout, stderr bytes.Buffer
    cmd.Stdout = &stdout
    cmd.Stderr = &stderr

	var waitStatus syscall.WaitStatus
	if err := cmd.Run(); err != nil {
	  if exitError, ok := err.(*exec.ExitError); ok {
		waitStatus = exitError.Sys().(syscall.WaitStatus)
	  }
	} else {
	  waitStatus = cmd.ProcessState.Sys().(syscall.WaitStatus)
	}

    return string(stdout.Bytes()), string(stderr.Bytes()), waitStatus.ExitStatus()

}

func writeFile(path string, bytes []byte) error {
	dir := filepath.Dir(path)
	if !exists(dir) {
		err := os.MkdirAll(dir, 0755)
		if err != nil { return err }
	}

	f, err := os.Create(path)
    if err != nil {
		return err
	}

    _, err = f.Write(bytes)
    if err != nil {
        f.Close()
        return err
    }

    err = f.Close()
    if err != nil {
        return err
    }

	return nil
}

func getFilesByExt(root string, ext string) ([]string, error) {
	var files []string
	err := filepath.WalkDir(root, func(s string, d fs.DirEntry, e error) error {
	  if e != nil { return e }
	  if filepath.Ext(d.Name()) == ext {
		 files = append(files, s)
	  }
	  return nil
	})

	if err != nil {
		return nil, err
	} else {
		return files, nil
	}
}

func exists(path string) bool {
	_, err := os.Stat(path)
	return !os.IsNotExist(err)
}

func isGitHash(h string) bool {
	match, err := regexp.MatchString("^[a-fA-f0-9]{40}$", h)
	if err != nil {
		fmt.Println("internal error: regex:", err)
		os.Exit(1)
	}
	return match
}
