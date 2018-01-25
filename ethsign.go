package main

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/accounts"
	"github.com/ethereum/go-ethereum/accounts/keystore"
	"github.com/ethereum/go-ethereum/accounts/usbwallet"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/rlp"

	"os"
	"fmt"
	"io/ioutil"
	"strings"
	"syscall"
	"runtime"
	
	"gopkg.in/urfave/cli.v1"

	"golang.org/x/crypto/ssh/terminal"
)

// https://github.com/ethereum/go-ethereum/blob/master/internal/ethapi/api.go#L373
// signHash is a helper function that calculates a hash for the given message that can be
// safely used to calculate a signature from.
//
// The hash is calculated as
//   keccak256("\x19Ethereum Signed Message:\n"${message length}${message}).
//
// This gives context to the signed message and prevents signing of transactions.
func signHash(data []byte) []byte {
	msg := fmt.Sprintf("\x19Ethereum Signed Message:\n%d%s", len(data), data)
	return crypto.Keccak256([]byte(msg))
}

func main() {
	var defaultKeyStores cli.StringSlice
	if runtime.GOOS == "darwin" {
		defaultKeyStores = []string{
			os.Getenv("HOME") + "/Library/Ethereum/keystore",
			os.Getenv("HOME") + "/Library/Application Support/io.parity.ethereum/keys/ethereum",
		}
	} else if runtime.GOOS == "windows" {
		// XXX: I'm not sure these paths are correct, but they are from geth/parity wikis.
		defaultKeyStores = []string{
			os.Getenv("APPDATA") + "/Ethereum/keystore",
			os.Getenv("APPDATA") + "/Parity/Ethereum/keys",
		}
	} else {
		defaultKeyStores = []string{
			os.Getenv("HOME") + "/.ethereum/keystore",
			os.Getenv("HOME") + "/.local/share/io.parity.ethereum/keys/ethereum",
		}
	}
	
	app := cli.NewApp()
	app.Name = "ethsign"
	app.Usage = "sign Ethereum transactions using a JSON keyfile"
	app.Version = "0.8.2"
	app.Commands = []cli.Command {
		cli.Command {
			Name: "list-accounts",
			Aliases: []string{"ls"},
			Usage: "list accounts in keystore and USB wallets",
			Flags: []cli.Flag{
				cli.StringSliceFlag{
					Name: "key-store",
					Usage: "path to key store",
					EnvVar: "ETH_KEYSTORE",
				},
			},
			Action: func(c *cli.Context) error {
				backends := []accounts.Backend{}

				var paths []string
				if len(c.StringSlice("key-store")) == 0 {
					paths = defaultKeyStores
				} else {
					paths = c.StringSlice("key-store")
				}
				for _, x := range(paths) {
					ks := keystore.NewKeyStore(
						x, keystore.StandardScryptN, keystore.StandardScryptP)
					backends = append(backends, ks)
				}

				if ledgerhub, err := usbwallet.NewLedgerHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Ledgers")
				} else {
					backends = append(backends, ledgerhub)
				}
				if trezorhub, err := usbwallet.NewTrezorHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Trezors")
				} else {
					backends = append(backends, trezorhub)
				}

				manager := accounts.NewManager(backends...)
				wallets := manager.Wallets()
				for _, x := range(wallets) {
					if x.URL().Scheme == "keystore" {
						for _, y := range(x.Accounts()) {
							fmt.Printf("%s keystore\n", y.Address.Hex())
						}
					} else if x.URL().Scheme == "ledger" {
						x.Open("")
						for j := 0; j <= 3; j++ {
							pathstr := fmt.Sprintf("m/44'/60'/0'/%d", j)
							path, _ := accounts.ParseDerivationPath(pathstr)
							z, err := x.Derive(path, false)
							if err != nil {
								return cli.NewExitError("ethsign: couldn't use Ledger: needs to be in Ethereum app with browser support off", 1)
							} else {
								fmt.Printf("%s ledger-%s\n", z.Address.Hex(), pathstr)
							}
						}
					}
				}
				
				return nil
			},
		},

		cli.Command {
			Name: "transaction",
			Aliases: []string{"tx"},
			Usage: "make a signed transaction",
			Flags: []cli.Flag{
				cli.StringSliceFlag{
					Name: "key-store",
					Usage: "path to key store",
					EnvVar: "ETH_KEYSTORE",
				},
				cli.BoolFlag{
					Name: "create",
					Usage: "make a contract creation transaction",
				},
				cli.StringFlag{
					Name: "from",
					Usage: "address of signing account",
					EnvVar: "ETH_FROM",
				},
				cli.StringFlag{
					Name: "passphrase-file",
					Usage: "path to file containing account passphrase",
				},
				cli.StringFlag{
					Name: "chain-id",
					Usage: "chain ID",
				},
				cli.StringFlag{
					Name: "to",
					Usage: "account of recipient",
				},
				cli.StringFlag{
					Name: "nonce",
					Usage: "account nonce",
				},
				cli.StringFlag{
					Name: "gas-price",
					Usage: "gas price",
				},
				cli.StringFlag{
					Name: "gas-limit",
					Usage: "gas limit",
				},
				cli.StringFlag{
					Name: "value",
					Usage: "transaction value",
				},
				cli.StringFlag{
					Name: "data",
					Usage: "hex data",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"nonce", "value", "gas-price", "gas-limit", "chain-id", "from",
				}

				for _, required := range(requireds) {
					if c.String(required) == "" {
						return cli.NewExitError("ethsign: missing required parameter --" + required, 1)
					}
				}

				create := c.Bool("create")
				
				if (c.String("to") == "" && !create) || (c.String("to") != "" && create) {
					return cli.NewExitError("ethsign: need exactly one of --to or --create", 1)
				}

				if (create && c.String("data") == "") {
					return cli.NewExitError("ethsign: need --data when doing --create", 1)
				}

				to := common.HexToAddress(c.String("to"))
				from := common.HexToAddress(c.String("from"))
				nonce := math.MustParseUint64(c.String("nonce"))
				gasPrice := math.MustParseBig256(c.String("gas-price"))
				gasLimit := math.MustParseBig256(c.String("gas-limit"))
				value := math.MustParseBig256(c.String("value"))
				chainID := math.MustParseBig256(c.String("chain-id"))
				
				dataString := c.String("data")
				if dataString == "" {
					dataString = "0x"
				}
				data := hexutil.MustDecode(dataString)
				
				backends := []accounts.Backend{ }

				var paths []string
				if len(c.StringSlice("key-store")) == 0 {
					paths = defaultKeyStores
				} else {
					paths = c.StringSlice("key-store")
				}
				for _, x := range(paths) {
					ks := keystore.NewKeyStore(
						x, keystore.StandardScryptN, keystore.StandardScryptP)
					backends = append(backends, ks)
				}

				if ledgerhub, err := usbwallet.NewLedgerHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Ledgers")
				} else {
					backends = append(backends, ledgerhub)
				}
				if trezorhub, err := usbwallet.NewTrezorHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Trezors")
				} else {
					backends = append(backends, trezorhub)
				}

				manager := accounts.NewManager(backends...)
				wallets := manager.Wallets()
				var wallet accounts.Wallet
				var acct *accounts.Account

				needPassphrase := true

				Scan:
				for _, x := range(wallets) {
					if x.URL().Scheme == "keystore" {
						for _, y := range(x.Accounts()) {
							if (y.Address == from) {
								wallet = x
								acct = &y
								break Scan
							}
						}
					} else if x.URL().Scheme == "ledger" {
						x.Open("")
						for j := 0; j <= 3; j++ {
							pathstr := fmt.Sprintf("m/44'/60'/0'/%d", j)
							path, _ := accounts.ParseDerivationPath(pathstr)
							y, err := x.Derive(path, true)
							if err != nil {
								return cli.NewExitError("ethsign: Ledger needs to be in Ethereum app with browser support off", 1)
							} else {
								if y.Address == from {
									wallet = x
									acct = &y
									needPassphrase = false
									break Scan
								}
							}
						}
					}
				}

				if acct == nil {
					return cli.NewExitError(
						"ethsign: account not found",
						1,
					)
				}


				passphrase := ""

				if needPassphrase {
					if c.String("passphrase-file") != "" {
						passphraseFile, err := ioutil.ReadFile(c.String("passphrase-file"))
						if err != nil {
							return cli.NewExitError("ethsign: failed to read passphrase file", 1)
						}
						
						passphrase = strings.TrimSuffix(string(passphraseFile), "\n")
					} else {
						fmt.Fprintf(os.Stderr, "Ethereum account passphrase (not echoed): ")
						bytes, err := terminal.ReadPassword(int(syscall.Stdin))
						if err != nil {
							return cli.NewExitError("ethsign: failed to read passphrase", 1)
						} else {
							passphrase = string(bytes)
						}
					}
				} else {
					fmt.Fprintf(os.Stderr, "Waiting for hardware wallet confirmation...\n")
				}

				var tx *types.Transaction
				if create {
					tx = types.NewContractCreation(nonce, value, gasLimit, gasPrice, data)
				} else {
					tx = types.NewTransaction(nonce, to, value, gasLimit, gasPrice, data)
				}
				
				signed, err := wallet.SignTxWithPassphrase(*acct, passphrase, tx, chainID)
				if err != nil {
					return cli.NewExitError("ethsign: failed to sign tx", 1)
				}

				encoded, _ := rlp.EncodeToBytes(signed)
				fmt.Println(hexutil.Encode(encoded[:]))
				
				return nil
			},
		},

		cli.Command{
			Name:    "message",
			Aliases: []string{"msg"},
			Usage:   "sign arbitrary data with header prefix",
			Flags: []cli.Flag{
				cli.StringSliceFlag{
					Name:   "key-store",
					Usage:  "path to key store",
					EnvVar: "ETH_KEYSTORE",
				},
				cli.StringFlag{
					Name:   "from",
					Usage:  "address of signing account",
					EnvVar: "ETH_FROM",
				},
				cli.StringFlag{
					Name:  "passphrase-file",
					Usage: "path to file containing account passphrase",
				},
				cli.StringFlag{
					Name:  "data",
					Usage: "hex data to sign",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"from", "data",
				}

				for _, required := range requireds {
					if c.String(required) == "" {
						return cli.NewExitError("ethsign: missing required parameter --"+required, 1)
					}
				}

				from := common.HexToAddress(c.String("from"))

				dataString := c.String("data")
				if !strings.HasPrefix(dataString, "0x") {
					dataString = "0x" + dataString
				}
				data := hexutil.MustDecode(dataString)

				backends := []accounts.Backend{ }

				var paths []string
				if len(c.StringSlice("key-store")) == 0 {
					paths = defaultKeyStores
				} else {
					paths = c.StringSlice("key-store")
				}
				for _, x := range(paths) {
					ks := keystore.NewKeyStore(
						x, keystore.StandardScryptN, keystore.StandardScryptP)
					backends = append(backends, ks)
				}

				if ledgerhub, err := usbwallet.NewLedgerHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Ledgers")
				} else {
					backends = append(backends, ledgerhub)
				}
				if trezorhub, err := usbwallet.NewTrezorHub(); err != nil {
					fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Trezors")
				} else {
					backends = append(backends, trezorhub)
				}

				manager := accounts.NewManager(backends...)
				wallets := manager.Wallets()

				var wallet accounts.Wallet
				var acct *accounts.Account

				needPassphrase := true

			Scan:
				for _, x := range wallets {
					if x.URL().Scheme == "keystore" {
						for _, y := range x.Accounts() {
							if y.Address == from {
								wallet = x
								acct = &y
								break Scan
							}
						}
					} else if x.URL().Scheme == "ledger" {
						x.Open("")
						for j := 0; j <= 3; j++ {
							pathstr := fmt.Sprintf("m/44'/60'/0'/%d", j)
							path, _ := accounts.ParseDerivationPath(pathstr)
							y, err := x.Derive(path, true)
							if err != nil {
								return cli.NewExitError("ethsign: Ledger needs to be in Ethereum app with browser support off", 1)
							}
							if y.Address == from {
								wallet = x
								acct = &y
								needPassphrase = false
								break Scan
							}
						}
					}
				}

				if acct == nil {
					return cli.NewExitError(
						"ethsign: account not found",
						1,
					)
				}

				passphrase := ""

				if needPassphrase {
					if c.String("passphrase-file") != "" {
						passphraseFile, err := ioutil.ReadFile(c.String("passphrase-file"))
						if err != nil {
							return cli.NewExitError("ethsign: failed to read passphrase file", 1)
						}

						passphrase = strings.TrimSuffix(string(passphraseFile), "\n")
					} else {
						fmt.Fprintf(os.Stderr, "Ethereum account passphrase (not echoed): ")
						bytes, err := terminal.ReadPassword(int(syscall.Stdin))
						if err != nil {
							return cli.NewExitError("ethsign: failed to read passphrase", 1)
						}
						passphrase = string(bytes)
					}
				} else {
					fmt.Fprintf(os.Stderr, "Waiting for hardware wallet confirmation...\n")
				}

				signature, err := wallet.SignHashWithPassphrase(*acct, passphrase, signHash(data))

				if err != nil {
					return cli.NewExitError("ethsign: failed to sign message", 1)
				}

				signature[64] += 27 // Transform V from 0/1 to 27/28 according to the yellow paper

				fmt.Println(hexutil.Encode(signature))

				return nil
			},
		},
	}
	
	app.Run(os.Args)
}
