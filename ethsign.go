package main

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/accounts"
	"github.com/ethereum/go-ethereum/accounts/keystore"
	"github.com/ethereum/go-ethereum/accounts/usbwallet"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/rlp"

	"os"
	"fmt"
	"io/ioutil"
	"strings"
	
	"gopkg.in/urfave/cli.v1"
)

func main() {
	app := cli.NewApp()
	app.Name = "ethsign"
	app.Usage = "sign Ethereum transactions using a JSON keyfile"
	app.Version = "0.7"
	app.Commands = []cli.Command {
		cli.Command {
			Name: "list-accounts",
			Aliases: []string{"ls"},
			Usage: "list accounts in keystore and USB wallets",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name: "key-store",
					Value: os.Getenv("HOME") + "/.ethereum/keystore",
					Usage: "path to key store",
				},
			},
			Action: func(c *cli.Context) error {
				ks := keystore.NewKeyStore(
					c.String("key-store"), keystore.StandardScryptN, keystore.StandardScryptP)
				
				backends := []accounts.Backend{ ks }

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
				cli.StringFlag{
					Name: "key-store",
					Value: os.Getenv("HOME") + "/.ethereum/keystore",
					Usage: "path to key store",
				},
				cli.StringFlag{
					Name: "derivation-path",
					Usage: "BIP44 derivation path for USB wallet account",
				},
				cli.BoolFlag{
					Name: "create",
					Usage: "make a contract creation transaction",
				},
				cli.StringFlag{
					Name: "from",
					Usage: "address of signing account",
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

				if (c.String("passphrase-file") == "" && c.String("derivation-path") == "") {
					return cli.NewExitError(
						"ethsign: need either --passphrase-file or --derivation-path", 1,
					)
				}
				if (c.String("passphrase-file") != "" && c.String("derivation-path") != "") {
					return cli.NewExitError(
						"ethsign: cannot use both --passphrase-file and --derivation-path", 1,
					)
				}
				
				to := common.HexToAddress(c.String("to"))
				from := common.HexToAddress(c.String("from"))
				nonce := math.MustParseUint64(c.String("nonce"))
				gasPrice := math.MustParseBig256(c.String("gas-price"))
				gasLimit := math.MustParseBig256(c.String("gas-limit"))
				value := math.MustParseBig256(c.String("value"))
				chainID := math.MustParseBig256(c.String("chain-id"))
				derivationPathString := c.String("derivation-path")
				
				dataString := c.String("data")
				if dataString == "" {
					dataString = "0x"
				}
				data := hexutil.MustDecode(dataString)
				
				ks := keystore.NewKeyStore(
					c.String("key-store"), keystore.StandardScryptN, keystore.StandardScryptP)
				
				backends := []accounts.Backend{ ks }

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
						if derivationPathString == "" {
							return cli.NewExitError("ethsign: Ledger found but no --derivation-path given", 1)
						}
						
						x.Open("")
						path, _ := accounts.ParseDerivationPath(derivationPathString)
						y, err := x.Derive(path, true)
						if err != nil {
							return cli.NewExitError(
								"ethsign: Ledger needs to be in Ethereum app with browser support off",
								1,
							)
						} else {
							if y.Address == from {
								wallet = x
								acct = &y
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
				
				if (c.String("passphrase-file") != "") {
					passphraseFile, err := ioutil.ReadFile(c.String("passphrase-file"))
					if err != nil {
						return cli.NewExitError("ethsign: failed to read passphrase file", 1)
					}
					
					passphrase = strings.TrimSuffix(string(passphraseFile), "\n")
				}

				tx := types.NewTransaction(nonce, to, value, gasLimit, gasPrice, data)
				
				signed, err := wallet.SignTxWithPassphrase(*acct, passphrase, tx, chainID)
				if err != nil {
					fmt.Fprintf(os.Stderr, "%s\n%s\n%s\n", wallet, *acct, err)
					return cli.NewExitError("ethsign: failed to sign tx", 1)
				}

				encoded, _ := rlp.EncodeToBytes(signed)
				fmt.Println(hexutil.Encode(encoded[:]))
				
				return nil
			},
		},
	}
	
	app.Run(os.Args)
}
