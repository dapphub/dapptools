package main

import (
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/accounts"
	"github.com/ethereum/go-ethereum/accounts/keystore"
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
	app.Version = "0.5.1"
	app.Commands = []cli.Command {
		cli.Command {
			Name: "list-accounts",
			Aliases: []string{"ls"},
			Usage: "list accounts in keystore",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name: "key-store",
					Usage: "path to key store",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"key-store",
				}
		
				for _, required := range(requireds) {
					if c.String(required) == "" {
						return cli.NewExitError("ethsign: missing required parameter --" + required, 1)
					}
				}
				ks := keystore.NewKeyStore(
					c.String("key-store"), keystore.StandardScryptN, keystore.StandardScryptP)
				
				accts := ks.Accounts()
				for _, x := range(accts) {
					fmt.Println(x.Address.Hex())
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
					Usage: "path to key store",
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
					"nonce", "value", "gas-price", "gas-limit",
					"chain-id", "key-store", "passphrase-file",
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
				
				ks := keystore.NewKeyStore(
					c.String("key-store"), keystore.StandardScryptN, keystore.StandardScryptP)

				accts := ks.Accounts()
				
				if c.String("from") == "" {
					for _, x := range(accts) {
						fmt.Fprintf(os.Stderr, "ethsign: found account %s\n", x.Address.Hex())
					}
					return cli.NewExitError("ethsign: choose a signing account with --from", 1)
				}

				acct, err := ks.Find(accounts.Account { Address: from })
				if err != nil {
					for _, x := range(accts) {
						fmt.Fprintf(os.Stderr, "ethsign: available account: %s\n", x.Address.Hex())
					}
					return cli.NewExitError("ethsign: failed to open " + from.Hex() + " in keystore", 1)
				}
				
				passphraseFile, err := ioutil.ReadFile(c.String("passphrase-file"))
				if err != nil {
					return cli.NewExitError("ethsign: failed to read passphrase file", 1)
				}

				passphrase := strings.TrimSuffix(string(passphraseFile), "\n")

				tx := types.NewTransaction(nonce, to, value, gasLimit, gasPrice, data)
				
				signed, err := ks.SignTxWithPassphrase(acct, passphrase, tx, chainID)
				if err != nil {
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
