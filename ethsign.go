package main

import (
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/accounts/keystore"
	"github.com/ethereum/go-ethereum/core/types"

  "os"
	"fmt"
	"io/ioutil"
	
	"gopkg.in/urfave/cli.v1"
)

func main() {
	app := cli.NewApp()
	app.Name = "ethsign"
	app.Usage = "sign Ethereum transactions using a JSON keyfile"
	app.Flags = []cli.Flag {
		cli.StringFlag{
			Name: "key-file",
			Usage: "path to keystore JSON file",
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
			Name: "value",
			Usage: "transaction value",
		},
		cli.StringFlag{
			Name: "data",
			Usage: "hex data",
		},
	}
	
	app.Action = func(c *cli.Context) error {
		to := common.StringToAddress(c.String("to"))
		nonce := math.MustParseUint64(c.String("nonce"))
		gasPrice := math.MustParseBig256(c.String("gas-price"))
		gasLimit := math.MustParseBig256(c.String("gas-limit"))
		value := math.MustParseBig256(c.String("value"))
		data := hexutil.MustDecode(c.String("data"))
		chainID := math.MustParseBig256(c.String("chain-id"))

		keyjson, err := ioutil.ReadFile(c.String("key-file"))
		if err != nil {
			return cli.NewExitError("ethsign: failed to read key file", 1)
		}

		passphrase, err := ioutil.ReadFile(c.String("passphrase-file"))
		if err != nil {
			return cli.NewExitError("ethsign: failed to read passphrase file", 1)
		}
		
		key, err := keystore.DecryptKey(keyjson, string(passphrase))
		if err != nil {
			return cli.NewExitError("ethsign: failed to decrypt key", 1)
		}
		
		tx := types.NewTransaction(nonce, to, value, gasLimit, gasPrice, data)
		signer := types.NewEIP155Signer(chainID)
		
		signed, err := types.SignTx(tx, signer, key.PrivateKey)
		if err != nil {
			return cli.NewExitError("ethsign: failed to sign tx", 1)
		}

		json, _ := signed.MarshalJSON()
		fmt.Println(string(json[:]))
		
		return nil
	}
	
	app.Run(os.Args)
}
