package main

import (
	"github.com/ethereum/go-ethereum/accounts"
	"github.com/ethereum/go-ethereum/accounts/keystore"
	"github.com/ethereum/go-ethereum/accounts/usbwallet"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/hexutil"
	"github.com/ethereum/go-ethereum/common/math"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/crypto"

	"fmt"
	"io/ioutil"
	"math/big"
	"os"
	"runtime"
	"strings"
	"syscall"

	"gopkg.in/urfave/cli.v1"

	"golang.org/x/crypto/ssh/terminal"
)

// https://github.com/ethereum/go-ethereum/blob/55599ee95d4151a2502465e0afc7c47bd1acba77/internal/ethapi/api.go#L404
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

func rawSignHash(data []byte) []byte {
	msg := fmt.Sprintf("%s", data)
	return crypto.Keccak256([]byte(msg))
}

func getWallets(c *cli.Context) []accounts.Wallet {
	backends := []accounts.Backend{}

	path := c.String("key-store")
	ks := keystore.NewKeyStore(
		path, keystore.StandardScryptN, keystore.StandardScryptP)
	backends = append(backends, ks)

	if ledgerhub, err := usbwallet.NewLedgerHub(); err != nil {
		fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Ledgers")
	} else {
		backends = append(backends, ledgerhub)
	}
	if trezorhub, err := usbwallet.NewTrezorHubWithHID(); err != nil {
		fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Trezors (HID)")
	} else {
		backends = append(backends, trezorhub)
	}
	if trezorhub, err := usbwallet.NewTrezorHubWithWebUSB(); err != nil {
		fmt.Fprintf(os.Stderr, "ethsign: failed to look for USB Trezors (WebUSD)")
	} else {
		backends = append(backends, trezorhub)
	}

	manager := accounts.NewManager(&accounts.Config{InsecureUnlockAllowed: false}, backends...)

	return manager.Wallets()
}

func getWalletData(c *cli.Context, defaultHDPaths cli.StringSlice, from common.Address) (*accounts.Account, string, accounts.Wallet, error) {

	wallets := getWallets(c)

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
			// if hd-path is given, check that account first
			if c.String("hd-path") != "" {
				path, _ := accounts.ParseDerivationPath(c.String("hd-path"))
				y, err := x.Derive(path, true)
				if err != nil {
					return nil, "", nil, cli.NewExitError("ethsign: Ledger needs to be in Ethereum app with browser support off", 1)
				} else {
					if y.Address == from {
						wallet = x
						acct = &y
						needPassphrase = false
						break Scan
					}
				}
			}

			for i := range defaultHDPaths {
				for j := 0; j <= c.Int("n"); j++ {
					pathstr := fmt.Sprintf(defaultHDPaths[i], j)
					path, _ := accounts.ParseDerivationPath(pathstr)
					y, err := x.Derive(path, true)
					if err != nil {
						return nil, "", nil, cli.NewExitError("ethsign: Ledger needs to be in Ethereum app with browser support off", 1)
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
	}

	if acct == nil {
		return nil, "", nil, cli.NewExitError(
			"ethsign: account not found",
			1,
		)
	}

	passphrase := ""

	if needPassphrase {
		if c.String("passphrase-file") != "" {
			passphraseFile, err := ioutil.ReadFile(c.String("passphrase-file"))
			if err != nil {
				return nil, "", nil, cli.NewExitError("ethsign: failed to read passphrase file", 1)
			}

			passphrase = strings.TrimSuffix(string(passphraseFile), "\n")
		} else {
			fmt.Fprintf(os.Stderr, "Ethereum account passphrase (not echoed): ")
			bytes, err := terminal.ReadPassword(int(syscall.Stdin))
			if err != nil {
				return nil, "", nil, cli.NewExitError("ethsign: failed to read passphrase", 1)
			} else {
				passphrase = string(bytes)
			}
		}
	} else {
		fmt.Fprintf(os.Stderr, "Waiting for hardware wallet confirmation...\n")
	}

	return acct, passphrase, wallet, nil
}

// https://github.com/ethereum/go-ethereum/blob/55599ee95d4151a2502465e0afc7c47bd1acba77/internal/ethapi/api.go#L442
func recover(data []byte, sig hexutil.Bytes, noPrefix bool) (common.Address, error) {
	if len(sig) != 65 {
		return common.Address{}, fmt.Errorf("signature must be 65 bytes long")
	}
	if sig[64] != 27 && sig[64] != 28 {
		return common.Address{}, fmt.Errorf("invalid Ethereum signature (V is not 27 or 28)")
	}
	sig[64] -= 27 // Transform yellow paper V from 27/28 to 0/1

	var hash []byte
	if noPrefix == true {
		hash = rawSignHash(data)
	} else {
		hash = signHash(data)
	}

	rpk, err := crypto.SigToPub(hash, sig)
	if err != nil {
		return common.Address{}, err
	}
	return crypto.PubkeyToAddress(*rpk), nil
}

func main() {
	var defaultHDPaths cli.StringSlice
	defaultHDPaths = []string{
		"m/44'/60'/%d'/0/0", // aka "ledger live"
		"m/44'/60'/0'/%d",   // aka "ledger legacy"
	}

	var defaultKeyStore string
	if runtime.GOOS == "darwin" {
		defaultKeyStore = os.Getenv("HOME") + "/Library/Ethereum/keystore"
	} else if runtime.GOOS == "windows" {
		defaultKeyStore = os.Getenv("APPDATA") + "/Ethereum/keystore"
	} else {
		defaultKeyStore = os.Getenv("HOME") + "/.ethereum/keystore"
	}

	app := cli.NewApp()
	app.Name = "ethsign"
	app.Usage = "sign Ethereum transactions using a JSON keyfile"
	app.Version = "0.16.1"
	app.Commands = []cli.Command{
		cli.Command{
			Name:    "list-accounts",
			Aliases: []string{"ls"},
			Usage:   "list accounts in keystore and USB wallets",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:   "key-store",
					Usage:  "path to key store",
					EnvVar: "ETH_KEYSTORE",
					Value:  defaultKeyStore,
				},
				cli.StringFlag{
					Name:   "hd-path",
					Usage:  "hd derivation path",
					EnvVar: "ETH_HDPATH",
					Value:  "",
				},
				cli.IntFlag{
					Name:  "n",
					Usage: "maximum ledger index",
					Value: 5,
				},
			},
			Action: func(c *cli.Context) error {
				wallets := getWallets(c)
				for _, x := range wallets {
					if x.URL().Scheme == "keystore" {
						for _, y := range x.Accounts() {
							fmt.Printf("%s keystore\n", y.Address.Hex())
						}
					} else if x.URL().Scheme == "ledger" {
						x.Open("")
						// if hd-path is given, check that account first
						if c.String("hd-path") != "" {
							path, _ := accounts.ParseDerivationPath(c.String("hd-path"))
							z, err := x.Derive(path, false)
							if err != nil {
								return cli.NewExitError("ethsign: couldn't use Ledger: needs to be in Ethereum app with browser support off", 1)
							} else {
								fmt.Printf("%s ledger-%s\n", z.Address.Hex(), path)
							}
						}
						for i := range defaultHDPaths {
							for j := 0; j <= c.Int("n"); j++ {
								pathstr := fmt.Sprintf(defaultHDPaths[i], j)
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
				}

				return nil
			},
		},

		cli.Command{
			Name:    "transaction",
			Aliases: []string{"tx"},
			Usage:   "make a signed transaction",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:   "key-store",
					Usage:  "path to key store",
					EnvVar: "ETH_KEYSTORE",
					Value:  defaultKeyStore,
				},
				cli.StringFlag{
					Name:   "hd-path",
					Usage:  "hd derivation path",
					EnvVar: "ETH_HDPATH",
					Value:  "",
				},
				cli.IntFlag{
					Name:  "n",
					Usage: "maximum ledger index",
					Value: 5,
				},
				cli.BoolFlag{
					Name:  "create",
					Usage: "make a contract creation transaction",
				},
				cli.BoolFlag{
					Name:  "sig",
					Usage: "create the signature only",
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
					Name:  "chain-id",
					Usage: "chain ID",
				},
				cli.StringFlag{
					Name:  "to",
					Usage: "account of recipient",
				},
				cli.StringFlag{
					Name:  "nonce",
					Usage: "account nonce",
				},
				cli.StringFlag{
					Name:  "gas-price",
					Usage: "gas price",
				},
				cli.StringFlag{
					Name:  "gas-limit",
					Usage: "gas limit",
				},
				cli.StringFlag{
					Name:  "prio-fee",
					Usage: "max priority fee per gas",
				},
				cli.StringFlag{
					Name:  "value",
					Usage: "transaction value",
				},
				cli.StringFlag{
					Name:  "data",
					Usage: "hex data",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"nonce", "value", "gas-price", "gas-limit", "chain-id", "from",
				}

				for _, required := range requireds {
					if c.String(required) == "" {
						return cli.NewExitError("ethsign: missing required parameter --"+required, 1)
					}
				}

				// TODO: could support tx type 1 at some point
				txtype := types.LegacyTxType
				if c.String("prio-fee") != "" {
					txtype = types.DynamicFeeTxType
				}

				create := c.Bool("create")

				if (c.String("to") == "" && !create) || (c.String("to") != "" && create) {
					return cli.NewExitError("ethsign: need exactly one of --to or --create", 1)
				}

				if create && c.String("data") == "" {
					return cli.NewExitError("ethsign: need --data when doing --create", 1)
				}

				to := common.HexToAddress(c.String("to"))
				from := common.HexToAddress(c.String("from"))
				nonce := math.MustParseUint64(c.String("nonce"))
				gasLimit := math.MustParseUint64(c.String("gas-limit"))
				value := math.MustParseBig256(c.String("value"))
				chainID := math.MustParseBig256(c.String("chain-id"))
				gasPrice := math.MustParseBig256(c.String("gas-price"))

				var (
					prioFee *big.Int
				)

				if txtype == types.DynamicFeeTxType {
					prioFee = math.MustParseBig256(c.String("prio-fee"))
				}

				dataString := c.String("data")
				if dataString == "" {
					dataString = "0x"
				}
				data := hexutil.MustDecode(dataString)
				acct, passphrase, wallet, err := getWalletData(c, defaultHDPaths, from)
				if err != nil {
					return err
				}

				var tx *types.Transaction
				if create {
					if txtype == types.LegacyTxType {
						tx = types.NewContractCreation(nonce, value, gasLimit, gasPrice, data)
					} else if txtype == types.DynamicFeeTxType {
						tx = types.NewTx(&types.DynamicFeeTx{
							Nonce:     nonce,
							To:        nil,
							Value:     value,
							Gas:       gasLimit,
							GasFeeCap: gasPrice,
							GasTipCap: prioFee,
							Data:      data,
						})
					}
				} else {
					if txtype == types.LegacyTxType {
						tx = types.NewTransaction(nonce, to, value, gasLimit, gasPrice, data)
					} else if txtype == types.DynamicFeeTxType {
						tx = types.NewTx(&types.DynamicFeeTx{
							Nonce:     nonce,
							To:        &to,
							Value:     value,
							Gas:       gasLimit,
							GasFeeCap: gasPrice,
							GasTipCap: prioFee,
							Data:      data,
						})
					}
				}

				signed, err := wallet.SignTxWithPassphrase(*acct, passphrase, tx, chainID)
				if err != nil {
					return cli.NewExitError("ethsign: failed to sign tx", 1)
				}

				signature := c.Bool("sig")
				if signature {
					v, r, s := signed.RawSignatureValues()
					fmt.Println(fmt.Sprintf("0x%064x%064x%02x", r, s, v))
				} else {
					encoded, _ := signed.MarshalBinary()
					fmt.Println(hexutil.Encode(encoded[:]))
				}
				return nil
			},
		},

		cli.Command{
			Name:    "message",
			Aliases: []string{"msg"},
			Usage:   "sign arbitrary data with header prefix",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:   "key-store",
					Usage:  "path to key store",
					EnvVar: "ETH_KEYSTORE",
					Value:  defaultKeyStore,
				},
				cli.StringFlag{
					Name:   "hd-path",
					Usage:  "hd derivation path",
					EnvVar: "ETH_HDPATH",
					Value:  "",
				},
				cli.IntFlag{
					Name:  "n",
					Usage: "maximum ledger index",
					Value: 5,
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
				cli.BoolFlag{
					Name:  "no-prefix",
					Usage: "sign data without prefix",
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

				acct, passphrase, wallet, err := getWalletData(c, defaultHDPaths, from)
				if err != nil {
					return err
				}

				var msg []byte
				if c.Bool("no-prefix") == true {
					msg = data
				} else {
					msg = []byte(fmt.Sprintf("\x19Ethereum Signed Message:\n%d%s", len(data), data))
				}
				signature, err := wallet.SignDataWithPassphrase(*acct, passphrase, accounts.MimetypeTypedData, msg)
				if err != nil {
					return cli.NewExitError(err, 1)
				}

				signature[64] += 27 // Transform V from 0/1 to 27/28 according to the yellow paper

				fmt.Println(hexutil.Encode(signature))

				return nil
			},
		},

		cli.Command{
			Name:  "verify",
			Usage: "verify signed data by given key",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:  "from",
					Usage: "address to verify",
				},
				cli.StringFlag{
					Name:  "data",
					Usage: "hex data to verify",
				},
				cli.StringFlag{
					Name:  "sig",
					Usage: "signature",
				},
				cli.BoolFlag{
					Name:  "no-prefix",
					Usage: "sign data without prefix",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"from", "data", "sig",
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

				sigString := c.String("sig")
				if !strings.HasPrefix(sigString, "0x") {
					sigString = "0x" + sigString
				}
				sig := hexutil.MustDecode(sigString)

				recoveredAddr, err := recover(data, sig, c.Bool("no-prefix"))
				if err != nil {
					return cli.NewExitError(err, 1)
				}

				if from != recoveredAddr {
					return cli.NewExitError("ethsign: address did not match. Wanted "+from.String()+" got "+recoveredAddr.String(), 1)
				}

				return nil
			},
		},

		cli.Command{
			Name:  "recover",
			Usage: "recover ethereum address from signature",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:  "data",
					Usage: "hex data to verify",
				},
				cli.StringFlag{
					Name:  "sig",
					Usage: "signature",
				},
				cli.BoolFlag{
					Name:  "no-prefix",
					Usage: "sign data without prefix",
				},
			},
			Action: func(c *cli.Context) error {
				requireds := []string{
					"data", "sig",
				}

				for _, required := range requireds {
					if c.String(required) == "" {
						return cli.NewExitError("ethsign: missing required parameter --"+required, 1)
					}
				}

				dataString := c.String("data")
				if !strings.HasPrefix(dataString, "0x") {
					dataString = "0x" + dataString
				}
				data := hexutil.MustDecode(dataString)

				sigString := c.String("sig")
				if !strings.HasPrefix(sigString, "0x") {
					sigString = "0x" + sigString
				}
				sig := hexutil.MustDecode(sigString)

				recoveredAddr, err := recover(data, sig, c.Bool("no-prefix"))
				if err != nil {
					return cli.NewExitError(err, 1)
				}

				fmt.Println(recoveredAddr.String())

				return nil
			},
		},

		cli.Command{
			Name:  "import",
			Usage: "import hexadecimal private key into keystore",
			Flags: []cli.Flag{
				cli.StringFlag{
					Name:   "key-store",
					Usage:  "path to keystore",
					EnvVar: "ETH_KEYSTORE",
					Value:  defaultKeyStore,
				},
			},
			Action: func(c *cli.Context) error {
				ks := keystore.NewKeyStore(
					c.String("key-store"),
					keystore.StandardScryptN, keystore.StandardScryptP)

				fmt.Fprintf(os.Stderr, "Private key as 64 hexadecimal digits (not echoed): ")
				keyBytes, err := terminal.ReadPassword(int(syscall.Stdin))
				fmt.Fprintf(os.Stderr, "\n")
				if err != nil {
					return cli.NewExitError("ethsign: failed to read private key", 1)
				}

				privatekey, err := crypto.HexToECDSA(string(keyBytes))
				if err != nil {
					return cli.NewExitError("ethsign: failed to decode private key", 1)
				}

				fmt.Fprintf(os.Stderr, "Choose a passphrase (not echoed): ")
				passphraseBytes, err := terminal.ReadPassword(int(syscall.Stdin))
				fmt.Fprintf(os.Stderr, "\n")
				if err != nil {
					return cli.NewExitError("ethsign: failed to read private key", 1)
				}

				acct, err := ks.ImportECDSA(privatekey, string(passphraseBytes))
				if err != nil {
					fmt.Fprintf(os.Stderr, "keystore error: %v\n", err)
					return cli.NewExitError("ethsign: failed to import key", 1)
				}

				fmt.Printf("%x\n", acct.Address)
				return nil
			},
		},
	}

	app.Run(os.Args)
}
