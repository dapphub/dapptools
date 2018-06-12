extern crate docopt;
extern crate rustc_hex as hex;
extern crate serde;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate error_chain;
extern crate ethabi;

mod error;

use std::fs::File;
use std::env;
use docopt::Docopt;
use hex::{ToHex, FromHex};
use ethabi::param_type::{ParamType, Reader};
use ethabi::token::{Token, Tokenizer, StrictTokenizer, LenientTokenizer, TokenFromHex};
use ethabi::{encode, decode, Contract, Function, Event};
use error::{Error, ResultExt};

pub const ETHABI: &'static str = r#"
Ethereum ABI coder.
  Copyright 2016-2017 Parity Technologies (UK) Limited

Usage:
    ethabi encode function <abi-path> <function-name> [-p <param>]... [-l | --lenient]
    ethabi encode params [-v <type> <param>]... [-l | --lenient]
    ethabi decode function <abi-path> <function-name> <data>
    ethabi decode params [-t <type>]... <data>
    ethabi decode log <abi-path> <event-name> [-l <topic>]... <data>
    ethabi -h | --help

Options:
    -h, --help         Display this message and exit.
    -l, --lenient      Allow short representation of input params.

Commands:
    encode             Encode ABI call.
    decode             Decode ABI call result.
    function           Load function from json ABI file.
    params             Specify types of input params inline.
    log                Decode event log.
"#;

#[derive(Debug, Deserialize)]
struct Args {
	cmd_encode: bool,
	cmd_decode: bool,
	cmd_function: bool,
	cmd_params: bool,
	cmd_log: bool,
	arg_abi_path: String,
	arg_function_name: String,
	arg_event_name: String,
	arg_param: Vec<String>,
	arg_type: Vec<String>,
	arg_data: String,
	arg_topic: Vec<String>,
	flag_lenient: bool,
}

fn main() {
	let result = execute(env::args());

	match result {
		Ok(s) => println!("{}", s),
		Err(error) => print_err(error),
	}
}

fn print_err(err: Error) {
	let message = err.iter()
		.map(|e| e.to_string())
		.filter(|e| !e.is_empty())
		.collect::<Vec<_>>().join("\n\nCaused by:\n  ");
	println!("{}", message);
}

fn execute<S, I>(command: I) -> Result<String, Error> where I: IntoIterator<Item=S>, S: AsRef<str> {
	let args: Args = Docopt::new(ETHABI)
		.and_then(|d| d.argv(command).deserialize())?;

	if args.cmd_encode && args.cmd_function {
		encode_input(&args.arg_abi_path, &args.arg_function_name, &args.arg_param, args.flag_lenient)
	} else if args.cmd_encode && args.cmd_params {
		encode_params(&args.arg_type, &args.arg_param, args.flag_lenient)
	} else if args.cmd_decode && args.cmd_function {
		decode_call_output(&args.arg_abi_path, &args.arg_function_name, &args.arg_data)
	} else if args.cmd_decode && args.cmd_params {
		decode_params(&args.arg_type, &args.arg_data)
	} else if args.cmd_decode && args.cmd_log {
		decode_log(&args.arg_abi_path, &args.arg_event_name, &args.arg_topic, &args.arg_data)
	} else {
		unreachable!()
	}
}

fn load_function(path: &str, function: &str) -> Result<Function, Error> {
	let file = File::open(path)?;
	let contract = Contract::load(file)?;
	let function = contract.function(function)?.clone();
	Ok(function)
}

fn load_event(path: &str, event: &str) -> Result<Event, Error> {
	let file = File::open(path)?;
	let contract = Contract::load(file)?;
	let event = contract.event(event)?.clone();
	Ok(event)
}

fn parse_tokens(params: &[(ParamType, &str)], lenient: bool) -> Result<Vec<Token>, Error> {
	params.iter()
		.map(|&(ref param, value)| match lenient {
			true => LenientTokenizer::tokenize(param, value),
			false => StrictTokenizer::tokenize(param, value)
		})
		.collect::<Result<_, _>>()
		.map_err(From::from)
}

fn encode_input(path: &str, function: &str, values: &[String], lenient: bool) -> Result<String, Error> {
	let function = load_function(path, function)?;

	let params: Vec<_> = function.inputs.iter()
		.map(|param| param.kind.clone())
		.zip(values.iter().map(|v| v as &str))
		.collect();

	let tokens = parse_tokens(&params, lenient)?;
	let result = function.encode_input(&tokens)?;

	Ok(result.to_hex())
}

fn encode_params(types: &[String], values: &[String], lenient: bool) -> Result<String, Error> {
	assert_eq!(types.len(), values.len());

	let types: Vec<ParamType> = types.iter()
		.map(|s| Reader::read(s))
		.collect::<Result<_, _>>()?;

	let params: Vec<_> = types.into_iter()
		.zip(values.iter().map(|v| v as &str))
		.collect();

	let tokens = parse_tokens(&params, lenient)?;
	let result = encode(&tokens);

	Ok(result.to_hex())
}

fn decode_call_output(path: &str, function: &str, data: &str) -> Result<String, Error> {
	let function = load_function(path, function)?;
	let data = data.from_hex().chain_err(|| "Expected <data> to be hex")?;
	let tokens = function.decode_output(&data)?;
	let types = function.outputs;

	assert_eq!(types.len(), tokens.len());

	let result = types.iter()
		.zip(tokens.iter())
		.map(|(ty, to)| format!("{} {}", ty.kind, to))
		.collect::<Vec<String>>()
		.join("\n");

	Ok(result)
}

fn decode_params(types: &[String], data: &str) -> Result<String, Error> {
	let types: Vec<ParamType> = types.iter()
		.map(|s| Reader::read(s))
		.collect::<Result<_, _>>()?;

	let data = data.from_hex().chain_err(|| "Expected <data> to be hex")?;

	let tokens = decode(&types, &data)?;

	assert_eq!(types.len(), tokens.len());

	let result = types.iter()
		.zip(tokens.iter())
		.map(|(ty, to)| format!("{} {}", ty, to))
		.collect::<Vec<String>>()
		.join("\n");

	Ok(result)
}

fn decode_log(path: &str, event: &str, topics: &[String], data: &str) -> Result<String, Error> {
	let event = load_event(path, event)?;
	let topics: Vec<[u8; 32]> = topics.into_iter()
		.map(|t| t.token_from_hex().map_err(From::from))
		.collect::<Result<_, Error>>()?;
	let data = data.from_hex().chain_err(|| "Expected <data> to be hex")?;
	let decoded = event.parse_log((topics, data).into())?;

	let result = decoded.params.into_iter()
		.map(|log_param| format!("{} {}", log_param.name, log_param.value))
		.collect::<Vec<String>>()
		.join("\n");

	Ok(result)
}

#[cfg(test)]
mod tests {
	use super::execute;

	#[test]
	fn simple_encode() {
		let command = "ethabi encode params -v bool 1".split(" ");
		let expected = "0000000000000000000000000000000000000000000000000000000000000001";
		assert_eq!(execute(command).unwrap(), expected);
	}

	// TODO: parsing negative values is not working
	#[test]
	#[ignore]
	fn int_encode() {
		let command = "ethabi encode paramas -v int256 -2 --lenient".split(" ");
		let expected = "fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn multi_encode() {
		let command = "ethabi encode params -v bool 1 -v string gavofyork -v bool 0".split(" ");
		let expected = "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000096761766f66796f726b0000000000000000000000000000000000000000000000";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn array_encode() {
		let command = "ethabi encode params -v bool[] [1,0,false]".split(" ");
		let expected = "00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn abi_encode() {
		let command = "ethabi encode function ../res/test.abi foo -p 1".split(" ");
		let expected = "455575780000000000000000000000000000000000000000000000000000000000000001";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn simple_decode() {
		let command = "ethabi decode params -t bool 0000000000000000000000000000000000000000000000000000000000000001".split(" ");
		let expected = "bool true";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn int_decode() {
		let command = "ethabi decode params -t int256 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe".split(" ");
		let expected = "int256 fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn multi_decode() {
		let command = "ethabi decode params -t bool -t string -t bool 00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000096761766f66796f726b0000000000000000000000000000000000000000000000".split(" ");
		let expected =
"bool true
string gavofyork
bool false";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn array_decode() {
		let command = "ethabi decode params -t bool[] 00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000003000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000".split(" ");
		let expected = "bool[] [true,false,false]";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn abi_decode() {
		let command = "ethabi decode function ../res/foo.abi bar 0000000000000000000000000000000000000000000000000000000000000001".split(" ");
		let expected = "bool true";
		assert_eq!(execute(command).unwrap(), expected);
	}

	#[test]
	fn log_decode() {
		let command = "ethabi decode log ../res/event.abi Event -l 0000000000000000000000000000000000000000000000000000000000000001 0000000000000000000000004444444444444444444444444444444444444444".split(" ");
		let expected =
"a true
b 4444444444444444444444444444444444444444";
		assert_eq!(execute(command).unwrap(), expected);
	}
}
