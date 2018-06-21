//! Contract function call builder.

use signature::short_signature;
use {Param, Token, Result, ErrorKind, Bytes, decode, ParamType, encode};

/// Contract function specification.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct Function {
	/// Function name.
	pub name: String,
	/// Function input.
	pub inputs: Vec<Param>,
	/// Function output.
	pub outputs: Vec<Param>,
	/// Constant function.
	#[serde(default)]
	pub constant: bool,
}

impl Function {
	/// Returns all input params of given function.
	fn input_param_types(&self) -> Vec<ParamType> {
		self.inputs.iter()
			.map(|p| p.kind.clone())
			.collect()
	}

	/// Returns all output params of given function.
	fn output_param_types(&self) -> Vec<ParamType> {
		self.outputs.iter()
			.map(|p| p.kind.clone())
			.collect()
	}

	/// Prepares ABI function call with given input params.
	pub fn encode_input(&self, tokens: &[Token]) -> Result<Bytes> {
		let params = self.input_param_types();

		if !Token::types_check(tokens, &params) {
			return Err(ErrorKind::InvalidData.into());
		}

		let signed = short_signature(&self.name, &params).to_vec();
		let encoded = encode(tokens);
		Ok(signed.into_iter().chain(encoded.into_iter()).collect())
	}

	/// Parses the ABI function output to list of tokens.
	pub fn decode_output(&self, data: &[u8]) -> Result<Vec<Token>> {
		decode(&self.output_param_types(), &data)
	}
}

#[cfg(test)]
mod tests {
	use hex::FromHex;
	use {Token, Param, Function, ParamType};

	#[test]
	fn test_function_encode_call() {
		let interface = Function {
			name: "baz".to_owned(),
			inputs: vec![Param {
				name: "a".to_owned(),
				kind: ParamType::Uint(32),
			}, Param {
				name: "b".to_owned(),
				kind: ParamType::Bool,
			}],
			outputs: vec![],
			constant: false,
		};

		let func = Function::from(interface);
		let mut uint = [0u8; 32];
		uint[31] = 69;
		let encoded = func.encode_input(&[Token::Uint(uint), Token::Bool(true)]).unwrap();
		let expected = "cdcd77c000000000000000000000000000000000000000000000000000000000000000450000000000000000000000000000000000000000000000000000000000000001".from_hex().unwrap();
		assert_eq!(encoded, expected);
	}
}
