use tiny_keccak::Keccak;
use param_type::{Writer, ParamType};

pub fn short_signature(name: &str, params: &[ParamType]) -> [u8; 4] {
	let mut result = [0u8; 4];
	fill_signature(name, params, &mut result);
	result
}

pub fn long_signature(name: &str, params: &[ParamType]) -> [u8; 32] {
	let mut result = [0u8; 32];
	fill_signature(name, params, &mut result);
	result
}

fn fill_signature(name: &str, params: &[ParamType], result: &mut [u8]) {
	let types = params.iter()
		.map(Writer::write)
		.collect::<Vec<String>>()
		.join(",");

	let data: Vec<u8> = From::from(format!("{}({})", name, types).as_str());

	let mut sponge = Keccak::new_keccak256();
	sponge.update(&data);
	sponge.finalize(result);
}

#[cfg(test)]
mod tests {
	use hex::FromHex;
	use super::short_signature;
	use {ParamType};

	#[test]
	fn test_signature() {
		assert_eq!("cdcd77c0".from_hex().unwrap(), short_signature("baz", &[ParamType::Uint(32), ParamType::Bool]));
	}
}
