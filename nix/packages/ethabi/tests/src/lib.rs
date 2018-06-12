#![deny(warnings)]

extern crate rustc_hex;
extern crate ethabi;
#[macro_use]
extern crate ethabi_derive;
#[macro_use]
extern crate ethabi_contract;

use_contract!(eip20, "Eip20", "../res/eip20.abi");
use_contract!(constructor, "Constructor", "../res/con.abi");
use_contract!(validators, "Validators", "../res/Validators.abi");

#[cfg(test)]
mod tests {
	use rustc_hex::{ToHex, FromHex};

	struct Wrapper([u8; 20]);

	impl Into<[u8; 20]> for Wrapper {
		fn into(self) -> [u8; 20] {
			self.0
		}
	}

	#[test]
	fn test_encoding_function_input_as_array() {
		use validators::Validators;

		let contract = Validators::default();
		let first = [0x11u8; 20];
		let second = [0x22u8; 20];

		let functions = contract.functions();
		let set_validators = functions.set_validators();

		let encoded_from_vec = set_validators.input(vec![first.clone(), second.clone()]);
		let encoded_from_vec_iter = set_validators.input(vec![first.clone(), second.clone()].into_iter());
		let encoded_from_vec_wrapped = set_validators.input(vec![Wrapper(first), Wrapper(second)]);

		let expected = "9300c9260000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000200000000000000000000000011111111111111111111111111111111111111110000000000000000000000002222222222222222222222222222222222222222".to_owned();
		assert_eq!(expected, encoded_from_vec.to_hex());
		assert_eq!(expected, encoded_from_vec_iter.to_hex());
		assert_eq!(expected, encoded_from_vec_wrapped.to_hex());
	}

	#[test]
	fn test_decoding_function_output() {
		// Make sure that the output param type of the derived contract is correct

		use eip20::Eip20;

		let contract = Eip20::default();
		let output = "000000000000000000000000000000000000000000000000000000000036455B".from_hex().unwrap();
		let decoded_output = contract.functions().total_supply().output(&output).unwrap();
		let expected_output = output.clone();
		assert_eq!(expected_output, decoded_output);
	}

	#[test]
	fn test_encoding_constructor_as_array() {
		use validators::Validators;

		let contract = Validators::default();
		let code = Vec::new();
		let first = [0x11u8; 20];
		let second = [0x22u8; 20];

		let encoded_from_vec = contract.constructor(code.clone(), vec![first.clone(), second.clone()]);
		let encoded_from_vec_iter = contract.constructor(code.clone(), vec![first.clone(), second.clone()].into_iter());
		let encoded_from_vec_wrapped = contract.constructor(code.clone(), vec![Wrapper(first), Wrapper(second)]);

		let expected = "0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000200000000000000000000000011111111111111111111111111111111111111110000000000000000000000002222222222222222222222222222222222222222".to_owned();
		assert_eq!(expected, encoded_from_vec.to_hex());
		assert_eq!(expected, encoded_from_vec_iter.to_hex());
		assert_eq!(expected, encoded_from_vec_wrapped.to_hex());
	}

	#[test]
	fn test_encoding_function_input_as_fixed_array() {
		use validators::Validators;

		let contract = Validators::default();
		let first = [0x11u8; 20];
		let second = [0x22u8; 20];

		let functions = contract.functions();
		let add_validators = functions.add_two_validators();

		let encoded_from_array = add_validators.input([first.clone(), second.clone()]);
		let encoded_from_array_wrapped = add_validators.input([Wrapper(first), Wrapper(second)]);

		let expected = "7de33d2000000000000000000000000011111111111111111111111111111111111111110000000000000000000000002222222222222222222222222222222222222222".to_owned();
		assert_eq!(expected, encoded_from_array.to_hex());
		assert_eq!(expected, encoded_from_array_wrapped.to_hex());
	}

	#[test]
	fn encoding_input_works() {
		use eip20::Eip20;

		let expected = "dd62ed3e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101010101010101010101010101010101010101".to_owned();
		let contract = Eip20::default();
		let owner = [0u8; 20];
		let spender = [1u8; 20];
		let encoded = contract.functions().allowance().input(owner, spender);
		// 4 bytes signature + 2 * 32 bytes for params
		assert_eq!(encoded.to_hex(), expected);

		let from = [2u8; 20];
		let to = [3u8; 20];
		let to2 = [4u8; 20];
		let _filter = contract.events().transfer().create_filter(from, vec![to, to2]);
		let _filter = contract.events().transfer().create_filter(None, None);
	}

	#[test]
	fn test_calling_function() {
		use eip20::Eip20;

		let contract = Eip20::default();
		let address_param = [0u8; 20];
		let result = contract.functions().balance_of().call(address_param, &|data| {
			assert_eq!(data, "70a082310000000000000000000000000000000000000000000000000000000000000000".from_hex().unwrap());
			Ok("000000000000000000000000000000000000000000000000000000000036455b".from_hex().unwrap())
        });
		assert_eq!(result.unwrap().to_hex(), "000000000000000000000000000000000000000000000000000000000036455b");
	}

}

