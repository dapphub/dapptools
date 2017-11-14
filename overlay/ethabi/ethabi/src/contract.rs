use std::{io, fmt};
use std::collections::HashMap;
use std::collections::hash_map::Values;
use serde::{Deserialize, Deserializer};
use serde::de::{Visitor, SeqAccess};
use serde_json;
use operation::Operation;
use {errors, ErrorKind, Event, Constructor, Function};

/// API building calls to contracts ABI.
#[derive(Clone, Debug, PartialEq)]
pub struct Contract {
	/// Contract constructor.
	pub constructor: Option<Constructor>,
	/// Contract functions.
	pub functions: HashMap<String, Function>,
	/// Contract events.
	pub events: HashMap<String, Event>,
	/// Contract has fallback function.
	pub fallback: bool,
}

impl<'a> Deserialize<'a> for Contract {
	fn deserialize<D>(deserializer: D) -> Result<Contract, D::Error> where D: Deserializer<'a> {
		deserializer.deserialize_any(ContractVisitor)
	}
}

struct ContractVisitor;

impl<'a> Visitor<'a> for ContractVisitor {
	type Value = Contract;

	fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
		formatter.write_str("valid abi spec file")
	}

	fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error> where A: SeqAccess<'a> {
		let mut result = Contract {
			constructor: None,
			functions: HashMap::default(),
			events: HashMap::default(),
			fallback: false,
		};

		while let Some(operation) = seq.next_element()? {
			match operation {
				Operation::Constructor(constructor) => {
					result.constructor = Some(constructor);
				},
				Operation::Function(func) => {
					result.functions.insert(func.name.clone(), func);
				},
				Operation::Event(event) => {
					result.events.insert(event.name.clone(), event);
				},
				Operation::Fallback => {
					result.fallback = true;
				},
			}
		}

		Ok(result)
	}
}

impl Contract {
	/// Loads contract from json.
	pub fn load<T: io::Read>(reader: T) -> errors::Result<Self> {
		serde_json::from_reader(reader).map_err(From::from)
	}

	/// Creates constructor call builder.
	pub fn constructor(&self) -> Option<&Constructor> {
		self.constructor.as_ref()
	}

	/// Creates function call builder.
	pub fn function(&self, name: &str) -> errors::Result<&Function> {
		self.functions.get(name).ok_or_else(|| ErrorKind::InvalidName(name.to_owned()).into())
	}

	/// Creates event decoder.
	pub fn event(&self, name: &str) -> errors::Result<&Event> {
		self.events.get(name).ok_or_else(|| ErrorKind::InvalidName(name.to_owned()).into())
	}

	/// Iterate over all functions of the contract in arbitrary order.
	pub fn functions(&self) -> Functions {
		Functions(self.functions.values())
	}

	/// Iterate over all events of the contract in arbitrary order.
	pub fn events(&self) -> Events {
		Events(self.events.values())
	}

	/// Returns true if contract has fallback
	pub fn fallback(&self) -> bool {
		self.fallback
	}
}

/// Contract functions interator.
pub struct Functions<'a>(Values<'a, String, Function>);

impl<'a> Iterator for Functions<'a> {
	type Item = &'a Function;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next()
	}
}

/// Contract events interator.
pub struct Events<'a>(Values<'a, String, Event>);

impl<'a> Iterator for Events<'a> {
	type Item = &'a Event;

	fn next(&mut self) -> Option<Self::Item> {
		self.0.next()
	}
}
