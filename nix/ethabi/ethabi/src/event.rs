//! Contract event.

use std::collections::HashMap;
use tiny_keccak::keccak256;
use signature::long_signature;
use {
	Log, Hash, RawLog, LogParam, RawTopicFilter, TopicFilter,
	Topic, ParamType, EventParam, encode, decode, Token,
	Result, ErrorKind
};

/// Contract event.
#[derive(Clone, Debug, PartialEq, Deserialize)]
pub struct Event {
	/// Event name.
	pub name: String,
	/// Event input.
	pub inputs: Vec<EventParam>,
	/// If anonymous, event cannot be found using `from` filter.
	pub anonymous: bool,
}

impl Event {
	/// Returns names of all params.
	fn params_names(&self) -> Vec<String> {
		self.inputs.iter()
			.map(|p| p.name.clone())
			.collect()
	}

	/// Returns types of all params.
	fn param_types(&self) -> Vec<ParamType> {
		self.inputs.iter()
			.map(|p| p.kind.clone())
			.collect()
	}

	/// Returns all params of the event.
	fn indexed_params(&self, indexed: bool) -> Vec<EventParam> {
		self.inputs.iter()
			.filter(|p| p.indexed == indexed)
			.cloned()
			.collect()
	}

	/// Event signature
	pub fn signature(&self) -> Hash {
		long_signature(&self.name, &self.param_types())
	}

	/// Creates topic filter
	pub fn create_filter(&self, raw: RawTopicFilter) -> Result<TopicFilter> {
		fn convert_token(token: Token, kind: &ParamType) -> Result<Hash> {
			if !token.type_check(kind) {
				return Err(ErrorKind::InvalidData.into());
			}
			let encoded = encode(&[token]);
			if encoded.len() == 32 {
				let mut data = [0u8; 32];
				data.copy_from_slice(&encoded);
				Ok(data)
			} else {
				Ok(keccak256(&encoded))
			}
		}

		fn convert_topic(topic: Topic<Token>, kind: Option<&ParamType>) -> Result<Topic<Hash>> {
			match topic {
				Topic::Any => Ok(Topic::Any),
				Topic::OneOf(tokens) => match kind {
					None => Err(ErrorKind::InvalidData.into()),
					Some(kind) => {
						let topics = tokens.into_iter()
							.map(|token| convert_token(token, kind))
							.collect::<Result<Vec<_>>>()?;
						Ok(Topic::OneOf(topics))
					}
				},
				Topic::This(token) => match kind {
					None => Err(ErrorKind::InvalidData.into()),
					Some(kind) => Ok(Topic::This(convert_token(token, kind)?)),
				}
			}
		}

		let kinds: Vec<_> = self.indexed_params(true).into_iter().map(|param| param.kind).collect();
		let result = if self.anonymous {
			TopicFilter {
				topic0: convert_topic(raw.topic0, kinds.get(0))?,
				topic1: convert_topic(raw.topic1, kinds.get(1))?,
				topic2: convert_topic(raw.topic2, kinds.get(2))?,
				topic3: Topic::Any,
			}
		} else {
			TopicFilter {
				topic0: Topic::This(self.signature()),
				topic1: convert_topic(raw.topic0, kinds.get(0))?,
				topic2: convert_topic(raw.topic1, kinds.get(1))?,
				topic3: convert_topic(raw.topic2, kinds.get(2))?,
			}
		};

		Ok(result)
	}

	/// Parses `RawLog` and retrieves all log params from it.
	pub fn parse_log(&self, log: RawLog) -> Result<Log> {
		let topics = log.topics;
		let data = log.data;
		let topics_len = topics.len();
		// obtains all params info
		let topic_params = self.indexed_params(true);
		let data_params = self.indexed_params(false);
		// then take first topic if event is not anonymous
		let to_skip = if self.anonymous {
			0
		} else {
			// verify
			let event_signature = topics.get(0).ok_or(ErrorKind::InvalidData)?;
			if event_signature != &self.signature() {
				return Err(ErrorKind::InvalidData.into());
			}
			1
		};

		let topic_types = topic_params.iter()
			.map(|p| p.kind.clone())
			.collect::<Vec<ParamType>>();

		let flat_topics = topics.into_iter()
			.skip(to_skip)
			.flat_map(|t| t.to_vec())
			.collect::<Vec<u8>>();

		let topic_tokens = try!(decode(&topic_types, &flat_topics));

		// topic may be only a 32 bytes encoded token
		if topic_tokens.len() != topics_len - to_skip {
			return Err(ErrorKind::InvalidData.into());
		}

		let topics_named_tokens = topic_params.into_iter()
			.map(|p| p.name)
			.zip(topic_tokens.into_iter());

		let data_types = data_params.iter()
			.map(|p| p.kind.clone())
			.collect::<Vec<ParamType>>();

		let data_tokens = try!(decode(&data_types, &data));

		let data_named_tokens = data_params.into_iter()
			.map(|p| p.name)
			.zip(data_tokens.into_iter());

		let named_tokens = topics_named_tokens
			.chain(data_named_tokens)
			.collect::<HashMap<String, Token>>();

		let decoded_params = self.params_names()
			.into_iter()
			.map(|name| LogParam {
				name: name.clone(),
				value: named_tokens.get(&name).unwrap().clone()
			})
			.collect();

		let result = Log {
			params: decoded_params,
		};

		Ok(result)
	}
}

#[cfg(test)]
mod tests {
	use hex::FromHex;
	use token::{Token, TokenFromHex};
	use signature::long_signature;
	use log::{RawLog, Log};
	use {EventParam, ParamType, Event, LogParam};

	#[test]
	fn test_decoding_event() {
		let event = Event {
			name: "foo".to_owned(),
			inputs: vec![EventParam {
				name: "a".to_owned(),
				kind: ParamType::Int(256),
				indexed: false,
			}, EventParam {
				name: "b".to_owned(),
				kind: ParamType::Int(256),
				indexed: true,
			}, EventParam {
				name: "c".to_owned(),
				kind: ParamType::Address,
				indexed: false,
			}, EventParam {
				name: "d".to_owned(),
				kind: ParamType::Address,
				indexed: true,
			}],
			anonymous: false,
		};

		let log = RawLog {
			topics: vec![
				long_signature("foo", &[ParamType::Int(256), ParamType::Int(256), ParamType::Address, ParamType::Address]),
				"0000000000000000000000000000000000000000000000000000000000000002".token_from_hex().unwrap(),
				"0000000000000000000000001111111111111111111111111111111111111111".token_from_hex().unwrap(),
			],
			data:
			("".to_owned() +
				"0000000000000000000000000000000000000000000000000000000000000003" +
				"0000000000000000000000002222222222222222222222222222222222222222").from_hex().unwrap()
		};
		let result = event.parse_log(log).unwrap();

		assert_eq!(result, Log { params: vec![
			("a".to_owned(), Token::Int("0000000000000000000000000000000000000000000000000000000000000003".token_from_hex().unwrap())),
			("b".to_owned(), Token::Int("0000000000000000000000000000000000000000000000000000000000000002".token_from_hex().unwrap())),
			("c".to_owned(), Token::Address("2222222222222222222222222222222222222222".token_from_hex().unwrap())),
			("d".to_owned(), Token::Address("1111111111111111111111111111111111111111".token_from_hex().unwrap())),
		].into_iter().map(|(name, value)| LogParam { name, value }).collect::<Vec<_>>()});
	}
}
