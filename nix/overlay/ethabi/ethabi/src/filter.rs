use serde::{Serialize, Serializer};
use serde_json::Value;
use hex::ToHex;
use {Hash, Token};

/// Raw topic filter.
#[derive(Debug, PartialEq, Default)]
pub struct RawTopicFilter {
	/// Topic.
	pub topic0: Topic<Token>,
	/// Topic.
	pub topic1: Topic<Token>,
	/// Topic.
	pub topic2: Topic<Token>,
}

/// Topic filter.
#[derive(Debug, PartialEq, Default)]
pub struct TopicFilter {
	/// Usually (for not-anonymous transactions) the first topic is event signature.
	pub topic0: Topic<Hash>,
	/// Second topic.
	pub topic1: Topic<Hash>,
	/// Third topic.
	pub topic2: Topic<Hash>,
	/// Fourth topic.
	pub topic3: Topic<Hash>,
}

impl Serialize for TopicFilter {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where S: Serializer {
		vec![&self.topic0, &self.topic1, &self.topic2, &self.topic3].serialize(serializer)
	}
}

/// Acceptable topic possibilities.
#[derive(Debug, PartialEq)]
pub enum Topic<T> {
	/// Match any.
	Any,
	/// Match any of the hashes.
	OneOf(Vec<T>),
	/// Match only this hash.
	This(T),
}

impl<T> Topic<T> {
	/// Map
	pub fn map<F, O>(self, f: F) -> Topic<O> where F: Fn(T) -> O {
		match self {
			Topic::Any => Topic::Any,
			Topic::OneOf(topics) => Topic::OneOf(topics.into_iter().map(f).collect()),
			Topic::This(topic) => Topic::This(f(topic)),
		}
	}
}

impl<T> Default for Topic<T> {
	fn default() -> Self {
		Topic::Any
	}
}

impl<T> From<Option<T>> for Topic<T> {
	fn from(o: Option<T>) -> Self {
		match o {
			Some(topic) => Topic::This(topic),
			None => Topic::Any,
		}
	}
}

impl<T> From<T> for Topic<T> {
	fn from(topic: T) -> Self {
		Topic::This(topic)
	}
}

impl<T> From<Vec<T>> for Topic<T> {
	fn from(topics: Vec<T>) -> Self {
		Topic::OneOf(topics)
	}
}

impl<T> Into<Vec<T>> for Topic<T> {
	fn into(self: Self) -> Vec<T> {
		match self {
			Topic::Any => vec![],
			Topic::This(topic) => vec![topic],
			Topic::OneOf(topics) => topics,
		}
	}
}

impl Serialize for Topic<Hash> {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where S: Serializer {
		let value = match *self {
			Topic::Any => Value::Null,
			Topic::OneOf(ref vec) => {
				let v = vec.iter()
					.map(|h| format!("0x{}", h.to_hex()))
					.map(Value::String)
					.collect();
				Value::Array(v)
			},
			Topic::This(ref hash) => Value::String(format!("0x{}", hash.to_hex())),
		};
		value.serialize(serializer)
	}
}

#[cfg(test)]
mod tests {
	use serde_json;
	use hex::FromHex;
	use super::{Topic, TopicFilter};
	use Hash;

	fn hash(s: &str) -> Hash {
		let v = s.from_hex().unwrap();
		let mut result = [0u8; 32];
		result.copy_from_slice(&v);
		result
	}

	#[test]
	fn test_topic_filter_serialization() {
		let expected =
r#"["0x000000000000000000000000a94f5374fce5edbc8e2a8697c15331677e6ebf0b",null,["0x000000000000000000000000a94f5374fce5edbc8e2a8697c15331677e6ebf0b","0x0000000000000000000000000aff3454fce5edbc8cca8697c15331677e6ebccc"],null]"#;

		let topic = TopicFilter {
			topic0: Topic::This(hash("000000000000000000000000a94f5374fce5edbc8e2a8697c15331677e6ebf0b")),
			topic1: Topic::Any,
			topic2: Topic::OneOf(vec![hash("000000000000000000000000a94f5374fce5edbc8e2a8697c15331677e6ebf0b"), hash("0000000000000000000000000aff3454fce5edbc8cca8697c15331677e6ebccc")]),
			topic3: Topic::Any,
		};

		let topic_str = serde_json::to_string(&topic).unwrap();
		assert_eq!(expected, &topic_str);
	}

	#[test]
	fn test_topic_from() {
		assert_eq!(Topic::Any as Topic<u64>, None.into());
		assert_eq!(Topic::This(10u64), 10u64.into());
		assert_eq!(Topic::OneOf(vec![10u64, 20]), vec![10u64, 20].into());
	}

	#[test]
	fn test_topic_into_vec() {
		let expected: Vec<u64> = vec![];
		let is: Vec<u64> = (Topic::Any as Topic<u64>).into();
		assert_eq!(expected, is);
		let expected: Vec<u64> = vec![10];
		let is: Vec<u64> = Topic::This(10u64).into();
		assert_eq!(expected, is);
		let expected: Vec<u64> = vec![10, 20];
		let is: Vec<u64> = Topic::OneOf(vec![10u64, 20]).into();
		assert_eq!(expected, is);
	}
}
