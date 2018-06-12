#![allow(unknown_lints)]

use std::io;
use {ethabi, docopt, hex};

error_chain! {
	links {
		Ethabi(ethabi::Error, ethabi::ErrorKind);
	}

	foreign_links {
		Io(io::Error);
		Docopt(docopt::Error);
		Hex(hex::FromHexError);
	}
}
