#[macro_export]
macro_rules! use_contract {
	($module: ident, $name: expr, $path: expr) => {
		#[allow(dead_code)]
		pub mod $module {
			#[derive(EthabiContract)]
			#[ethabi_contract_options(name = $name, path = $path)]
			struct _Dummy;
		}
	}
}
