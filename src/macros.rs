#[macro_export]
macro_rules! impl_get_default {
   ($raw_parameters: ident, $parameter_type: ident;
    $($variant:pat, $name:expr, $idx:expr, $default:expr;)*) => {
        impl $raw_parameters {
            pub fn get_default(x: $parameter_type) -> f32 {
                match x {
                    $($variant => $default,)*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_display {
   ($raw_parameters: ident, $parameter_type: ident;
    $($variant:pat, $name:expr, $idx:expr, $default:expr;)*) => {
        impl std::fmt::Display for ParameterType {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($variant => write!(f, $name),)*
                }
            }
        }
    };
}

#[macro_export]
macro_rules! impl_from_i32 {
   ($raw_parameters: ident, $parameter_type: ident;
    $($variant:expr, $name:expr, $idx:expr, $default:expr;)*) => {
        impl TryFrom<i32> for ParameterType {
            type Error = ();
            fn try_from(x: i32) -> Result<Self, Self::Error> {
                match x {
                    $($idx => Ok($variant),)*
                    _ => Err(()),
                }
            }
        }
    }
}

#[macro_export]
macro_rules! impl_into_i32 {
   ($raw_parameters: ident, $parameter_type: ident;
    $($variant:pat, $name:expr, $idx:expr, $default:expr;)*) => {
        impl From<ParameterType> for i32 {
            fn from(x: ParameterType) -> i32 {
                match x {
                    $($variant => $idx,)*
                }
            }
        }
    };
}
