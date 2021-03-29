#[macro_export]
macro_rules! impl_display {
     ($($variant:pat, $idx:expr, $name:expr;)*) => {
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
    ($($variant:expr, $idx:expr, $_:expr;)*) => {
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
    ($($variant:pat, $idx:expr, $_:expr;)*) => {
        impl From<ParameterType> for i32 {
            fn from(x: ParameterType) -> i32 {
                match x {
                    $($variant => $idx,)*
                }
            }
        }
    };
}
