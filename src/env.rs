use std::env::{var, VarError};
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;

/// Parse the specified environment variable as a flag if present, panicking if it's invalid
pub fn environment_flag(variable: &str) -> Option<bool> {
    match try_environment_config::<EnvironmentFlag>(variable) {
        Ok(result) => Some(result.0),
        Err(ref err) if err.is_missing() => None,
        Err(err) => panic!("{}", err)
    }
}
/// Parse the specified environment variable if it's present, panicking if it's invalid
#[inline]
pub fn environment_config<T: FromStr>(variable: &str) -> Option<T> where T::Err: Display {
    match try_environment_config(variable) {
        Ok(result) => Some(result),
        Err(ref err) if err.is_missing() => None,
        Err(err) => panic!("{}", err)
    }
}
#[inline]
pub fn try_environment_config<T: FromStr>(name: &str) -> Result<T, EnvironmentError<T::Err>> {
    match var(name) {
        Ok(result) => {
            result.parse().map_err(|cause| EnvironmentError {
                variable: name.to_owned(),
                kind: EnvironmentErrorKind::ParseError(cause)
            })
        },
        Err(VarError::NotPresent) => Err(EnvironmentError {
            variable: name.to_owned(),
            kind: EnvironmentErrorKind::MissingVariable
        }),
        Err(VarError::NotUnicode(_)) => Err(EnvironmentError {
            variable: name.to_owned(),
            kind: EnvironmentErrorKind::InvalidUtf8
        })
    }
}
pub struct EnvironmentError<T> {
    pub variable: String,
    pub kind: EnvironmentErrorKind<T>
}
impl<T> EnvironmentError<T> {
    #[inline]
    pub fn is_missing(&self) -> bool {
        if let EnvironmentErrorKind::MissingVariable = self.kind {
            true
        } else {
            false
        }
    }
}
pub enum EnvironmentErrorKind<T> {
    MissingVariable,
    ParseError(T),
    InvalidUtf8
}
impl<T: Display> Display for EnvironmentError<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.kind {
            EnvironmentErrorKind::MissingVariable => write!(f, "Missing variable: {}", self.variable),
            EnvironmentErrorKind::ParseError(ref cause) => write!(f, "Invalid {}: {}", self.variable, cause),
            EnvironmentErrorKind::InvalidUtf8 => write!(f, "Invalid UTF8 in variable {}", self.variable)
        }
    }
}
struct EnvironmentFlag(pub bool);
impl FromStr for EnvironmentFlag {
    type Err = InvalidEnvironmentFlag;

    #[inline]
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "yes" | "on" | "1" => Ok(EnvironmentFlag(true)),
            "no" | "off" | "0" => Ok(EnvironmentFlag(false)),
            _ => Err(InvalidEnvironmentFlag(s.to_owned()))
        }
    }
}
struct InvalidEnvironmentFlag(String);
impl Display for InvalidEnvironmentFlag {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Invalid flag {:?}, must be either ('yes', 'no', 'on', 'off', '1', or '0').", self.0)
    }
}