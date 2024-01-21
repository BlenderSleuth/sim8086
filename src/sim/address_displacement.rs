use std::fmt::{self, Formatter};
use std::num::{NonZeroI16, NonZeroI8};

#[derive(Clone, Copy, PartialEq)]
pub enum AddressDisplacement {
    Zero,
    Byte(NonZeroI8),
    Word(NonZeroI16),
}

impl fmt::Display for AddressDisplacement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use AddressDisplacement::*;

        let displacement = match self {
            Zero => return write!(f, ""),
            Byte(displacement) => displacement.get() as i16,
            Word(displacement) => displacement.get(),
        };

        let sign = if displacement > 0 { "+" } else { "-" };
        let displacement_abs = displacement.abs();

        write!(f, "{sign} {displacement_abs}")
    }
}

impl From<i16> for AddressDisplacement {
    fn from(value: i16) -> Self {
        match NonZeroI16::new(value) {
            Some(value) => Self::Word(value),
            None => Self::Zero,
        }
    }
}

impl From<u16> for AddressDisplacement {
    fn from(value: u16) -> Self {
        (value as i16).into()
    }
}

impl From<i8> for AddressDisplacement {
    fn from(value: i8) -> Self {
        match NonZeroI8::new(value) {
            Some(value) => Self::Byte(value),
            None => Self::Zero,
        }
    }
}

impl From<u8> for AddressDisplacement {
    fn from(value: u8) -> Self {
        (value as i8).into()
    }
}

impl AddressDisplacement {
    pub fn to_signed(&self) -> i16 {
        use AddressDisplacement::*;
        match self {
            Zero => 0,
            Byte(displacement) => displacement.get() as i16,
            Word(displacement) => displacement.get(),
        }
    }

    pub fn to_unsigned(&self) -> u16 {
        use AddressDisplacement::*;
        match self {
            Zero => 0,
            Byte(displacement) => displacement.get() as u16,
            Word(displacement) => displacement.get() as u16,
        }
    }
}
