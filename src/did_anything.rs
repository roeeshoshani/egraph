use std::ops::{BitOr, BitOrAssign, Not};

/// an enum which represents whether an operation actually did anything.
///
/// this is useful for cases where we want to continue iterating as long as we did anything.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum DidAnything {
    /// yes, we did anything.
    True,

    /// no, we didn't do anything.
    False,
}

impl From<bool> for DidAnything {
    fn from(value: bool) -> Self {
        if value { Self::True } else { Self::False }
    }
}

impl From<DidAnything> for bool {
    fn from(value: DidAnything) -> Self {
        value == DidAnything::True
    }
}

impl From<&DidAnything> for bool {
    fn from(value: &DidAnything) -> Self {
        *value == DidAnything::True
    }
}

impl DidAnything {
    /// converts this "did anything" value to a boolean
    pub fn as_bool(&self) -> bool {
        self.into()
    }
}

impl Not for DidAnything {
    type Output = Self;

    fn not(self) -> Self::Output {
        (!self.as_bool()).into()
    }
}

impl BitOrAssign for DidAnything {
    fn bitor_assign(&mut self, rhs: Self) {
        if rhs.into() {
            *self = Self::True;
        }
    }
}

impl BitOr for DidAnything {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        (self.as_bool() && rhs.as_bool()).into()
    }
}
