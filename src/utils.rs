use std::ops::Deref;

use rsleigh::{SleighCtx, Vn};

/// creates an array vec containing the arguments.
#[macro_export]
macro_rules! array_vec {
    () => (
        ::arrayvec::ArrayVec::new()
    );
    ($($x:expr),+ $(,)?) => (
        {
            let mut result = ::arrayvec::ArrayVec::new();
            $(
                result.push($x);
            )+
            result
        }
    );
}

pub enum CowBox<'a, T: ?Sized> {
    Borrowed(&'a T),
    Owned(Box<T>),
}
impl<'a, T: ?Sized> Deref for CowBox<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            CowBox::Borrowed(borrowed) => borrowed,
            CowBox::Owned(owned) => owned,
        }
    }
}
impl<'a, T: ?Sized> AsRef<T> for CowBox<'a, T> {
    fn as_ref(&self) -> &T {
        match self {
            CowBox::Borrowed(borrowed) => borrowed,
            CowBox::Owned(owned) => owned,
        }
    }
}

pub fn display_vn(vn: Vn, sleigh_ctx: &SleighCtx) -> String {
    // if the varnode is a register, try to use the register name when displaying it
    if let Some(reg_name) = sleigh_ctx.reg_to_name(vn) {
        return reg_name.into();
    }

    let space_info = sleigh_ctx.space_info(vn.addr.space);
    format!(
        "{}[{}]:u{}",
        space_info.name().unwrap(),
        vn.addr.off,
        vn.size * 8
    )
}
