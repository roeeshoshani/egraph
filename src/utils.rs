use std::ops::Deref;

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
