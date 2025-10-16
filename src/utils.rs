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
