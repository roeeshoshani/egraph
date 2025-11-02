use egraph::node::{ValueSize, Vn};
use rsleigh::{VnAddr, VnSpace};

/// a convenience function for creating a sample varnode for testing purposes.
pub fn vn(index: u64) -> Vn {
    Vn {
        size: ValueSize::U64,
        addr: VnAddr {
            off: index * 8,
            space: VnSpace::REGISTER,
        },
    }
}
