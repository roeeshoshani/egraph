use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;
use enum_variant_accessors::{EnumAsVariant, EnumIsVariant};
use rsleigh::{Vn, VnSpace};

use crate::array_vec;

/// the max amount of fixed links in a node.
pub const NODE_MAX_FIXED_LINKS: usize = 3;

/// an array of all links of a node.
#[derive(Debug)]
pub struct NodeLinks<'a, L> {
    pub fixed: ArrayVec<&'a L, NODE_MAX_FIXED_LINKS>,
    pub dynamic: &'a [L],
}
impl<'a, L> NodeLinks<'a, L> {
    pub fn len(&self) -> usize {
        self.fixed.len() + self.dynamic.len()
    }

    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.dynamic.is_empty()
    }
    pub fn iter(&self) -> impl Iterator<Item = &'a L> {
        self.fixed.iter().copied().chain(self.dynamic.iter())
    }
}
impl<'a, L> std::ops::Index<usize> for NodeLinks<'a, L> {
    type Output = L;

    fn index(&self, index: usize) -> &Self::Output {
        if let Some(dynamic_index) = index.checked_sub(self.fixed.len()) {
            &self.dynamic[dynamic_index]
        } else {
            &self.fixed[index]
        }
    }
}
impl<'a, L> std::ops::Add for NodeLinks<'a, L> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let dynamic = match (self.dynamic.is_empty(), rhs.dynamic.is_empty()) {
            (true, true) => {
                // both are empty, so keep it empty
                &[]
            }
            (true, false) => {
                // use the rhs
                rhs.dynamic
            }
            (false, true) => {
                // use the lhs
                rhs.dynamic
            }
            (false, false) => {
                // both of them already have a dynamic part, so we can't combine them
                panic!("attempted to add two node links which both already have a dynamic part");
            }
        };

        // combine the fixed parts
        let mut combined_fixed = self.fixed;
        combined_fixed
            .try_extend_from_slice(&rhs.fixed[..])
            .unwrap();

        Self {
            fixed: combined_fixed,
            dynamic,
        }
    }
}

/// an immediate value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Imm(pub u64);

/// an internal variable. exists to serve as placeholder for nodes who don't have a proper value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct InternalVar(pub u64);

/// the kind of a binary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum BinOpKind {
    #[display("+")]
    Add,
    #[display("*")]
    Mul,
    #[display("&")]
    BitAnd,
    #[display("|")]
    BitOr,
}

impl BinOpKind {
    /// applies this binary operation to the given immediates.
    pub fn apply_to_imms(&self, lhs: Imm, rhs: Imm) -> Imm {
        let res = match self {
            BinOpKind::Add => lhs.0.wrapping_add(rhs.0),
            BinOpKind::Mul => lhs.0.wrapping_mul(rhs.0),
            BinOpKind::BitAnd => lhs.0 & rhs.0,
            BinOpKind::BitOr => lhs.0 | rhs.0,
        };
        Imm(res)
    }
}

/// a binary operation. this is basically an operation with 2 operands, a lhs operand and a rhs operand.
/// an example of a binary operation is an addition operation (`x + y`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BinOp<L> {
    /// the kind of the binary operation (e.g addition, multiplication, etc)
    pub kind: BinOpKind,

    /// the lhs operand.
    pub lhs: L,

    /// the rhs operand.
    pub rhs: L,
}

impl<L> BinOp<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> BinOp<L2>
    where
        F: FnMut(&L) -> L2,
    {
        BinOp {
            kind: self.kind,
            lhs: conversion(&self.lhs),
            rhs: conversion(&self.rhs),
        }
    }
}

/// the kind of a unary operation.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, EnumDisplay)]
pub enum UnOpKind {
    /// integer arithmetic negation (as in `-x`). this is also called the 2s complement of the integer.
    #[display("-")]
    Neg,

    /// integer not operation, which basically means inverting all the bits of the integer.
    #[display("!")]
    BitNot,
}
impl UnOpKind {
    /// applies this unary operation to the given immediate.
    pub fn apply_to_imm(&self, operand: Imm) -> Imm {
        let res = match self {
            UnOpKind::Neg => operand.0.wrapping_neg(),
            UnOpKind::BitNot => !operand.0,
        };
        Imm(res)
    }
}

/// a unary operation. this is basically an operation with only a single operand.
/// an example of a unary operation is a negation operation (`-x`).
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct UnOp<L> {
    /// the kind of the unary operation (e.g negation, bitwise not, etc)
    pub kind: UnOpKind,

    /// the operand to which the operation is applied.
    pub operand: L,
}

impl<L> UnOp<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> UnOp<L2>
    where
        F: FnMut(&L) -> L2,
    {
        UnOp {
            kind: self.kind,
            operand: conversion(&self.operand),
        }
    }
}

/// a node which represents an extension calculation, which increases the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExtNode<L> {
    pub new_size: u32,
    pub kind: ExtKind,
    pub operand: L,
}

impl<L> ExtNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExtNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExtNode {
            new_size: self.new_size,
            kind: self.kind,
            operand: conversion(&self.operand),
        }
    }
}

/// a node which represents a truncation calculation, which shortens the bit length of a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TruncNode<L> {
    pub new_size: u32,
    pub operand: L,
}

impl<L> TruncNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> TruncNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        TruncNode {
            new_size: self.new_size,
            operand: conversion(&self.operand),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtKind {
    Zero,
    Sign,
}

impl ExtKind {
    /// returns a letter which represents the kind of extension
    pub fn letter(&self) -> char {
        match self {
            ExtKind::Zero => 'Z',
            ExtKind::Sign => 'S',
        }
    }
}

/// a node which represents a memory load operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LoadNode<L> {
    /// the address space to load from
    pub space: VnSpace,

    /// the size of the loaded value
    pub size: u32,

    /// the address to load from
    pub address: L,

    pub cf_input: L,
}

impl<L> LoadNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> LoadNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        LoadNode {
            space: self.space,
            size: self.size,
            address: conversion(&self.address),
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// a node which represents a memory store operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StoreNode<L> {
    /// the address space to store to
    pub space: VnSpace,

    /// the address to store into
    pub address: L,

    /// the value to store
    pub value: L,

    pub cf_input: L,
}

impl<L> StoreNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> StoreNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        StoreNode {
            space: self.space,
            address: conversion(&self.address),
            value: conversion(&self.value),
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// the final value of a vn at the exit of control flow.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExitVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub cf_input: L,
}

impl<L> ExitVnNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExitVnNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExitVnNode {
            vn: self.vn,
            value: conversion(&self.value),
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// the address of a machine insn
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MachineInsnAddr {
    /// the offset, inside the default code address space, of the machine insn
    pub code_space_off: u64,
}
impl std::ops::Add<u64> for MachineInsnAddr {
    type Output = Self;

    fn add(self, rhs: u64) -> Self::Output {
        Self {
            code_space_off: self.code_space_off + rhs,
        }
    }
}

/// the kind of a call node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CallNodeKind<L> {
    /// a direct call, which is a call to a function with a statically known address.
    Direct(MachineInsnAddr),

    /// an indirect call, which is a call to a function whose address is determined at runtime.
    Indirect(L),
}

impl<L> CallNodeKind<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNodeKind<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            CallNodeKind::Direct(addr) => CallNodeKind::Direct(*addr),
            CallNodeKind::Indirect(l) => CallNodeKind::Indirect(conversion(l)),
        }
    }
}

/// a node which represents a call to another function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallNode<L> {
    pub cf_input: L,
    pub kind: CallNodeKind<L>,
}

impl<L> CallNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        CallNode {
            cf_input: conversion(&self.cf_input),
            kind: self.kind.convert_links(|x| conversion(x)),
        }
    }
}

/// the call target of a call node
pub enum CallNodeTarget<L> {
    Direct(MachineInsnAddr),
    Indirect(L),
}

impl<L> CallNodeTarget<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallNodeTarget<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            CallNodeTarget::Direct(addr) => CallNodeTarget::Direct(*addr),
            CallNodeTarget::Indirect(l) => CallNodeTarget::Indirect(conversion(l)),
        }
    }
}

/// the value of a vn when calling a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallVnNode<L> {
    pub vn: Vn,
    pub value: L,
    pub call_node: L,
}

impl<L> CallVnNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> CallVnNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        CallVnNode {
            vn: self.vn,
            value: conversion(&self.value),
            call_node: conversion(&self.call_node),
        }
    }
}

/// a value which may be clobbered by a function call.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClobberNode<L> {
    pub vn: Vn,
    pub original_vn_value: L,
    pub call_node: L,
}

impl<L> ClobberNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ClobberNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ClobberNode {
            vn: self.vn,
            original_vn_value: conversion(&self.original_vn_value),
            call_node: conversion(&self.call_node),
        }
    }
}

/// a conditional if operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IfNode<L> {
    pub cond: L,
    pub cf_input: L,
}

impl<L> IfNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> IfNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        IfNode {
            cond: conversion(&self.cond),
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// one of the cases of an if condition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IfCase {
    /// the "true" case of an if operation.
    True,

    /// the "false" case of an if operation.
    False,
}

/// a node to represent one of the cases of an if condition
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IfCaseNode<L> {
    pub case: IfCase,
    pub cf_input: L,
}

impl<L> IfCaseNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> IfCaseNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        IfCaseNode {
            case: self.case,
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// the operation number of a sleigh callother operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallOtherOpNum(pub u64);

/// a phi node which merges multiple values coming from different paths in the code.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PhiNode<L> {
    pub inputs: Vec<L>,
    pub region_node: L,
}

impl<L> PhiNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> PhiNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        PhiNode {
            inputs: self.inputs.iter().map(|l| conversion(l)).collect(),
            region_node: conversion(&self.region_node),
        }
    }
}

/// a region node which merges multiple control flow paths.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegionNode<L> {
    pub cf_inputs: Vec<L>,
}

impl<L> RegionNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> RegionNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        RegionNode {
            cf_inputs: self.cf_inputs.iter().map(|l| conversion(l)).collect(),
        }
    }
}

/// an exit of control flow.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExitNode<L> {
    pub cf_input: L,
}

impl<L> ExitNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> ExitNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        ExitNode {
            cf_input: conversion(&self.cf_input),
        }
    }
}

/// a node type that is generic over the link type. the link type determines how the node points to other nodes that it uses as inputs.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq, EnumIsVariant, EnumAsVariant)]
pub enum GenericNode<L> {
    /// an immediate value.
    Imm(Imm),

    /// an internal variable. exists to serve as placeholder for nodes who don't have a proper value.
    InternalVar(InternalVar),

    /// the initial value of a varnode
    Vn(Vn),

    /// a binary operation.
    BinOp(BinOp<L>),

    /// a unary operation.
    UnOp(UnOp<L>),

    /// an extension calculation.
    Ext(ExtNode<L>),

    /// a truncation calculation.
    Trunc(TruncNode<L>),

    /// a memory load operation
    Load(LoadNode<L>),

    /// a memory store operation
    Store(StoreNode<L>),

    /// the final value of a vn at the exit of control flow.
    ExitVn(ExitVnNode<L>),

    /// a call to another function.
    Call(CallNode<L>),

    /// the value of a vn when calling a function.
    CallVn(CallVnNode<L>),

    /// a value which may be clobbered by a function call.
    Clobber(ClobberNode<L>),

    /// a sleigh callother operation.
    CallOther(CallOtherOpNum),

    /// a conditional if operation.
    If(IfNode<L>),

    /// one of the cases of an if condition
    IfCase(IfCaseNode<L>),

    /// a phi node which merges multiple values coming from different paths in the code.
    Phi(PhiNode<L>),

    /// a region node which merges multiple control flow paths.
    Region(RegionNode<L>),

    /// the entrypoint of control flow.
    Entry,

    /// an exit of control flow.
    Exit(ExitNode<L>),
}

impl<L> GenericNode<L> {
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> GenericNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::InternalVar(v) => GenericNode::InternalVar(*v),
            GenericNode::Vn(vn) => GenericNode::Vn(*vn),

            GenericNode::BinOp(b) => GenericNode::BinOp(b.convert_links(|x| conversion(x))),
            GenericNode::UnOp(u) => GenericNode::UnOp(u.convert_links(|x| conversion(x))),
            GenericNode::Ext(e) => GenericNode::Ext(e.convert_links(|x| conversion(x))),
            GenericNode::Trunc(t) => GenericNode::Trunc(t.convert_links(|x| conversion(x))),
            GenericNode::Load(l) => GenericNode::Load(l.convert_links(|x| conversion(x))),
            GenericNode::Store(s) => GenericNode::Store(s.convert_links(|x| conversion(x))),
            GenericNode::ExitVn(ev) => GenericNode::ExitVn(ev.convert_links(|x| conversion(x))),
            GenericNode::Call(c) => GenericNode::Call(c.convert_links(|x| conversion(x))),
            GenericNode::CallVn(cv) => GenericNode::CallVn(cv.convert_links(|x| conversion(x))),
            GenericNode::Clobber(cl) => GenericNode::Clobber(cl.convert_links(|x| conversion(x))),

            GenericNode::CallOther(co) => GenericNode::CallOther(*co),

            GenericNode::If(i) => GenericNode::If(i.convert_links(|x| conversion(x))),
            GenericNode::IfCase(ic) => GenericNode::IfCase(ic.convert_links(|x| conversion(x))),
            GenericNode::Phi(p) => GenericNode::Phi(p.convert_links(|x| conversion(x))),
            GenericNode::Region(r) => GenericNode::Region(r.convert_links(|x| conversion(x))),

            GenericNode::Entry => GenericNode::Entry,
            GenericNode::Exit(e) => GenericNode::Exit(e.convert_links(|x| conversion(x))),
        }
    }

    /// returns an array of all links of this node.
    pub fn links(&self) -> NodeLinks<'_, L> {
        todo!()
    }

    /// returns a string which represents a human readable formatting of this node's structure. the returned string
    /// contains no information about the node's links, only the structure itself, which is everything other than the links.
    pub fn structural_display(&self) -> String {
        todo!()
    }
}

// convert from an integer value to an immediate node.
impl<L> From<u64> for GenericNode<L> {
    fn from(value: u64) -> Self {
        Imm(value).into()
    }
}
