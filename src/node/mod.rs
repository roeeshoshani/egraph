use arrayvec::ArrayVec;
use derive_more::From;
use enum_display::EnumDisplay;
use enum_variant_accessors::{EnumAsVariant, EnumIsVariant};
use rsleigh::VnAddr;

pub mod imm;

use crate::{array_vec, node::imm::Imm};

/// the max amount of links in a node.
pub const NODE_MAX_LINKS: usize = 2;

/// an array of all links of a node.
pub type NodeLinks<'a, L> = ArrayVec<&'a L, NODE_MAX_LINKS>;

/// a varnode.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Vn {
    /// the size of this varnode.
    pub size: ValueSize,

    /// the address of this varnode.
    pub addr: VnAddr,
}
impl From<rsleigh::Vn> for Vn {
    fn from(vn: rsleigh::Vn) -> Self {
        Self {
            size: ValueSize { bytes: vn.size },
            addr: vn.addr,
        }
    }
}
impl From<Vn> for rsleigh::Vn {
    fn from(vn: Vn) -> Self {
        Self {
            size: vn.size.bytes,
            addr: vn.addr,
        }
    }
}

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
    #[display("u<")]
    UnsignedLess,
}

impl BinOpKind {
    /// applies this binary operation to the given immediates.
    pub fn apply_to_imms(&self, lhs: Imm, rhs: Imm) -> Imm {
        Imm::apply_bin_op(lhs, *self, rhs)
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
        todo!()
        // let res = match self {
        //     UnOpKind::Neg => operand.0.wrapping_neg(),
        //     UnOpKind::BitNot => !operand.0,
        // };
        // Imm(res)
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

/// the size of a value.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValueSize {
    /// the size in bytes.
    pub bytes: u32,
}
impl ValueSize {
    /// a 64-bit size.
    pub const U64: Self = Self { bytes: 8 };

    /// a 32-bit size.
    pub const U32: Self = Self { bytes: 4 };

    /// a 16-bit size.
    pub const U16: Self = Self { bytes: 2 };

    /// an 8-bit size.
    pub const U8: Self = Self { bytes: 1 };

    /// returns the size in bits.
    pub fn bits(&self) -> u32 {
        self.bytes * 8
    }
}

/// a function
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Function<L> {
    /// the list of output values of this function.
    pub outputs: Vec<L>,
}

/// a function call
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FnCall<L> {
    /// the function to call.
    pub function: L,

    /// a list of arguments to pass as inputs to the function call.
    pub arguments: Vec<L>,
}

/// a node which represents a parameter of a function.
///
/// this node does not refer to a specific function, but to the concept of using function arguments.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub struct FnParam(
    /// the index of the parameter in the list of parameters of the function.
    pub u32,
);

/// a node which represents a tail controlled loop.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub struct Loop<L> {
    /// a list of outputs for the loop.
    pub outputs: Vec<L>,

    /// the condition of the loop.
    pub cond: L,
}

/// a node which represents a parameter of a loop.
///
/// this node does not refer to a specific loop, but to the concept of using loop params.
#[derive(Debug, Clone, Copy, From, Hash, PartialEq, Eq)]
pub struct LoopParam(
    /// the index of the parameter in the list of parameters of the loop.
    pub u32,
);

/// an evaluation of a loop with specific input values.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LoopEval<L> {
    /// the loop to evaluate.
    pub loop_node: L,

    /// a list of inputs to to pass to the loop. the inputs will be used as the initial values for the loop parameters.
    ///
    /// must have the same shape as the loop's outputs.
    pub inputs: Vec<L>,
}

/// the initial state of the memory.
#[derive(Debug, Clone, Copy, From, Hash, PartialEq, Eq)]
pub struct InitialMemoryState;

/// get the value at the specified index from a tuple of values
#[derive(Debug, Clone, From, Hash, PartialEq, Eq)]
pub struct TupleGet<L> {
    /// the tuple of values to get the value from.
    pub tuple: L,

    /// the index of the value in the tuple.
    pub index: u32,
}
/// a node type that is generic over the link type. the link type determines how the node points to other nodes that it uses as inputs.
#[derive(Debug, Clone, From, Hash, PartialEq, Eq, EnumIsVariant, EnumAsVariant)]
pub enum GenericNode<L> {
    Imm(Imm),
    BinOp(BinOp<L>),
    UnOp(UnOp<L>),
    VnInitialValue(Vn),
    InitialMemoryState(InitialMemoryState),

    FnParam(FnParam),
    Function(Function<L>),
    FnCall(FnCall<L>),

    LoopParam(LoopParam),
    Loop(Loop<L>),
    LoopEval(LoopEval<L>),

    TupleGet(TupleGet<L>),
}

impl<L> GenericNode<L> {
    /// converts the links of the given node using the given conversion function, generating a new node with the converted link values.
    pub fn convert_links<L2, F>(&self, mut conversion: F) -> GenericNode<L2>
    where
        F: FnMut(&L) -> L2,
    {
        match self {
            GenericNode::Imm(imm) => GenericNode::Imm(*imm),
            GenericNode::BinOp(BinOp { kind, lhs, rhs }) => GenericNode::BinOp(BinOp {
                kind: *kind,
                lhs: conversion(lhs),
                rhs: conversion(rhs),
            }),
            GenericNode::UnOp(UnOp { kind, operand }) => GenericNode::UnOp(UnOp {
                kind: *kind,
                operand: conversion(operand),
            }),
            GenericNode::VnInitialValue(vn) => GenericNode::VnInitialValue(vn.clone()),
            GenericNode::FnParam(fn_param) => todo!(),
            GenericNode::Function(function) => todo!(),
            GenericNode::FnCall(fn_call) => todo!(),
            GenericNode::LoopParam(loop_param) => GenericNode::LoopParam(loop_param.clone()),
            GenericNode::Loop(Loop {
                outputs,
                cond: condition,
            }) => GenericNode::Loop(Loop {
                outputs: outputs.iter().map(&mut conversion).collect(),
                cond: conversion(condition),
            }),
            GenericNode::LoopEval(LoopEval { loop_node, inputs }) => {
                GenericNode::LoopEval(LoopEval {
                    loop_node: conversion(loop_node),
                    inputs: inputs.iter().map(&mut conversion).collect(),
                })
            }
            GenericNode::InitialMemoryState(initial_memory_state) => {
                GenericNode::InitialMemoryState(*initial_memory_state)
            }
            GenericNode::TupleGet(TupleGet { tuple, index }) => GenericNode::TupleGet(TupleGet {
                tuple: conversion(tuple),
                index: *index,
            }),
        }
    }

    /// returns an array of all links of this node.
    pub fn links(&self) -> NodeLinks<'_, L> {
        match self {
            GenericNode::BinOp(bin_op) => array_vec![&bin_op.lhs, &bin_op.rhs],
            GenericNode::UnOp(un_op) => array_vec![&un_op.operand],
            _ => array_vec![],
        }
    }

    /// returns a string which represents a human readable formatting of this node's structure. the returned string
    /// contains no information about the node's links, only the structure itself, which is everything other than the links.
    pub fn structural_display(&self) -> String {
        match self {
            GenericNode::Imm(imm) => format!("0x{:x}:u{}", imm.val, imm.size.bits()),
            GenericNode::BinOp(bin_op) => bin_op.kind.to_string(),
            GenericNode::UnOp(un_op) => un_op.kind.to_string(),
            GenericNode::VnInitialValue(vn) => todo!(),
            GenericNode::Function(function) => todo!(),
            GenericNode::FnCall(fn_call) => todo!(),
            GenericNode::Loop(_) => todo!(),
            GenericNode::FnParam(fn_param) => todo!(),
            GenericNode::LoopParam(loop_param) => todo!(),
            GenericNode::LoopEval(loop_eval) => todo!(),
            GenericNode::InitialMemoryState(initial_memory_state) => todo!(),
            GenericNode::TupleGet(tuple_get) => todo!(),
        }
    }
}
