use rsleigh::SleighCtx;

use crate::node::{BinOp, GenericNode, UnOp};

/// the link of a recursive node.
pub enum RecNodeLink {
    /// a regular recursive node link. just points to another node.
    Regular(Box<RecNode>),

    /// a special link value used to mark a link that loops to a parent node.
    ///
    /// loops are not representable in recursive nodes, since each node must have exactly one parent.
    ///
    /// but, in some cases, we want to convert some graph representation to a recursive node, and have a representation that
    /// still encodes all information that could be encoded, but excluding the loops, which obviously can't be encoded in recursive
    /// nodes.
    ///
    /// so, this variant is used to mark such unrepresentable link, while still allowing us to represent the rest of the graph's
    /// structure in a convenient recursive way.
    // TODO: maybe we should just avoid using recursive nodes in those scenarios, and instead use graphs, because this is very hacky.
    Loop,
}

/// a recursive node, which is a node whose link value directly owns the linked-to node.
pub type RecNode = GenericNode<RecNodeLink>;

/// a bin op which uses recursive nodes.
pub type RecBinOp = BinOp<RecNodeLink>;

/// a un op which uses recursive nodes.
pub type RecUnOp = UnOp<RecNodeLink>;

// convenience, convert a recursive node value directly to a recursive link value.
impl<T> From<T> for RecNodeLink
where
    RecNode: From<T>,
{
    fn from(value: T) -> Self {
        Self::Regular(Box::new(value.into()))
    }
}
impl RecNode {
    pub fn display(&self, sleigh_ctx: &SleighCtx) -> String {
        let structural_display = self.structural_display(sleigh_ctx);
        let links = self.links();
        if links.is_empty() {
            return structural_display;
        }

        let links_display = links
            .iter()
            .map(|link| link.display(sleigh_ctx))
            .collect::<Vec<_>>()
            .join(", ");

        format!("{}({})", structural_display, links_display)
    }
}
impl RecNodeLink {
    pub fn display(&self, sleigh_ctx: &SleighCtx) -> String {
        match self {
            RecNodeLink::Regular(node) => node.display(sleigh_ctx),
            RecNodeLink::Loop => "...".into(),
        }
    }
}
