use crate::{
    egraph::{rewrite::*, *},
    node::imm::Imm,
};

struct Match {
    loop_enode_id: ENodeId,
    inner_loop_use_added_imm: Imm,
    var_index: u32,
}

struct TmpAttemptRewrite;
impl Rewrite for TmpAttemptRewrite {
    fn apply(&self, egraph: &mut EGraph) -> DidAnything {
        let matches = find_matches(egraph);
        apply_matches(&matches, egraph)
    }
}

fn apply_matches(matches: &[Match], egraph: &mut EGraph) -> DidAnything {
    let mut did_anything = DidAnything::False;
    for match_obj in matches {
        let loop_node = egraph[match_obj.loop_enode_id]
            .as_e_node()
            .unwrap()
            .as_loop()
            .unwrap();
        let new_loop_id = egraph.alloc_loop_id();
        let new_loop = Loop {
            inner_vars: loop_node.inner_vars.clone(),
            vars: loop_node.vars.clone(),
            cond: loop_node.cond.clone(),
            id: new_loop_id,
        };
        let new_loop_enode = egraph.add_enode(new_loop.into());
        if new_l
    }
    did_anything
}

fn find_matches(egraph: &EGraph) -> Vec<Match> {
    let mut matches = Vec::new();
    for eclass_id in egraph.union_find().eclass_ids() {
        for (enode_id, enode) in egraph.union_find().enumerate_enodes_in_eclass(eclass_id) {
            match_root_loop_enode(enode_id, enode, egraph, &mut matches);
        }
    }
    matches
}

fn match_root_loop_enode(
    enode_id: ENodeId,
    enode: &ENode,
    egraph: &EGraph,
    matches: &mut Vec<Match>,
) {
    match enode {
        GenericNode::Loop(loop_node) => {
            for cond_enode in egraph.union_find().enodes_in_eclass(loop_node.cond) {
                let Some(cond_bin_op) = cond_enode.as_bin_op() else {
                    continue;
                };
                if cond_bin_op.kind != BinOpKind::UnsignedLess {
                    continue;
                }
                for cond_lhs_enode in egraph.union_find().enodes_in_eclass(cond_bin_op.lhs) {
                    let Some(cond_other_loop_var) = cond_lhs_enode.as_other_loop_var() else {
                        continue;
                    };
                    if cond_other_loop_var.loop_id != loop_node.id {
                        // this value belongs to another loop
                        continue;
                    }

                    for var_eclass_id in &loop_node.vars {
                        scan_inner_loop_link(
                            *var_eclass_id,
                            cond_other_loop_var.index,
                            enode_id,
                            loop_node,
                            egraph,
                            matches,
                        );
                    }
                }
            }
        }
        _ => {
            todo!()
        }
    }
}

fn scan_inner_loop_link(
    link_eclass_id: EClassId,
    cond_loop_var_index: u32,
    loop_enode_id: ENodeId,
    loop_node: &Loop<EClassId>,
    egraph: &EGraph,
    matches: &mut Vec<Match>,
) {
    for enode in egraph.union_find().enodes_in_eclass(link_eclass_id) {
        match_inner_loop_enode(
            enode,
            cond_loop_var_index,
            loop_enode_id,
            loop_node,
            egraph,
            matches,
        );

        // the enode itself, didn't match. try recursing into the enode's links.
        if !enode.is_loop() {
            for cur_link in enode.links() {
                scan_inner_loop_link(
                    *cur_link,
                    cond_loop_var_index,
                    loop_enode_id,
                    loop_node,
                    egraph,
                    matches,
                );
            }
        }
    }
}

fn match_inner_loop_enode(
    enode: &ENode,
    cond_loop_var_index: u32,
    loop_enode_id: ENodeId,
    loop_node: &Loop<EClassId>,
    egraph: &EGraph,
    matches: &mut Vec<Match>,
) {
    let Some(bin_op) = enode.as_bin_op() else {
        return;
    };
    if bin_op.kind != BinOpKind::Add {
        return;
    }
    for lhs_enode in egraph.union_find.enodes_in_eclass(bin_op.lhs) {
        let Some(lhs_other_loop_var) = lhs_enode.as_other_loop_var() else {
            continue;
        };
        let expected_other_loop_var = OtherLoopVar {
            loop_id: loop_node.id,
            index: cond_loop_var_index,
        };
        if *lhs_other_loop_var != expected_other_loop_var {
            continue;
        };

        for rhs_enode in egraph.union_find.enodes_in_eclass(bin_op.rhs) {
            let Some(rhs_imm) = rhs_enode.as_imm() else {
                continue;
            };
            matches.push(Match {
                loop_enode_id,
                inner_loop_use_added_imm: *rhs_imm,
                var_index: cond_loop_var_index,
            });
        }
    }
}
