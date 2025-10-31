use std::{fmt::Write as _, io::Write as _, path::Path};

use duct::cmd;
use hashbrown::HashSet;
use tempfile::NamedTempFile;

use crate::egraph::*;

impl ENodesUnionFind {
    /// converts the egraph to an svg using the graphviz dot language, and writes the svg file to the given path.
    ///
    /// this requires having the `fdp` program installed and in PATH.
    pub fn dump_graphviz_dot_svg<P: AsRef<Path>>(&self, out_file_path: P) {
        let out_file_path = out_file_path.as_ref();

        let dot = self.to_graphviz_dot();

        let mut tmpfile = NamedTempFile::with_suffix(".dot").unwrap();
        tmpfile.write_all(dot.as_bytes()).unwrap();
        tmpfile.flush().unwrap();

        let tmp_path = tmpfile.into_temp_path();

        cmd!("fdp", "-Tsvg", &*tmp_path, "-o", out_file_path)
            .run()
            .unwrap();
    }

    /// converts the egraph to a graphviz dot language representation.
    pub fn to_graphviz_dot(&self) -> String {
        let mut out = String::new();

        let find_eclass_label_of_node = |enode_id: ENodeId| {
            self.enumerate_enodes_eq_to(enode_id)
                .map(|(enode_id, _)| enode_id)
                .min()
                .unwrap()
        };

        fn eclass_dot_id(eclass_label: ENodeId) -> String {
            format!("cluster_eclass_{}", eclass_label.0.0)
        }
        fn enode_dot_id(eclass_label: ENodeId, index_in_eclass: usize) -> String {
            format!("eclass_{}_item_{}", eclass_label.0.0, index_in_eclass)
        }

        let eclass_labels: HashSet<ENodeId> =
            self.enode_ids().map(find_eclass_label_of_node).collect();

        for &eclass_label in &eclass_labels {
            let eclass_id = eclass_label.eclass_id();

            writeln!(&mut out, "subgraph {} {{", eclass_dot_id(eclass_label),).unwrap();

            writeln!(
                &mut out,
                "color=gray60; style=\"rounded\"; fontcolor=\"white\"; label=\"{}\"",
                self.extract_eclass(eclass_id)
            )
            .unwrap();

            // one node per enode in the class
            for (i, (cur_enode_id, cur_enode)) in
                self.enumerate_enodes_eq_to(eclass_label).enumerate()
            {
                let label = cur_enode.structural_display();
                writeln!(
                    &mut out,
                    "{} [label=\"{}\", tooltip=\"{}\"];",
                    enode_dot_id(eclass_label, i),
                    label,
                    cur_enode_id.0.0
                )
                .unwrap();
            }

            out.push_str("}\n");
        }

        // edges from each enode to target e-class clusters
        for &eclass_label in &eclass_labels {
            for (i, cur_enode) in self.enodes_eq_to(eclass_label).enumerate() {
                for link in cur_enode.links() {
                    // route to target cluster anchor; ltail/lhead draw the edge between clusters
                    let target_eclass_label = find_eclass_label_of_node(link.enode_id);
                    writeln!(
                        &mut out,
                        "{} -> {} [lhead={}];",
                        enode_dot_id(eclass_label, i),
                        enode_dot_id(target_eclass_label, 0),
                        eclass_dot_id(target_eclass_label)
                    )
                    .unwrap();
                }
            }
        }

        return format!(
            r##"
            digraph egraph {{
                compound=true;
                rankdir=TB;
                bgcolor="#181818"
                node [
                    fontcolor = "#e6e6e6",
                    style = filled,
                    color = "#e6e6e6",
                    fillcolor = "#333333"
                ]
                edge [
                    color = "#e6e6e6",
                    fontcolor = "#e6e6e6"
                ]
                {}
            }}
            "##,
            out
        );
    }
}
