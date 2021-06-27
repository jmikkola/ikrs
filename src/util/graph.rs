// module graph
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;
use std::hash::Hash;

/// This could probably be much more efficient by borrowing all values
pub struct Graph<A> {
    nodes: HashSet<A>,
    edges: HashMap<A, HashSet<A>>,
}

impl<A: Eq + Hash + Clone> Graph<A> {
    pub fn new() -> Self {
        Graph {
            nodes: HashSet::new(),
            edges: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: A) {
        self.nodes.insert(node);
    }

    /// Automatically adds the nodes
    pub fn add_edge(&mut self, from: A, to: A) {
        self.add_node(from.clone());
        self.add_node(to.clone());

        if let Some(to_set) = self.edges.get_mut(&from) {
            to_set.insert(to);
        } else {
            let mut to_set = HashSet::new();
            to_set.insert(to);
            self.edges.insert(from, to_set);
        }
    }

    pub fn get_topo_ordering(&self) -> Option<Vec<Vec<A>>> {
        let mut num_dependencies = self.get_num_dependences();
        let reversed = self.reverse();

        let mut nodes_to_process = LinkedList::new();
        for (node, count) in num_dependencies.iter() {
            if *count == 0 {
                nodes_to_process.push_front(node.clone());
            }
        }

        let mut nodes_processed = 0;
        let mut remaining_in_group = nodes_to_process.len();
        let mut groups = vec![];
        let mut group = vec![];

        while let Some(node) = nodes_to_process.pop_back() {
            nodes_processed += 1;

            if let Some(parents) = reversed.edges.get(&node) {
                for parent in parents.iter() {
                    let new_count = num_dependencies[parent] - 1;
                    num_dependencies.insert(parent.clone(), new_count);
                    if new_count == 0 {
                        nodes_to_process.push_front(parent.clone());
                    }
                }
            }

            group.push(node.clone());
            remaining_in_group -= 1;
            if remaining_in_group == 0 {
                groups.push(group);
                group = vec![];
                remaining_in_group = nodes_to_process.len();
            }
        }

        if nodes_processed < self.nodes.len() {
            None
        } else {
            Some(groups)
        }
    }

    fn get_num_dependences(&self) -> HashMap<A, usize> {
        let mut counts = HashMap::new();
        for node in self.nodes.iter() {
            counts.insert(node.clone(), 0);
        }
        for (from, to_set) in self.edges.iter() {
            counts.insert(from.clone(), to_set.len());
        }
        counts
    }

    pub fn reverse(&self) -> Self {
        let mut result = Graph::new();
        result.nodes = self.nodes.clone();
        for node in self.nodes.iter() {
            result.edges.insert(node.clone(), HashSet::new());
        }
        for (from, to_set) in self.edges.iter() {
            for to in to_set.iter() {
                result.edges.get_mut(to).unwrap().insert(from.clone());
            }
        }
        result
    }

    pub fn find_cycles(&self) -> Vec<Vec<A>> {
        self.strongly_connected_components()
            .drain(..)
            .filter(|v| v.len() > 1)
            .collect()
    }

    // https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm
    pub fn strongly_connected_components(&self) -> Vec<Vec<A>> {
        let ordering = {
            let mut visited = HashSet::new();
            let mut ordering = Vec::new();
            for node in self.nodes.iter() {
                self.scc_visit(&mut visited, &mut ordering, node);
            }
            ordering
        };

        let assignments = {
            let reversed = self.reverse();
            let mut assignments = HashMap::new();
            for node in ordering.iter().rev() {
                reversed.scc_assign(&mut assignments, node, node);
            }
            assignments
        };

        let mut components: HashMap<A, Vec<A>> = HashMap::new();
        for (node, component) in assignments {
            if let Some(component_vec) = components.get_mut(&component) {
                component_vec.push(node);
            } else {
                let component_vec = vec![node];
                components.insert(component, component_vec);
            }
        }

        let mut result = vec![];
        for (_, nodes) in components.drain() {
            result.push(nodes);
        }
        result
    }

    fn scc_visit<'a>(
        &'a self,
        visited: &mut HashSet<&'a A>,
        ordering: &mut Vec<&'a A>,
        node: &'a A,
    ) {
        if visited.contains(node) {
            return;
        }
        visited.insert(node);

        if let Some(children) = self.edges.get(node) {
            for child in children.iter() {
                self.scc_visit(visited, ordering, child);
            }
        }

        ordering.push(node);
    }

    /// To be called on the reversed graph
    fn scc_assign(&self, assignments: &mut HashMap<A, A>, node: &A, component: &A) {
        if assignments.contains_key(node) {
            return;
        }
        assignments.insert(node.clone(), component.clone());

        if let Some(children) = self.edges.get(node) {
            for child in children.iter() {
                self.scc_assign(assignments, child, component);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn build_graph(description: Vec<(i32, Vec<i32>)>) -> Graph<i32> {
        let mut graph = Graph::new();
        for (node, edges) in description {
            for edge in edges {
                graph.add_edge(node, edge);
            }
        }
        graph
    }

    fn set_of<A: Eq + Hash>(items: Vec<A>) -> HashSet<A> {
        let mut items_ = items;
        items_.drain(..).collect()
    }

    #[test]
    fn test_reverse_simple() {
        let simple = vec![(1, vec![2, 3])];
        let reversed = build_graph(simple).reverse();

        let expected_nodes = set_of(vec![1, 2, 3]);
        assert!(reversed.nodes == expected_nodes);

        assert!(reversed.edges[&1].is_empty());
        assert!(reversed.edges[&2] == set_of(vec![1]));
        assert!(reversed.edges[&3] == set_of(vec![1]));
    }

    #[test]
    fn test_find_cycles_empty() {
        assert!(build_graph(vec![]).find_cycles().is_empty());
    }

    #[test]
    fn test_find_cycles_tree() {
        let description = vec![
            (1, vec![11, 12, 13]),
            (11, vec![104, 105, 106]),
            (12, vec![107]),
            (105, vec![1009]),
        ];
        let cycles = build_graph(description).find_cycles();
        assert!(cycles.is_empty());
    }

    #[test]
    fn test_find_cycles_dag() {
        let description = vec![
            (1, vec![11, 12, 13]),
            (11, vec![104, 105, 106]),
            (12, vec![104, 105]),
            (13, vec![1009]),
            (105, vec![1009]),
        ];
        let cycles = build_graph(description).find_cycles();
        assert!(cycles.is_empty());
    }

    #[test]
    fn test_find_cycles_in_graph_with_cycles() {
        let description = vec![
            (1, vec![2]),
            (2, vec![3, 5, 6]),
            (3, vec![4, 7]),
            (4, vec![3, 8]),
            (5, vec![1, 6]),
            (6, vec![7]),
            (7, vec![6]),
            (8, vec![4, 7]),
        ];

        let mut cycles = build_graph(description).find_cycles();
        let expected = vec![vec![1, 2, 5], vec![3, 4, 8], vec![6, 7]];

        for cycle in cycles.iter_mut() {
            cycle.sort_unstable();
        }
        cycles.sort();
        assert!(cycles == expected);
    }

    #[test]
    fn test_get_topo_ordering_when_cycles() {
        let description = vec![
            (1, vec![2]),
            (2, vec![3, 5, 6]),
            (3, vec![4, 7]),
            (4, vec![3, 8]),
            (5, vec![1, 6]),
            (6, vec![7]),
            (7, vec![6]),
            (8, vec![4, 7]),
        ];
        assert!(build_graph(description).get_topo_ordering().is_none());
    }

    #[test]
    fn test_get_topo_ordering_dag() {
        let description = vec![
            (1, vec![11, 12, 13]),
            (11, vec![104, 105, 106]),
            (12, vec![104, 105]),
            (13, vec![1009]),
            (105, vec![1009]),
        ];

        let mut ordering = build_graph(description).get_topo_ordering().unwrap();
        for group in ordering.iter_mut() {
            group.sort_unstable();
        }

        let expected = vec![vec![104, 106, 1009], vec![13, 105], vec![11, 12], vec![1]];
        assert!(expected == ordering);
    }
}
