use std::collections::{BinaryHeap, HashMap, HashSet};

use super::riscv;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Item
{
    pub degree: usize,
    pub name: String,
}

#[derive(Debug)]
struct Shader {
    edges: HashMap<String, Vec<String>>,
    vertices: HashSet<String>,
}

impl Shader {
    pub fn new(vertices: HashSet<String>) -> Self {
        Shader {
            edges: HashMap::new(),
            vertices,
        }
    }

    fn get_available_color(&self, current_color_map: &HashMap<String, String>, vertex: &String) -> Option<String> {
        let mut used_colors: HashSet<String> = HashSet::new();
        if let Some(neighbors) = self.edges.get(vertex) {
            for neighbor in neighbors {
                if let Some(color) = current_color_map.get(neighbor) {
                    used_colors.insert(color.clone());
                }
            }
        }

        for &color in riscv::RV_CALLER_SAVE_REGS.iter() {
            if !used_colors.contains(color) {
                return Some(color.to_string());
            }
        }
        for &color in riscv::RV_CALLEE_SAVE_REGS.iter() {
            if !used_colors.contains(color) {
                return Some(color.to_string());
            }
        }

        None
    }

    /// # Returns
    /// If successful, returns a HashMap with a color map
    /// If not successful, returns a virtual register with the greatest degree
    pub fn try_shade(&self) -> Result<HashMap<String, String>, String> {
        let color_order = self.get_color_order();
        let mut color_map: HashMap<String, String> = HashMap::new();
        for vertex in color_order {
            if let Some(color) = self.get_available_color(&color_map, &vertex) {
                color_map.insert(vertex, color);
            } else {
                return Err(self.get_spill_vertices());
            }
        }
        assert!(color_map.len() == self.vertices.len(), "Color map size mismatch");
        Ok(color_map)
    }


    pub fn add_conflict(&mut self, conflict: HashSet<String>) {
        for from in &conflict {
            assert!(self.vertices.contains(from), "Vertex not in graph: {}", from);
            for to in &conflict {
                if from == to {
                    continue; // Skip self-loops
                }
                self.edges
                    .entry(from.clone())
                    .or_insert_with(Vec::new)
                    .push(to.clone());
            }
        }
    }

    fn get_spill_vertices(&self) -> String
    {
        let mut max_degree = 0;
        let mut spill_vertex = String::new();

        for (vertex, neighbors) in &self.edges {
            let degree = neighbors.len();
            if degree > max_degree && !riscv::is_real_reg(vertex) {
                max_degree = degree;
                spill_vertex = vertex.clone();
            }
        }

        spill_vertex
    }

    fn get_color_order(&self) -> Vec<String> {
        let mut degree_map: HashMap<String, usize> = self.vertices
            .iter()
            .map(|v| {
                let degree = self.edges.get(v).map_or(0, |neighbors| neighbors.len());
                (v.clone(), degree)
            })
            .collect();
        let mut priority_queue = degree_map
            .iter()
            .map(|(name, degree)| Item { degree: *degree, name: name.clone() })
            .collect::<BinaryHeap<Item>>();
        let mut color_order = Vec::new();

        while !priority_queue.is_empty()
        {
            let Item { degree, name } = priority_queue.pop().unwrap();
            if let Some(recorded_degree) = degree_map.get(&name) {
                assert_eq!(*recorded_degree, degree, "Degree mismatch for vertex: {}", name);
            } else {
                continue; // Skip if the vertex has been removed
            }
            color_order.push(name.clone());
            for neighbor in self.edges.get(&name).unwrap_or(&Vec::new()).iter() {
                if let Some(neighbor_degree) = degree_map.get_mut(neighbor) {
                    *neighbor_degree = *neighbor_degree + 1;
                    priority_queue.push(Item {
                        degree: *neighbor_degree,
                        name: neighbor.clone(),
                    });
                }
            }
            degree_map.remove(&name);
        }
        assert!(color_order.len() == self.vertices.len(), "Color order length mismatch");
        color_order
    }
}