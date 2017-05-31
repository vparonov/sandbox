import networkx as nx
import networkx.algorithms.isomorphism as iso
from networkx.algorithms import isomorphism

G1 = nx.Graph()
G2 = nx.Graph()

G1.clear()
G2.clear()

G1.add_node(0, lbl="S/PMOS")
G1.add_node(1, lbl="PMOS")
G1.add_node(2, lbl="D/PMOS")
G1.add_node(3, lbl="G/PMOS")
G1.add_edges_from([(0, 1), (2, 1), (3, 1)]) 

G1.add_node(4, lbl="S/PMOS")
G1.add_node(5, lbl="PMOS")
G1.add_node(6, lbl="D/PMOS")
G1.add_node(7, lbl="G/PMOS")
G1.add_edges_from([(4, 5), (6, 5), (7, 5)]) 

G2.add_node(10, lbl="S/PMOS")
G2.add_node(11, lbl="PMOS")
G2.add_node(12, lbl="D/PMOS")
G2.add_node(13, lbl="G/PMOS")
G2.add_edges_from([(10, 11), (12, 11), (13, 11)]) 

GM = isomorphism.GraphMatcher(G1, G2, node_match=iso.categorical_node_match(['lbl'],['E']))

for subgraph in GM.subgraph_isomorphisms_iter():
    print(subgraph)
	
#print(GM.subgraph_is_isomorphic())
#print(GM.mapping)