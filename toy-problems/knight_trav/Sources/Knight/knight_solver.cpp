/*
 * \file knight_solver.cpp
 * Implements the KnightSolver interface.
 *
 * @author Daniel Grigg
 *
 */
#include "knight_solver.h"
#include "position.h"
#include "io.h"

#include <iostream>
#include <vector>
#include <list>
#include <algorithm>
#include <boost/bind.hpp>
#include <boost/iterator/counting_iterator.hpp>
#include <boost/graph/graph_concepts.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>

namespace kt {

  // Legal, relative knight movements
  Position knight_jumps[KT::KNIGHT_MOVE_DEGREE] = {
  Position(2, -1),
  Position(1, -2),
  Position(-1, -2),
  Position(-2, -1),
  Position(-2, 1),
  Position(-1, 2),
  Position(1, 2),
  Position(2, 1)
};

// Convert a node into a Position (0 <= node <= 63).
Position node_to_position(int n) {
  return Position(n / KT::BOARD_SIZE, n % KT::BOARD_SIZE);
}

// Convert a Position into a node (0 <= node <= 63).
size_t position_to_node(Position p) {
  return KT::BOARD_SIZE * p[0] + p[1];
}

// List all legal edges adjacent to vertex n.
std::vector<Edge> vertex_adjacent_edges(int n) {
  std::vector<Edge> edges;
  edges.reserve(KT::BOARD_SIZE);
  Position vertex_pos = node_to_position(n);
                
  // Add all possible knight moves that stay on the board.
  for (int i = 0; i < KT::BOARD_SIZE; ++i) {
    Position jump_position = vertex_pos + knight_jumps[i];
    if (jump_position.valid_position()) {
      edges.push_back(std::make_pair(n, position_to_node(jump_position)));
    }
  }
  return edges;
}

// Append a sequence to a sequence.
template <typename SequenceT>
void concat(SequenceT& result, SequenceT& src) {
  result.insert(result.end(), src.begin(), src.end());
}

KnightSolver::KnightSolver() {
  std::list<std::vector<Edge> > edges_by_vertex;

  // Generate the adjacent-edges for every vertex.
  std::transform(boost::counting_iterator<int>(0),
      boost::counting_iterator<int>(KT::NUM_BOARD_VERTICES),
      std::back_inserter(edges_by_vertex),
      vertex_adjacent_edges);

  // Flatten into a single, consecutive edge list
  std::vector<Edge> edges;
  std::for_each(edges_by_vertex.begin(), edges_by_vertex.end(),
      boost::bind(concat<std::vector<Edge> >, boost::ref(edges), _1));

  // All weights are one and yes, we could use BFS instead.  But dijkstra
  // is a little better optimised in boost.
  std::vector<int> weights(edges.size(), 1);
  _g = graph_t_ptr(new graph_t(edges.begin(), edges.end(), weights.begin(), 
        KT::NUM_BOARD_VERTICES));
}

bool KnightSolver::shortest_path(const Position& start_at, 
    const Position& end_at,
    std::vector<Position>& path) { 
  path.clear();

  if (!start_at.valid_position() || !end_at.valid_position()) {
    return false;
  }

  using namespace boost;

  std::vector<vertex_descriptor> pred(num_vertices(*_g));
  std::vector<int> dist(num_vertices(*_g));
  vertex_descriptor src = vertex(position_to_node(start_at), *_g);
  dijkstra_shortest_paths(*_g, src, 
      predecessor_map(&pred[0]).distance_map(&dist[0]));

  // Walk the predecessors and construct the backwards path.
  vertex_descriptor v(position_to_node(end_at));
  path.reserve(dist[v]);
  do {
    path.push_back(node_to_position(v));
    v = pred[v];
  } while (dist[v] > 0);

  reverse(path.begin(), path.end());
  return true;
}

bool KnightSolver::knight_travail(const std::string& input, std::string& output) {

  Position start_at, end_at;
  if (parse_input(input, start_at, end_at)) {

    std::vector<Position> route;

    if (!shortest_path(start_at, end_at, route)) {
      std::cerr << "Unable to find route between " 
        << start_at.chess_notation()
        << " and " << end_at.chess_notation() << std::endl;
      return false;
    }
    output = route_to_string(route);
  } else {
    std::cerr << "Input must be a pair of valid chesssboard positions"
      << " in chess notation, eg, B2 E5" << std::endl;
    return false;

  }
  return true;
}
}

