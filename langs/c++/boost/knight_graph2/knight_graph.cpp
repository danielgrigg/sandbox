#include <boost/graph/graph_concepts.hpp>
#include <boost/iterator/counting_iterator.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/adjacency_list.hpp>
//#include <boost/graph/astar_search.hpp>

#include "boost/tuple/tuple.hpp"
#include <boost/bind.hpp>
#include <iostream>
#include <utility>
#include <algorithm>
#include <vector>
#include <list>

static const size_t BOARD_SIZE = 8;
static const size_t NUM_KNIGHT_MOVES = 8;

typedef std::pair < int, int > Position;
Position knight_jumps[NUM_KNIGHT_MOVES] = {
  Position(2, -1),
  Position(1, -2),
  Position(-1, -2),
  Position(-2, -1),
  Position(-2, 1),
  Position(-1, 2),
  Position(1, 2),
  Position(2, 1)
};

bool position_valid(Position p) {
  return p.first >= 0 && p.first < BOARD_SIZE && p.second >= 0 && p.second < BOARD_SIZE;
}

Position vertex_to_position(size_t v) {
  return std::make_pair(v / BOARD_SIZE, v % BOARD_SIZE);
}

size_t position_to_vertex(Position p) {
  return BOARD_SIZE * p.first + p.second;
}

std::string chess_notation(Position p) {
  char rank_file[] = {'a' + p.first, '1' + p.second, 0};
  return std::string(rank_file);
}

std::ostream& operator<<(std::ostream& os, Position rhs) {
  //os << rhs.first << " " << rhs.second;
  //  os << 'a' + rhs.first << '1' + rhs.second);
  os << chess_notation(rhs);
  return os;
}

Position operator+(const Position & p1, const Position & p2) {
  return Position(p1.first + p2.first, p1.second + p2.second);
}

typedef std::pair<int, int> Edge;

std::vector<Edge> make_vertex_edges(int n) {
  std::vector<Edge> ret;
  ret.reserve(BOARD_SIZE);
  Position vertex_pos = vertex_to_position(n);

  for (int i = 0; i < BOARD_SIZE; ++i) {
    Position jump_position = vertex_pos + knight_jumps[i];
    if (position_valid(jump_position)) {
      ret.push_back(std::make_pair(n, position_to_vertex(jump_position)));
    }
  }
  return ret;
}


void concat_edges(std::vector<Edge>& result, std::vector<Edge>& src) {
  result.insert(result.end(), src.begin(), src.end());
}

int main(int argc, char **argv)
{
  using namespace boost;
  std::cout << "Running knight-graph" << std::endl;

  typedef adjacency_list < vecS, vecS, directedS,
    no_property, property < edge_weight_t, int > > graph_t;
  typedef graph_traits < graph_t >::vertex_descriptor vertex_descriptor;
  typedef graph_traits < graph_t >::edge_descriptor edge_descriptor;

  const int num_nodes = BOARD_SIZE * BOARD_SIZE;

  std::vector<Position> vertex_positions;
  vertex_positions.reserve(BOARD_SIZE * BOARD_SIZE);
  std::transform(boost::counting_iterator<int>(0),
                  boost::counting_iterator<int>(num_nodes),
                  std::back_inserter(vertex_positions),
                  vertex_to_position);

  std::vector<Edge> edges;
  std::list<std::vector<Edge> > vertex_edges;
  std::transform(boost::counting_iterator<int>(0),
                  boost::counting_iterator<int>(num_nodes),
                  std::back_inserter(vertex_edges),
                  make_vertex_edges);

  std::for_each(vertex_edges.begin(), vertex_edges.end(),
      boost::bind(concat_edges, boost::ref(edges), _1));

  std::vector<int> weights(edges.size(), 1);
  graph_t g(edges.begin(), edges.end(), weights.begin(), num_nodes);
 
  int v = 50;
  std::cout << "out_degree(v, g) " << out_degree(v, g) << "\n";
  std::cout << vertex_to_position(v) <<  " adjacents\n";
  graph_traits<graph_t>::out_edge_iterator ei, ei_end;
  for (boost::tie(ei, ei_end) = out_edges(v, g); ei != ei_end; ++ei) {
//    std::cout << "e " << vertex_to_position(e->first) << "\n";
    edge_descriptor e = *ei;
    std::cout << "e " << vertex_to_position(source(e, g)) << 
      " -> " << vertex_to_position(target(e, g)) << std::endl;
  }

  for (int x = 0; x < 1; ++x) {
    for (int src_index = 0; src_index < BOARD_SIZE*BOARD_SIZE; ++src_index) {
      std::vector<vertex_descriptor> pred(num_vertices(g));
      std::vector<int> d(num_vertices(g));
      vertex_descriptor src = vertex(src_index, g);
      dijkstra_shortest_paths(g, src, predecessor_map(&pred[0]).distance_map(&d[0]));

         std::cout << "distances and parents:" << std::endl;
         graph_traits < graph_t >::vertex_iterator vi, vend;
         for (boost::tie(vi, vend) = vertices(g); vi != vend; ++vi) {
         std::cout << "distance(" << vertex_positions[*vi] << ") = " 
         << d[*vi] << ", ";
         std::cout << "parent(" << vertex_positions[*vi] << ") = " 
         << vertex_positions[pred[*vi]] << std:: endl;
         }
    }
  }


  std::cout << std::endl;
  return 0;
}

