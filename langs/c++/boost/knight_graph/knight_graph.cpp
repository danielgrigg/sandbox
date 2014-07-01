#include <boost/graph/graph_concepts.hpp>
#include <boost/graph/properties.hpp>
#include <boost/iterator/counting_iterator.hpp>
//#include <boost/graph/breadth_first_search.hpp>
#include <boost/graph/dijkstra_shortest_paths.hpp>
#include <boost/graph/astar_search.hpp>

#include "boost/tuple/tuple.hpp"
#include <iostream>
#include <utility>
#include <map>

class knight_graph;
class knight_incident_edge_iterator;
class knight_adjacency_iterator;
struct edge_weight_map;

static const size_t BOARD_SIZE = 8;
static const size_t NUM_KNIGHT_MOVES = 8;
typedef int cost;


namespace boost {
  template<>
  struct property_map< knight_graph, edge_weight_t > {
    typedef edge_weight_map type;
    typedef edge_weight_map const_type;
  };

  template<>
  struct property_map< const knight_graph, edge_weight_t > {
    typedef edge_weight_map type;
    typedef edge_weight_map const_type;
  };
}



struct knight_traversal_category:
  virtual public boost::bidirectional_graph_tag,
  virtual public boost::incidence_graph_tag,
  virtual public boost::vertex_list_graph_tag
  {};


class knight_graph {
public:

  // Graph types
  typedef size_t vertex_descriptor;
  typedef boost::undirected_tag directed_category;
  //  typedef boost::directed_tag directed_category;
  typedef boost::disallow_parallel_edge_tag edge_parallel_category;
  typedef knight_traversal_category traversal_category;

  // IncidentGraph types
  typedef std::pair<vertex_descriptor, vertex_descriptor> edge_descriptor;
  typedef knight_incident_edge_iterator out_edge_iterator;
  typedef std::size_t degree_size_type;

  typedef knight_incident_edge_iterator in_edge_iterator;


  // VertexListGraph
  typedef boost::counting_iterator<vertex_descriptor> vertex_iterator;
  typedef std::size_t vertices_size_type;

  // EdgeListGraph associated types
  typedef void edge_iterator;
  typedef void edges_size_type;

  typedef vertex_descriptor vertex_property_type;



  knight_graph(){}

  std::size_t num_vertices()const { return BOARD_SIZE * BOARD_SIZE; }
private:
};

namespace boost
{
    template <> struct graph_traits<knight_graph>
    {
      typedef knight_graph G;
      
      typedef G::vertex_descriptor      vertex_descriptor;
      typedef G::edge_descriptor        edge_descriptor;
      typedef G::out_edge_iterator      out_edge_iterator;
      
      typedef G::directed_category      directed_category;
      typedef G::edge_parallel_category edge_parallel_category;
      typedef G::traversal_category     traversal_category;
      
      typedef G::degree_size_type       degree_size_type;
      
      typedef G::vertex_iterator vertex_iterator;
      typedef G::vertices_size_type vertices_size_type;

      typedef G::in_edge_iterator in_edge_iterator;
      typedef void edge_iterator;
      typedef void edges_size_type;

      //typedef void adjacency_iterator;
    };
}


typedef boost::graph_traits<knight_graph>::vertex_descriptor vertex_descriptor;
typedef boost::graph_traits<knight_graph>::edge_descriptor edge_descriptor;
//typedef boost::graph_traits<knight_graph>::out_edge_iterator out_edge_iterator;
typedef boost::graph_traits<knight_graph>::degree_size_type degree_size_type;
typedef boost::graph_traits<knight_graph>::vertex_iterator vertex_iterator;
typedef boost::graph_traits<knight_graph>::vertices_size_type vertices_size_type;

//knight_graph::vertex_descriptor source(knight_graph::edge_descriptor e, const knight_graph&);
//knight_graph::vertex_descriptor target(knight_graph::edge_descriptor e, const knight_graph&);
//std::pair<knight_graph::out_edge_iterator, 
//	  knight_graph::out_edge_iterator> out_edges(knight_graph::vertex_descriptor u, 
//						     const knight_graph& g);
//knight_graph::degree_size_type out_degree(knight_graph::vertex_descriptor u, const knight_graph& g);

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

Position vertex_to_position(vertex_descriptor v) {
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

// IncidenceGraph valid expressions
 knight_graph::vertex_descriptor source(knight_graph::edge_descriptor e, 
					const knight_graph&) {
  // The first vertex in the edge pair is the source.
   return e.first;
 }
 
 knight_graph::vertex_descriptor target(knight_graph::edge_descriptor e, 
					const knight_graph&) {
 // The second vertex in the edge pair is the target.
 return e.second;
}

// VertexList expressions
vertices_size_type num_vertices(const knight_graph& g) {
  return g.num_vertices();
}

std::pair<vertex_iterator, vertex_iterator> vertices(const knight_graph& g) {
  return std::pair<vertex_iterator, vertex_iterator>(vertex_iterator(0),
						     vertex_iterator(num_vertices(g)));
}

 struct knight_incident_edge_iterator : 
  public boost::iterator_facade<knight_incident_edge_iterator,  
				std::pair<size_t, size_t>,
				boost::forward_traversal_tag,
				std::pair<size_t, size_t> >

 {
  knight_incident_edge_iterator():
    _edge_index(0),
    _v(0),
    _g(NULL)
  {}

  knight_incident_edge_iterator(int edge_index, size_t v, 
				const knight_graph& g):
    _edge_index(edge_index),
    _v(v),
    _g(&g) {
    next_valid();
  }

   knight_incident_edge_iterator & operator=(const knight_incident_edge_iterator& rhs) {
     _edge_index = rhs._edge_index;
     _v = rhs._v;
     _g = rhs._g;
     return *this;
   }

   bool equal(const knight_incident_edge_iterator& rhs)const { return operator==(rhs); }
   void increment() { operator++(); }

  size_t operator*()const { 
    return position_to_vertex(vertex_to_position(_v) + 
			      knight_jumps[_edge_index]);
  }
  void operator++() { ++_edge_index; next_valid(); }
  bool operator==(const knight_incident_edge_iterator& rhs)const {
    return _edge_index == rhs._edge_index; 
  }
protected:
  int _edge_index;
  size_t _v;
  const knight_graph* _g;
  
  void next_valid() {
    while (!position_valid(vertex_to_position(_v) + knight_jumps[_edge_index])
	   && _edge_index < NUM_KNIGHT_MOVES) {
      ++_edge_index;
    }
  }
};

 std::pair<knight_graph::out_edge_iterator, knight_graph::out_edge_iterator>
   out_edges(knight_graph::vertex_descriptor u, 
	     const knight_graph& g) {
   return std::make_pair(knight_graph::out_edge_iterator(0, u, g),
			 knight_graph::out_edge_iterator(NUM_KNIGHT_MOVES, u, g));
}

 knight_graph::degree_size_type out_degree(knight_graph::vertex_descriptor u, 
					   const knight_graph& g) {
   knight_graph::out_edge_iterator e, e_end;
   size_t degree = 0;
   for (boost::tie(e, e_end) = out_edges(u, g); !(e == e_end); ++e) {
     ++degree;
   }
   return degree;
 }

// BidirectionalGraph valid expressions
 std::pair<knight_graph::in_edge_iterator, knight_graph::in_edge_iterator>
  in_edges(knight_graph::vertex_descriptor u, const knight_graph& g) {
  // The in-edges and out-edges are the same in an undirected graph.
  return out_edges(u, g);
}

knight_graph::degree_size_type in_degree(knight_graph::vertex_descriptor u, const knight_graph& g) {
  // The in-degree and out-degree are both equal to the number of incident
  // edges in an undirected graph.
  return out_degree(u, g);
}

knight_graph::degree_size_type degree(knight_graph::vertex_descriptor u, const knight_graph& g) {
  // The in-degree and out-degree are both equal to the number of incident
  // edges in an undirected graph.
  return out_degree(u, g);
}


// euclidean distance heuristic
template <class Graph, class CostType>
  class distance_heuristic : public boost::astar_heuristic<Graph, CostType>
{
public:
  typedef typename boost::graph_traits<Graph>::vertex_descriptor Vertex;
  distance_heuristic(Vertex goal)
    : m_goal(goal) {}
  CostType operator()(Vertex u)
  {
    //    CostType dx = m_location[m_goal].x - m_location[u].x;
    //CostType dy = m_location[m_goal].y - m_location[u].y;
    //return ::sqrt(dx * dx + dy * dy);
    return 0;
  }
private:
  //  LocMap m_location;
  Vertex m_goal;
};

struct found_goal {}; // exception for termination

// visitor that terminates when we find the goal
template <class Vertex>
class astar_goal_visitor : public boost::default_astar_visitor
{
public:
  astar_goal_visitor(Vertex goal) : m_goal(goal) {}
  template <class Graph>
  void examine_vertex(Vertex u, Graph& g) {
    std::cout << "Exploring " << vertex_to_position(u) << "..." << std::endl;
    if(u == m_goal)
      throw found_goal();
  }
private:
  Vertex m_goal;
};

struct PredecessorMap {
  PredecessorMap() {}
  PredecessorMap(PredecessorMap const& rhs) : p(rhs.p) {}

  typedef size_t key_type;
  typedef size_t value_type;
  typedef size_t & reference_type;
  typedef boost::read_write_property_map_tag category;

  size_t & operator[](size_t v) { return p[v]; }

  std::map<size_t,size_t> p;
};

size_t get(PredecessorMap const& pm, size_t v) {
  std::map<size_t, size_t>::const_iterator found = pm.p.find(v);
  return (found != pm.p.end()) ? found->second : v;
}

void put(PredecessorMap & pm, size_t key, size_t value) {
    pm.p[key] = value;
}

struct edge_weight_map {
  typedef int value_type;
  typedef value_type reference;
  typedef edge_descriptor key_type;
  typedef boost::readable_property_map_tag category;

  // Edges have a weight equal to the average of their endpoint indexes.
  reference operator[](key_type e) const {
    return 1;
  }
};

typedef boost::property_map<knight_graph,
                            boost::edge_weight_t>::const_type
        const_edge_weight_map;
typedef boost::property_traits<const_edge_weight_map>::reference
        edge_weight_map_value_type;
typedef boost::property_traits<const_edge_weight_map>::key_type
        edge_weight_map_key;


// PropertyMap valid expressions
edge_weight_map_value_type
get(const_edge_weight_map pmap, edge_weight_map_key e) {
  return pmap[e];
}


// ReadablePropertyGraph valid expressions
const_edge_weight_map get(boost::edge_weight_t, const knight_graph&) {
  return const_edge_weight_map();
}

edge_weight_map_value_type get(boost::edge_weight_t tag,
                               const knight_graph& g,
                               edge_weight_map_key e) {
  return get(tag, g)[e];
}


// This expression is not part of a graph concept, but is used to return the
// default vertex index map used by the Dijkstra search algorithm.
boost::identity_property_map get(boost::vertex_index_t, const knight_graph&) {
  // The vertex descriptors are already unsigned integer indices, so just
  // return an identity map.
  return boost::identity_property_map();
}



int main(int argc, char **argv)
{
  using namespace boost;
  std::cout << "Running knight-graph" << std::endl;

  //BOOST_CONCEPT_ASSERT(( BidirectionalGraphConcept<knight_graph> ));
  //  BOOST_CONCEPT_ASSERT(( IncidentGraphConcept<knight_graph> ));
  BOOST_CONCEPT_ASSERT(( VertexListGraphConcept<knight_graph> ));
 

  knight_graph g;

  std::cout << "vertices(g) =\n";
  boost::graph_traits<knight_graph>::vertex_iterator vi, vi_end;
  
  for (tie(vi, vi_end) = vertices(g); vi != vi_end; ++vi) {
    //   std::cout << "Vertex " << vertex_to_position(*vi) << std::endl;
  }

  int v = 50;
  std::cout << "out_degree(v, g) " << out_degree(v, g) << "\n";
  std::cout << vertex_to_position(v) <<  " adjacents\n";
  knight_graph::out_edge_iterator e, e_end;
  for (boost::tie(e, e_end) = out_edges(v, g); e != e_end; ++e) {
    std::cout << "e " << vertex_to_position(*e) << "\n";
  }

  std::vector<vertex_descriptor> p(g.num_vertices());
  std::vector<cost> d(g.num_vertices());
  //  astar_search_no_init(g, 0, distance_heuristic<knight_graph, cost>(10),
  //		       predecessor_map(&p[0]).
  //		       distance_map(&d[0]).
  //		       visitor(astar_goal_visitor<vertex_descriptor>(10)));

  
  knight_graph::vertex_descriptor source = 0;
  std::vector<knight_graph::vertex_descriptor> pred(g.num_vertices());
  std::vector<int> dist(g.num_vertices());

  //  dijkstra_shortest_paths(g, source,
  //			  predecessor_map(&pred[0]).distance_map(&dist[0]));
  std::cout << std::endl;
  return 0;
}

