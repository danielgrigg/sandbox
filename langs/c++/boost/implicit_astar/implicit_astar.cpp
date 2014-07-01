/**
 * Example use of boost::astar_search_no_init on an infinite, implicitly-defined graph.
 *
 * The graph type used here is XYGraph, representing an infinite grid of squares.  Each
 * square is connected to its eight neighbors; however, the example shows how to use
 * boost::filtered_graph to make the search take place only along orthogonal edges.
 */

#include <iostream>
#include <list>
#include <map>
#include <set>
#include <utility>

#include <boost/graph/graph_traits.hpp>
#include <boost/graph/astar_search.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/operators.hpp>
#include <boost/ref.hpp>

namespace Direction
{
enum id
{
    MIN = 0,
    N = MIN, S, E, W, NW, NE, SE, SW, NONE
};
}

struct XY : public boost::additive<XY,
    boost::totally_ordered<XY,
    boost::equivalent<XY>
    > >
{
    typedef int X;
    typedef int Y;

    XY(X x = 0, Y y = 0);

    // Same square counts.
    bool adjacentTo(XY const& that) const;

    XY & operator=(XY const& that);
    XY & operator+=(XY const& that);

    bool operator<(XY const& that) const;

    X x;
    Y y;

    XY neighbor(Direction::id direction) const;
    std::set<XY> allNeighbors() const;
};

std::ostream & operator<<(std::ostream & os, XY const& xy);

struct neighbor_iterator;

/*
 * Model of:
 *  * Graph
 *  * IncidenceGraph
 */
struct XYGraph
{
    XYGraph();

    // Graph concept requirements
    typedef XY                                vertex_descriptor;
    typedef std::pair<XY, XY>                 edge_descriptor;
    typedef boost::undirected_tag             directed_category;
    typedef boost::disallow_parallel_edge_tag edge_parallel_category;
    typedef boost::incidence_graph_tag        traversal_category;

    // IncidenceGraph concept requirements
    typedef neighbor_iterator          out_edge_iterator;
    typedef int                        degree_size_type;
};

namespace boost
{
    template <> struct graph_traits<XYGraph>
    {
        typedef XYGraph G;

        typedef G::vertex_descriptor      vertex_descriptor;
        typedef G::edge_descriptor        edge_descriptor;
        typedef G::out_edge_iterator      out_edge_iterator;

        typedef G::directed_category      directed_category;
        typedef G::edge_parallel_category edge_parallel_category;
        typedef G::traversal_category     traversal_category;

        typedef G::degree_size_type       degree_size_type;

        typedef void in_edge_iterator;
        typedef void vertex_iterator;
        typedef void vertices_size_type;
        typedef void edge_iterator;
        typedef void edges_size_type;
    };
}

// IncidenceGraph concept requirements
std::pair<XYGraph::out_edge_iterator, 
XYGraph::out_edge_iterator> out_edges(XYGraph::vertex_descriptor v, XYGraph const& g);
XYGraph::degree_size_type out_degree(XYGraph::vertex_descriptor v, XYGraph const& g);
XYGraph::vertex_descriptor source(XYGraph::edge_descriptor e, XYGraph const& g);
XYGraph::vertex_descriptor target(XYGraph::edge_descriptor e, XYGraph const& g);

// Iterator
struct neighbor_iterator : 
    public boost::iterator_facade<neighbor_iterator,
                                  std::pair<XY,XY>,
                                  boost::forward_traversal_tag,
                                  std::pair<XY,XY> >
{
public:
    neighbor_iterator();
    neighbor_iterator(XY xy, Direction::id direction);

    neighbor_iterator & operator=(neighbor_iterator const& that);

    std::pair<XY,XY> operator*() const;
    void operator++();
    bool operator==(neighbor_iterator const& that) const;

    bool equal(neighbor_iterator const& that) const { return operator==(that); }
    void increment() { operator++(); }

private:
    XY xy;
    Direction::id direction;
};


struct orthogonal_only;
template <typename Graph> class distance_heuristic;

struct found_goal {}; // exception for termination

// visitor that terminates when we find the goal
class astar_goal_visitor : public boost::default_astar_visitor
{
public:
    astar_goal_visitor(XY goal) : m_goal(goal) {}

    virtual void examine_vertex(XY xy, XYGraph const& g) {
        std::cout << "Exploring " << xy << "..." << std::endl;
        if(xy == m_goal)
            throw found_goal();
    }
    virtual void examine_vertex(XY xy, boost::filtered_graph<XYGraph, orthogonal_only> const& g) {
        std::cout << "Exploring " << xy << "..." << std::endl;
        if(xy == m_goal)
            throw found_goal();
    }
private:
    XY m_goal;
};

template <typename K, typename V>
class default_map
{
public:
    typedef K key_type;
    typedef V data_type;
    typedef std::pair<K,V> value_type;

    default_map(V const& defaultValue)
        : defaultValue(defaultValue)
    {}

    V & operator[](K const& k)
    {
        if (m.find(k) == m.end())
        {
            m[k] = defaultValue;
        }
        return m[k];
    }

private:
    std::map<K,V> m;
    V const defaultValue;
};

struct PredecessorMap
{
    PredecessorMap() {}
    PredecessorMap(PredecessorMap const& that) : m(that.m) {}

    typedef XY key_type;
    typedef XY value_type;
    typedef XY & reference_type;
    typedef boost::read_write_property_map_tag category;

    XY & operator[](XY xy) { return m[xy]; }

    std::map<XY,XY> m;
};

XY get(PredecessorMap const& pm, XY xy)
{
    std::map<XY,XY>::const_iterator found = pm.m.find(xy);
    return (found != pm.m.end()) ? found->second : xy;
}

void put(PredecessorMap & pm, XY key, XY value)
{
    pm.m[key] = value;
}

// Filter used to traverse grid only along orthogonal (non-diagonal) edges.
struct orthogonal_only
{
    typedef std::pair<XY,XY> Edge;
    bool operator()(Edge const& edge) const
    {
        return edge.first.x == edge.second.x || edge.first.y == edge.second.y;
    }
};

// Euclidean distance heuristic (square root omitted)
template <typename Graph>
class distance_heuristic : public boost::astar_heuristic<Graph, int>
{
public:
    distance_heuristic(XY goal)
        : m_goal(goal) {}
    unsigned operator()(XY xy)
    {
        int dx = m_goal.x - xy.x;
        int dy = m_goal.y - xy.y;
        unsigned retval = static_cast<unsigned>(dx * dx + dy * dy);
        return retval;
    }
private:
    XY m_goal;
};

int main(int argc, char **argv)
{
    XYGraph baseGraph;
    boost::filtered_graph<XYGraph, orthogonal_only> g(baseGraph, orthogonal_only());
    //BOOST_CONCEPT_ASSERT((IncidenceGraphConcept< boost::filtered_graph<XYGraph, orthogonal_only> >));

    XY start(0,0);
    XY goal(5,7);

    std::cout << "Start vertex: " << start << std::endl;
    std::cout << "Goal vertex: " << goal << std::endl;

    PredecessorMap p;
    typedef boost::associative_property_map< default_map<XY,unsigned> > DistanceMap;
    typedef default_map<XY,unsigned> WrappedDistanceMap;
    WrappedDistanceMap wrappedMap =         WrappedDistanceMap(std::numeric_limits<unsigned>::max());
    wrappedMap[start] = 0;
    DistanceMap d = DistanceMap(wrappedMap);

    try {
        astar_search_no_init(g, 
            start,
            distance_heuristic<XYGraph>(goal)
            , visitor(astar_goal_visitor(goal))
            . distance_map(d)
            . predecessor_map(boost::ref(p))
            . weight_map(boost::associative_property_map< default_map<std::pair<XY,XY>,unsigned> >(
                default_map<std::pair<XY,XY>,unsigned>(1)))
            . vertex_index_map(boost::associative_property_map< std::map<XY,unsigned> >(std::map<XY,unsigned>()))
            . rank_map(boost::associative_property_map< std::map<XY,unsigned> >(std::map<XY,unsigned>()))
            . color_map(boost::associative_property_map< std::map<XY,boost::default_color_type> >(
                std::map<XY,boost::default_color_type>()))
            . distance_compare(std::less<unsigned>())
            . distance_combine(std::plus<unsigned>())
            );
    } catch(found_goal const&) { // found a path to the goal
        std::list<XY> shortest_path;
        for(XY xy = goal;; xy = p[xy]) {
            shortest_path.push_front(xy);
            if(p[xy] == xy)
                break;
        }
        std::cout << "Shortest path from " << start << " to "
            << goal << ": ";
        std::list<XY>::iterator spi = shortest_path.begin();
        std::cout << start;
        for(++spi; spi != shortest_path.end(); ++spi) 
            std::cout << " -> " << (*spi);
        std::cout << std::endl;
        return 0;
    }

    std::cout << "Didn't find a path from " << start << "to"
        << goal << "!" << std::endl;
    return 0;
}

XYGraph::XYGraph()
{}

std::pair<XYGraph::out_edge_iterator, XYGraph::out_edge_iterator> 
out_edges(XYGraph::vertex_descriptor v,
          XYGraph const& g)
{
    return std::make_pair(
        XYGraph::out_edge_iterator(v, Direction::MIN), 
        XYGraph::out_edge_iterator(v, Direction::NONE) );
}

XYGraph::degree_size_type 
out_degree(XYGraph::vertex_descriptor v,
           XYGraph const& g)
{
    return v.allNeighbors().size();
}

XYGraph::vertex_descriptor 
source(XYGraph::edge_descriptor e,
       XYGraph const& g)
{
    return e.first;
}

XYGraph::vertex_descriptor target(
    XYGraph::edge_descriptor e,
    XYGraph const& g)
{
    return e.second;
}

neighbor_iterator::neighbor_iterator()
{}

neighbor_iterator::neighbor_iterator(XY xy, Direction::id direction)
: xy(xy)
, direction(direction)
{
}

neighbor_iterator & neighbor_iterator::operator=(neighbor_iterator const& that)
{
    xy = that.xy;
    direction = that.direction;
    return *this;
}

std::pair<XY,XY> neighbor_iterator::operator*() const
{
    std::pair<XY,XY> const retval = std::make_pair(xy, xy.neighbor(direction));
    return retval;
}

void neighbor_iterator::operator++()
{
    direction = static_cast<Direction::id>(int(direction) + 1);
}

bool neighbor_iterator::operator==(neighbor_iterator const& that) const
{
    return xy == that.xy && direction == that.direction;
}


XY::XY(X x, Y y)
: x(x)
, y(y)
{
}

bool XY::adjacentTo(XY const& that) const
{
    return abs(x - that.x) <= 1 && abs(y - that.y) <= 1;
}

XY & XY::operator=(XY const& that)
{
    x = that.x;
    y = that.y;
    return *this;
}

XY & XY::operator+=(XY const& that)
{
    x += that.x;
    y += that.y;
    return *this;
}

bool XY::operator<(XY const& that) const
{
    return x < that.x || (x == that.x && y < that.y);
}

std::ostream & operator<<(std::ostream & os, XY const& xy)
{
    os << "(" << xy.x << "," << xy.y << ")";
    return os;
}

XY XY::neighbor(Direction::id direction) const
{
    using namespace Direction;

    int dx = 0, dy = 0;
    switch (direction)
    {
    case NW:
    case W:
    case SW:
        dx = -1;
        break;
    case NE:
    case E:
    case SE:
        dx = 1;
    }
    switch (direction)
    {
    case NW:
    case N:
    case NE:
        dy = -1;   
        break;
    case SW:
    case S:
    case SE:
        dy = 1;
    }
    XY const neighbor(x + dx, y + dy);
    return neighbor;
}

std::set<XY> XY::allNeighbors() const
{
    std::set<XY> neighbors;

    for (int dx = -1; dx <= 1; ++dx)
        for (int dy = -1; dy <= 1; ++dy)
            neighbors.insert(XY(x+dx,y+dy));

    return neighbors;
}

