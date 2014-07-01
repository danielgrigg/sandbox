/*
 * \file knight_solver.h
 * The KnightSolver lives here!
 *
 * \author Daniel Grigg
 */

#ifndef KT_KNIGHT_SOLVER_H
#define KT_KNIGHT_SOLVER_H

#include <utility>
#include "kt.h"
#include <vector>
#include <boost/graph/adjacency_list.hpp>
#include <tr1/memory>

namespace kt {

  class Position;

  /*
   * \brief Compute path-finding problems for Knight pieces."
   */
  class KnightSolver {
    public:
      typedef boost::adjacency_list <boost::vecS, boost::vecS, boost::directedS,
              boost::no_property, boost::property<boost::edge_weight_t, int> > graph_t;
      typedef std::tr1::shared_ptr<graph_t> graph_t_ptr;
      typedef boost::graph_traits < graph_t >::vertex_descriptor vertex_descriptor;
      typedef boost::graph_traits < graph_t >::edge_descriptor edge_descriptor;

      KnightSolver();

      /**
       * @brief Find the shortest-path between two points."
       */
      bool shortest_path(const Position& start_at, 
          const Position& end_at,
          std::vector<Position>& path);

      /* @brief Given an start-point, end-point pair of chess-notation coordinates,
       *        generate the shortest path between them as a string of 
       *        chess-notation coordinates.
       */
      bool knight_travail(const std::string& input, std::string& output);


    private:
      graph_t_ptr _g;
  };


}

#endif

