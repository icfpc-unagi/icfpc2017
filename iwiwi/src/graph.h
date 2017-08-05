#pragma once
#include "base.h"

struct Edge {
  int to;
  int owner;
};

using Graph = vector<vector<Edge>>;

Graph ConstructGraph(const GameState &s);
vector<pair<int, int>> SSSP(const Graph &g, int s);
vector<pair<int, int>> SSSPPlayer(const Graph &g, int s, int rank);
