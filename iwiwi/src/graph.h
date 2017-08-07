#pragma once
#include "base.h"

struct Edge {
  int to;
  int owner, owner2;
  double score;  // Used for some scoring (e.g., betweenness centrality)

  bool Available(int rank) const {
    return owner == -1 || owner == rank;
  }
};

using Graph = vector<vector<Edge>>;

Graph ConstructGraph(const GameState &s);
vector<pair<int, int>> SSSP(const Graph &g, int s);
vector<pair<int, int>> SSSPPlayer(const Graph &g, int s, int rank);

struct UnionFind {  // Union find toha ittenai
  vector<vector<int>> vertices;
  vector<int> root;

  UnionFind(int n = 0) {
    Init(n);
  }

  void Init(int n) {
    vertices.assign(n, vector<int>(1));
    root.resize(n);

    rep (i, n) {
      vertices[i][0] = i;
      root[i] = i;
    }
  }

  void Merge(int v, int w) {
    v = root[v];
    w = root[w];
    if (v == w) return;

    if (vertices[v].size() < vertices[w].size()) swap(v, w);
    for (int x : vertices[w]) {
      vertices[v].emplace_back(x);
      root[x] = v;
    }
    vertices[w].clear();
  }
};

pair<Graph, UnionFind> ConstructContractedGraph(const Graph &g, int rank, bool allow_option = false);
void SingleSourceWeightedBetweenness
(Graph &g, int s, int distance_limit, const vector<double> &weight);
pair<int, int> FindOriginalEdge(int a, int b, const UnionFind &uf, const Graph &original_g, bool allow_option = false);
