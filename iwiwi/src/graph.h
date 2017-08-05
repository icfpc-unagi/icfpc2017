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


struct UnionFind {
  vector<int> par, siz;

  UnionFind(int n) : par(n), siz(n) {
    rep (i, n) {
      par[i] = i;
      siz[i] = 1;
    }
  }

  int Root(int v) {
    return par[v] == v ? v : par[v] = Root(par[v]);
  }

  void Merge(int v, int w) {
    v = Root(v);
    w = Root(w);
    if (v == w) continue;

    if (siz[v] < siz[w]) swap(v, w);
    par[w] = v;
    siz[v] += siz[w];
  }

  int Size(int v) {
    return siz[v];
  }
};
