#pragma once
#include "base.h"

struct Edge {
  int to;
  int owner;

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

  UnionFind(int n) : vertices(n, vector<int>(1)), root(n) {
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
}

/*
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
*/
