#include "graph.h"

Graph ConstructGraph(const GameState &s) {
  int n = s.map.sites.size();
  Graph g(n);

  map<pair<int, int>, int> river_to_owner;
  for (auto &&claim: s.claims) {
    int a = claim.source, b = claim.target;
    if (a > b) swap(a, b);
    river_to_owner[make_pair(a, b)] = claim.rank;
  }

  for (auto &&river: s.map.rivers) {
    int a = river.first, b = river.second, o = -1;
    if (a > b) swap(a, b);
    if (river_to_owner.count(make_pair(a, b))) {
      o = river_to_owner[make_pair(a, b)];
    }

    g[a].emplace_back(Edge{b, o});
    g[b].emplace_back(Edge{a, o});
  }

  return g;
}


// (distance, previous vertex)
vector<pair<int, int>> SSSP(const Graph &g, int s) {
  vector<pair<int, int>> dst(g.size(), make_pair(INT_MAX, -1));
  queue<int> que;
  dst[s] = make_pair(0, -2);
  que.push(s);

  while (!que.empty()) {
    int v = que.front();
    int d = dst[v].first;
    que.pop();

    for (auto &&e : g[v]) {
      int tv = e.to;
      int td = d + 1;
      if (dst[tv].second != -1) continue;
      dst[tv] = make_pair(td, v);
      que.push(tv);
    }
  }

  return dst;
}

vector<pair<int, int>> SSSPPlayer(const Graph &g, int s, int rank) {
  vector<pair<int, int>> dst(g.size(), make_pair(INT_MAX, -1));
  deque<pair<int, int>> que;
  dst[s] = make_pair(0, -2);
  que.push_back(make_pair(s, 0));

  while (!que.empty()) {
    int v = que.front().first;
    int d = que.front().second;
    que.pop_front();
    if (d > dst[v].first) continue;

    for (auto &&e : g[v]) {
      int tv = e.to, td;
      if (e.owner == -1) td = d + 1;
      else if (e.owner == rank) td = d;
      else continue;  // Owned by others

      if (td < dst[tv].first) {
        dst[tv] = make_pair(td, v);
        if (d == td) que.push_front(make_pair(tv, td));
        else que.push_back(make_pair(tv, td));
      }
    }
  }

  return dst;
}

pair<Graph, UnionFind> ConstructContractedGraph(const Graph &g, int rank) {
  int n = g.size();
  UnionFind uf(n);
  rep (v, n) for (const auto &e : g[v]) {
    if (e.owner == rank) uf.Merge(v, e.to);
  }

  set<pair<int, int>> es;
  rep (v, n) for (const auto &e : g[v]) {
    if (e.owner == -1) {
      int a = uf.root[v];
      int b = uf.root[e.to];
      if (a == b) continue;
      es.emplace(min(a, b), max(a, b));
    }
  }

  Graph h(n);
  for (const auto &e : es) {
    h[e.first ].emplace_back(Edge{e.second, -1});
    h[e.second].emplace_back(Edge{e.first, -1});
  }
  return make_pair(h, uf);
}

// NOTE: Give me contracted graph!!!
void SingleSourceWeightedBetweenness
(Graph &g, int s, int distance_limit, const vector<double> &weight) {
  // https://github.com/flowlight0/fully-dynamic-betweenness-centrality/blob/master/src/algorithm/centrality_brandes.cpp
  int V = g.size();
  auto &forward_adj = g;

  vector<int>    distance(V, -1);
  vector<double> num_paths(V, 0);
  vector<double> delta(V, 0);
  vector<int>    order;
  queue<int>     que;

  distance[s] = 0;
  num_paths[s] = 1;
  que.push(s);

  while (!que.empty()){
    int v = que.front();
    que.pop();
    order.push_back(v);

    if (distance[v] >= distance_limit) continue;

    for (const auto &e : forward_adj[v]){
      int w = e.to;
      if (distance[w] == -1){
        distance[w] = distance[v] + 1;
        que.push(w);
      }

      if (distance[w] == distance[v] + 1){
        num_paths[w] += num_paths[v];
      }
    }
  }

  rep (v, V) if (num_paths[v] > 0) cerr << v << " " << distance[v] << " " << num_paths[v] << "; ";
  cerr << endl;

  reverse(order.begin(), order.end());
  for (int v : order){
    for (auto &e : forward_adj[v]) {
      int w = e.to;
      if (distance[w] == distance[v] + 1 && num_paths[w] > 0){
        // delta[v] += num_paths[v] / num_paths[w];
        // delta[v] += num_paths[v] / num_paths[w] * delta[w];

        double val = 0;
        val += num_paths[v] / num_paths[w] * weight[w];
        val += num_paths[v] / num_paths[w] * delta[w];
        e.score += val;
        delta[v] = val;
      }
    }
  }
}

pair<int, int> FindOriginalEdge(int a, int b, const UnionFind &uf, const Graph &original_g) {
  set<pair<int, int>> es;
  rep (v, original_g.size()) for (const auto &e : original_g[v]) {
    if (e.owner == -1) es.emplace(min(v, e.to), max(v, e.to));
  }

  for (int v : uf.vertices[a]) {
    for (int w : uf.vertices[b]) {
      if (es.count(make_pair(min(v, w), max(v, w)))) {
        return make_pair(v, w);
      }
    }
  }
}
