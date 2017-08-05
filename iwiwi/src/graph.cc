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
  dst[s] = make_pair(0, -1);
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
  dst[s] = make_pair(0, -1);
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
