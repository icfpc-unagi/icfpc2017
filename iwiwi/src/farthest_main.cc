#include "base.h"
#include "random.h"

//
// Graph (prototype)
//

struct Edge {
  int to;
  int owner;
};

using Graph = vector<vector<Edge>>;

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

//
// AI
//

namespace {
struct MyAIState {
  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {}
};

using MyState = State<MyAIState>;

GameState game;

int N;
Graph G;

MyAIState Setup(const GameState &game) {
  return MyAIState();
}

pair<pair<int, int>, MyAIState> Play(const MyState &s) {
  game = s.game;
  G = ConstructGraph(game);
  N = G.size();

  // rep (v, N) for (auto &e : G[v]) cerr << e.to << " " << e.owner << endl;

  int num_edges = game.map.rivers.size();
  int num_remaining_turns = num_edges / game.size - game.turn;  // TODO: more precise

  pair<int, pair<int, int>> bst{-1, {-1, -1}};

  for (int s : game.map.mines) {
    auto dst1 = SSSP(G, s);
    auto dst2 = SSSPPlayer(G, s, game.rank);

    rep (v, N) {
      if (dst2[v].first > num_remaining_turns) continue;  // Unreachable anymore
      if (dst2[v].first == 0) continue;  // Already reaching

      if (dst1[v].first > bst.first) {
        bst.first = dst1[v].first;

        int x = v, y;
        for (;;) {
          y = dst2[x].second;
          if (dst2[x].first == 1 && dst2[y].first == 0) break;
          x = y;
        }
        bst.second = make_pair(x, y);
      }
    }
  }
  cerr << bst.first << endl;

  if (bst.first != -1) {
    return make_pair(bst.second, MyAIState());
  } else {
    return make_pair(RandomRemaining(game), MyAIState());
  }
}
}  // namespace

int main() {
  srand(getpid() * time(NULL));
  Run<MyAIState>(Setup, Play);
  return 0;
}
