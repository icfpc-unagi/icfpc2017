#include "base.h"
#include "random.h"
#include "graph.h"

namespace {
struct MyAIState {
  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {}
};

using MyState = State<MyAIState>;

GameState game;

int N;
Graph G, H;
UnionFind uf;

MyAIState Setup(const GameState &game) {
  return MyAIState();
}

pair<pair<int, int>, MyAIState> Play(const MyState &s) {
  game = s.game;
  G = ConstructGraph(game);
  N = G.size();
  tie(H, uf) = ConstructContractedGraph(G, game.rank);

  int num_edges = game.map.rivers.size();
  int num_remaining_turns = num_edges / game.size - game.turn;  // TODO: more precise

  rep (v, N) for (auto &e : H[v]) e.score = 0.0;
  for (int s : game.map.mines) {
    vector<pair<int, int>> dst1 = SSSP(G, s);

    vector<double> weight(N);
    rep (v, N) weight[uf.root[v]] += dst1[v].first * dst1[v].first;
    SingleSourceWeightedBetweenness(H, uf.root[s], num_remaining_turns, weight);
  }

  tuple<double, int, int> bst(-1, -1, -1);
  rep (v, N) for (const auto &e : H[v]) {
    bst = max(bst, make_tuple(e.score, v, e.to));
  }

  cerr << get<0>(bst) << endl;

  if (get<0>(bst) == -1) {
    return make_pair(RandomRemaining(game), MyAIState());
  } else {

    return make_pair(FindOriginalEdge(get<1>(bst), get<2>(bst), uf, G), MyAIState());
  }
}
}  // namespace

int main() {
  srand(getpid() * time(NULL));
  Run<MyAIState>(Setup, Play);
  return 0;
}
