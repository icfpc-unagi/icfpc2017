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
Graph G;

MyAIState Setup(const GameState &game) {
  return MyAIState();
}

pair<pair<int, int>, MyAIState> Play(const MyState &s) {
  game = s.game;
  G = ConstructGraph(game);
  N = G.size();

  int num_edges = game.map.rivers.size();
  int num_remaining_turns = num_edges / game.size - game.turn;  // TODO: more precise

  pair<pair<int, int>, pair<int, int>> bst{{-1, -1}, {-1, -1}};

  for (int s : game.map.mines) {
    auto dst1 = SSSP(G, s);
    auto dst2 = SSSPPlayer(G, s, game.rank);

    rep (v, N) {
      if (dst2[v].first > num_remaining_turns) continue;  // Unreachable anymore
      if (dst2[v].first == 0) continue;  // Already reaching

      pair<int, int> tmp = make_pair(dst1[v].first, - dst2[v].first);
      if (tmp > bst.first) {
        bst.first = tmp;

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

  cerr <<
      "Real distance: " << bst.first.first << ", " <<
      "My distance: " << -bst.first.second << ", " <<
      "Total turns: " << num_edges / game.size << ", " <<
      "Current turn: " << game.turn << endl;

  if (bst.first.first != -1) {
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
