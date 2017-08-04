#include "random.h"

pair<int, int> RandomRemaining(const GameState &game_state) {
  set<pair<int, int>> usd;
  for (auto &&claim : game_state.claims) {
    int a = claim.target;
    int b = claim.source;
    if (a > b) swap(a, b);
    usd.emplace(a, b);
  }

  vector<int> ord(game_state.map.rivers.size());
  iota(all(ord), 0);
  random_shuffle(all(ord));

  for (int i : ord) {
    const auto &river = game_state.map.rivers[i];
    int a = river.first;
    int b = river.second;
    if (a > b) swap(a, b);
    if (!usd.count(make_pair(a, b))) {
      return make_pair(a, b);
    }
  }
  assert(false);
}
