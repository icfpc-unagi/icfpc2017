#include "base.h"

struct MyAIState {
  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {}
};

using MyState = State<MyAIState>;

GameState game_state;

pair<int, int> Solve() {
  set<pair<int, int>> usd;
  for (auto &&claim : game_state.claims) {
    int a = claim.target;
    int b = claim.source;
    if (a > b) swap(a, b);
    usd.emplace(a, b);
  }

  for (auto &&river : game_state.map.rivers) {
    int a = river.first;
    int b = river.second;
    if (a > b) swap(a, b);
    if (!usd.count(make_pair(a, b))) {
      return make_pair(a, b);
    }
  }
}

int main() {
  // Input
  std::string stdin((std::istreambuf_iterator<char>(cin)),
                    std::istreambuf_iterator<char>());
  string err;
  auto j = json11::Json::parse(stdin, err);
  assert(err.empty());
  MyState s = GetState<MyAIState>(j);
  game_state = s.game;

  // Solve
  pair<int, int> res = Solve();

  // Output
  json11::Json out_json = json11::Json::object {
    { "claim", json11::Json::object {
        { "punter", s.game.rank },
        { "source", s.game.map.sites[res.first].id },
        { "target", s.game.map.sites[res.second].id },
      }, },
    { "state", DumpState(s) },
  };
  cout << out_json.dump() << endl;

  return 0;
}
