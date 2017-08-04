#include "base.h"
#include "random.h"

struct MyAIState {
  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {}
};

using MyState = State<MyAIState>;

pair<int, int> Solve(const MyState &s) {
  return RandomRemaining(s.game);
}

int main() {
  srand(getpid() * time(NULL));

  // Input
  std::string stdin((std::istreambuf_iterator<char>(cin)),
                    std::istreambuf_iterator<char>());
  string err;
  auto j = json11::Json::parse(stdin, err);
  assert(err.empty());
  MyState s = GetState<MyAIState>(j);

  // Solve
  pair<int, int> res = Solve(s);

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
