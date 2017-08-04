#include "base.h"
#include "random.h"

namespace {
struct MyAIState {
  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {}
};

using MyState = State<MyAIState>;

MyAIState Setup(const GameState &game) {
  return MyAIState();
}

pair<pair<int, int>, MyAIState> Play(const MyState &s) {
  return make_pair(RandomRemaining(s.game), MyAIState());
}
}  // namespace

int main() {
  srand(getpid() * time(NULL));
  Run<MyAIState>(Setup, Play);
  return 0;
}
