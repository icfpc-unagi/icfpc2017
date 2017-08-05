#pragma once

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <stack>
#include <algorithm>
#include <queue>
#include <set>
#include <map>
#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cmath>
#include <cassert>
#include <unordered_set>
#include <unordered_map>
#include <fstream>
#include <numeric>

#include <boost/serialization/serialization.hpp>
#include <boost/serialization/vector.hpp>
#include <boost/serialization/string.hpp>
#include <boost/serialization/utility.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include "json11/json11.hpp"
using namespace std;

#define all(c) (c).begin(), (c).end()
#define iter(c) __typeof((c).begin())
#define cpresent(c, e) (find(all(c), (e)) != (c).end())
#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define tr(c, i) for (iter(c) i = (c).begin(); i != (c).end(); ++i)
#define pb(e) push_back(e)
#define mp(a, b) make_pair(a, b)

//
// Map
//

struct Site {
  int id;
  double x, y;
  bool is_mine;

  template<class Archive> void serialize(Archive& ar, unsigned int ver) {
    ar & id & is_mine;
  }
};

struct Map {
  vector<Site> sites;
  vector<pair<int, int>> rivers;
  vector<int> mines;

  template<class Archive> void serialize(Archive& ar, unsigned int ver) {
    ar & sites & rivers & mines;
  }
};

Map ConstructMap(const json11::Json &j);
map<int, int> ConstructIdToIndexMap(const Map &s);

//
// GameState
//

struct Claim {
  int rank, source, target;

  template<class Archive> void serialize(Archive& ar, unsigned int ver) {
    ar & rank & source & target;
  }
};

struct GameState {
  // Game info given at the first step
  int rank, size;  // (punter & punters are just too dangerous)
  Map map;
  int turn;

  // Previous moves
  vector<Claim> claims;

  template<class Archive> void serialize(Archive& ar, unsigned int ver) {
    ar & rank & size & map & turn & claims;
  }
};

GameState ConstructGameState(const json11::Json &j);



//
// State
//

template<typename AIState>
struct State {
  GameState game;
  AIState ai;

  template<class Archive> void serialize(Archive& ar, unsigned int ver) {
    ar & game & ai;
  }
};

template<typename AIState>
State<AIState> GetState(const json11::Json &j) {
  State<AIState> s;
  // Restore
  istringstream iss(j["state"].string_value());
  boost::archive::text_iarchive ia(iss);
  ia >> s;

  // TODO: check move or score

  // Advance
  map<int, int> id_to_idx = ConstructIdToIndexMap(s.game.map);

  s.game.turn += 1;
  auto moves = j["move"]["moves"].array_items();
  for (auto &&move : moves) {
    if (move["claim"].is_null()) continue;
    auto jj = move["claim"];
    s.game.claims.emplace_back
        (Claim{jj["punter"].int_value(),
              id_to_idx[jj["source"].int_value()],
              id_to_idx[jj["target"].int_value()]});
  }
  return s;
}

template<typename State>
string DumpState(const State &s) {
  ostringstream oss;
  boost::archive::text_oarchive oa(oss);
  oa << s;
  return oss.str();
}

//
// IO
//
json11::Json InputJSON();
void OutputJSON(const json11::Json &json);
bool IsSetup(const json11::Json &json);

//
// Entry point
//


template<typename AIState, typename SetupFunc, typename PlayFunc>
void Run(SetupFunc setup, PlayFunc play) {
  using MyState = State<AIState>;

  // Input
  json11::Json in_json = InputJSON(), out_json;

  if (IsSetup(in_json)) {
    // Setup
    MyState s;
    s.game = ConstructGameState(in_json);
    s.ai = setup(s.game);
    out_json = json11::Json::object{
      {"ready", s.game.rank},
      {"state", DumpState(s)},
    };
  } else {
    // Play
    MyState s = GetState<AIState>(in_json);
    pair<pair<int, int>, AIState> res = play(s);
    s.ai = res.second;

    out_json = json11::Json::object{
      {"claim", json11::Json::object{
          {"punter", s.game.rank},
          {"source", s.game.map.sites[res.first.first].id},
          {"target", s.game.map.sites[res.first.second].id}}},
      {"state", DumpState(s) }};
  }

  // Output
  OutputJSON(out_json);
}
