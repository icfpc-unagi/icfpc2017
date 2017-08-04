#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <stack>
#include <string>
#include <vector>
#include "base/base.h"
#include "json11.hpp"
#include "ninetan/ninestream_util.h"
#include "strings/numbers.h"
#include "strings/strcat.h"

DEFINE_string(map, "", "map file");
DEFINE_string(ai, "", "deprecated; specify AI commands as args");

using json11::Json;
using ninetan::StreamUtil;

typedef string id_type;

template <typename T>
std::pair<T, T> make_sorted_pair(const T& a, const T& b) {
  return a < b ? make_pair(a, b) : make_pair(b, a);
}

class Game {
  vector<string> ais_;
  // Map = {"sites" : [Site], "rivers" : [River], "mines" : [SiteId]}
  // Site = {"id" : SiteId}
  // River = {"source" : SiteId, "target" : SiteId}
  // SiteId = Nat
  Json map_json_;
  vector<int64> site_ids_;
  map<int64, int> site_id_to_index_;
  map<pair<int64, int64>, int> river_to_index_;
  vector<int> mines_;

  vector<Json> states_;
  vector<bool> river_claimed_;
  vector<vector<vector<int>>> punter_river_adj_;
  vector<Json> last_moves_;
  int claimed_;

 public:
  Game(vector<string> ais) : ais_(ais) {}

  void init() {
    string err;
    std::ifstream ifs(FLAGS_map);
    map_json_ = Json::parse(string((std::istreambuf_iterator<char>(ifs)),
                                   std::istreambuf_iterator<char>()),
                            err);
    CHECK(err.empty()) << "loading " << FLAGS_map << ": " << err;
    for (auto site : map_json_["sites"].array_items()) {
      site_ids_.push_back(site["id"].int_value());
    }
    for (int i = 0; i < site_ids_.size(); ++i) {
      site_id_to_index_.emplace(site_ids_[i], i);
    }
    auto rivers = map_json_["rivers"].array_items();
    for (int i = 0; i < rivers.size(); ++i) {
      river_to_index_.emplace(
          make_sorted_pair<int64>(rivers[i]["source"].int_value(),
                                  rivers[i]["target"].int_value()),
          i);
    }
    river_claimed_.resize(river_to_index_.size());
    for (const auto& mine : map_json_["mines"].array_items()) {
      mines_.push_back(site_id_to_index_[mine.int_value()]);
    }

    punter_river_adj_.resize(ais_.size(),
                             vector<vector<int>>(site_ids_.size()));
    states_.resize(ais_.size());
    for (int i = 0; i < ais_.size(); ++i) {
      last_moves_.emplace_back(
          Json::object{{"pass", Json::object{{"punter", 1}}}});
    }
    claimed_ = 0;
  }

  void start() {
    for (int i = 0; i < ais_.size(); ++i) {
      states_[i] = setup(i, ais_.size(), map_json_)["state"];
    }
    for (int turn = 0; turn < river_claimed_.size(); ++turn) {
      for (int i = 0; i < ais_.size(); ++i) {
        auto move_state = gameplay(i, last_moves_, states_[i]);
        auto claim = move_state.first["claim"];
        int64 s = claim["source"].int_value();
        int64 t = claim["target"].int_value();
        auto it = river_to_index_.find(make_sorted_pair(s, t));
        if (it == river_to_index_.end()) {
          LOG(ERROR) << ais_[i] << ": " << claim.dump();
          continue;
        }
        int river_index = it->second;
        if (river_claimed_[river_index]) {
          LOG(WARNING) << "river claimed twice: " << claim.dump();
          last_moves_[i] =
              Json::object{{"pass", Json::object{{"punter", claim["punter"]}}}};
        } else {
          river_claimed_[river_index] = true;
          claimed_++;
          last_moves_[i] = move_state.first;
          int s_i = site_id_to_index_[s];
          int t_i = site_id_to_index_[t];
          punter_river_adj_[i][s_i].push_back(t_i);
          punter_river_adj_[i][t_i].push_back(s_i);
        }
        states_[i] = move_state.second;
      }
      if (claimed_ >= river_claimed_.size()) {
        LOG(INFO) << "All rivers claimed; game ends.";
        break;
      }
    }

    int n = site_ids_.size();
    vector<vector<int>> d(n, vector<int>(n, INT_MAX / 2));
    for (int i = 0; i < n; ++i) d[i][i] = 0;
    for (const auto& r : river_to_index_) {
      d[r.first.first][r.first.second] = 1;
      d[r.first.second][r.first.first] = 1;
    }
    for (int k = 0; k < n; ++k) {
      for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
          d[i][j] = min(d[i][j], d[i][k] + d[k][j]);
        }
      }
    }
    for (int p = 0; p < ais_.size(); ++p) {
      int score = 0;
      for (int m : mines_) {
        vector<bool> visited(n, false);
        std::stack<int> st;
        st.push(m);
        while (!st.empty()) {
          int s = st.top();
          st.pop();
          if (visited[s]) continue;
          visited[s] = true;
          score += d[m][s] * d[m][s];
          for (int t : punter_river_adj_[p][s]) st.push(t);
        }
      }
      LOG(INFO) << ais_[p] << ": score=" << score;
    }
  }

 private:
  static Json io_once(const string& cmd, const Json& in) {
    string id = GetResponseOrDie(StreamUtil::Run(1, cmd)).stream_ids[0];
    string send = in.dump();
    GetResponseOrDie(StreamUtil::Write(id, StrCat(send.size(), ":", send)));
    string recv = GetResponseOrDie(StreamUtil::Read(id, 10000)).data;
    GetResponseOrDie(StreamUtil::Kill(id));
    size_t i = recv.find(':');
    CHECK_NE(i, string::npos) << recv;
    uint32 n;
    CHECK(SimpleAtoi(recv.substr(0, i), &n)) << recv;
    string err;
    Json out = Json::parse(recv.substr(i + 1), err);
    CHECK(err.empty()) << err;
    return out;
  }

  // state
  Json setup(int id, int total, Json map_json) {
    // S → P {"punter" : p, "punters" : n, "map" : map}
    // P → S {"ready" : p, "state" : state}
    int p = id;
    Json send = Json::object{
        {"punter", p}, {"punters", total}, {"map", map_json},
    };
    Json got = io_once(ais_[id], send);
    CHECK_EQ(got["ready"].int_value(), p);
    return got;
  }

  // move, state
  pair<Json, Json> gameplay(int id, const Json::array& moves,
                            const Json& state) {
    // S → P {"punter" : p, "punters" : n, "map" : map}
    // P → S {"ready" : p, "state" : state}
    Json send = Json::object{{"move", Json::object{{"moves", moves}}},
                             {"state", state}};
    Json got = io_once(ais_[id], send);
    // does not support "pass"
    const auto& claim = got["claim"];
    CHECK_EQ(claim["punter"].int_value(), id);
    CHECK(claim["source"].is_number());
    CHECK(claim["target"].is_number());
    return make_pair(Json(Json::object{{"claim", got["claim"]}}), got["state"]);
  }
};

int main(int argc, char** argv) {
  ParseCommandLineFlags(&argc, &argv);
  if (FLAGS_map.empty()) {
    puts("required --map");
    return 0;
  }

  vector<string> ais;
  if (!FLAGS_ai.empty()) ais.push_back(FLAGS_ai);
  for (int i = 1; i < argc; ++i) ais.push_back(argv[i]);

  Game game(ais);
  game.init();
  game.start();

  StreamUtil::Exit();
  return 0;
}
