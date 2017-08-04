#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
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

class Game {
  vector<string> ais_;
  // Map = {"sites" : [Site], "rivers" : [River], "mines" : [SiteId]}
  // Site = {"id" : SiteId}
  // River = {"source" : SiteId, "target" : SiteId}
  // SiteId = Nat
  Json map_json_;
  vector<int64> site_ids_;
  map<pair<int64, int64>, int> river_to_index_;

  vector<Json> states_;
  vector<bool> river_claimed_;
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
      site_ids_.push_back(site["id"].number_value());
    }
    const auto& rivers = map_json_["rivers"].array_items();
    for (int i = 0; i < rivers.size(); ++i) {
      int64 s = rivers[i]["source"].number_value();
      int64 t = rivers[i]["target"].number_value();
      if (s < t) {
        river_to_index_.emplace(make_pair(s, t), i);
      } else {
        river_to_index_.emplace(make_pair(t, s), i);
      }
    }
    river_claimed_.resize(rivers.size());

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
    for (int i = 0; i < river_claimed_.size(); ++i) {
      for (int i = 0; i < ais_.size(); ++i) {
        auto move_state = gameplay(i, last_moves_, states_[i]);
        auto claim = move_state.first["claim"];
        int64 s = claim["source"].int_value();
        int64 t = claim["target"].int_value();
        auto it =
            river_to_index_.find(s < t ? make_pair(s, t) : make_pair(t, s));
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
        }
        states_[i] = move_state.second;
      }
      if (claimed_ >= river_claimed_.size()) {
        LOG(INFO) << "All rivers claimed; game ends.";
        break;
      }
    }
    LOG(INFO) << Json(Json::object{
                          {"stop", Json::object{{"moves", last_moves_},
                                                {"score", "<unimplemented>"}}}})
                     .dump();
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
