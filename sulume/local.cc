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

DEFINE_int32(timeout, 10000, "timeout in millisecs");
DEFINE_string(map, "", "map file");
DEFINE_string(dot, "", "output dot file");
DEFINE_bool(dot_all, false, "output dot for all steps");
DEFINE_double(scale, 5.0, "dot scale");
DEFINE_bool(futures, true, "enable futures extension");
DEFINE_string(listener, "", "step listener");

using json11::Json;
using ninetan::StreamUtil;

typedef string id_type;

template <typename T>
std::pair<T, T> make_sorted_pair(const T& a, const T& b) {
  return a < b ? make_pair(a, b) : make_pair(b, a);
}

const char* const kColorPalette[] = {
    "blue", "green", "brown", "pink", "cyan", "violet", "gold", "orange",
};

// foo.bar -> foo-1.bar
string part_filename(const string& file, int part) {
  size_t p = file.rfind('.');
  return p == string::npos
             ? StrCat(file, "-", part)
             : StrCat(StringPiece(file, 0, p), "-", part, StringPiece(file, p));
}

class Game {
  vector<string> ais_;
  string listener_id = "";
  // Map = {"sites" : [Site], "rivers" : [River], "mines" : [SiteId]}
  // Site = {"id" : SiteId}
  // River = {"source" : SiteId, "target" : SiteId}
  // SiteId = Nat
  Json map_json_;
  vector<int64> site_ids_;
  vector<double> site_x_;
  vector<double> site_y_;
  map<int64, int> site_id_to_index_;
  map<pair<int64, int64>, int> river_to_index_;
  vector<int> mines_;
  vector<map<int, int>> futures_;

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
      if (site["x"].is_number() && site["y"].is_number()) {
        site_x_.push_back(site["x"].number_value());
        site_y_.push_back(site["y"].number_value());
      }
    }
    if (site_x_.size() != site_ids_.size() ||
        site_y_.size() != site_ids_.size()) {
      site_x_.clear();
      site_y_.clear();
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
    futures_.resize(ais_.size());
    states_.resize(ais_.size());
    for (int i = 0; i < ais_.size(); ++i) {
      last_moves_.emplace_back(
          Json::object{{"pass", Json::object{{"punter", 1}}}});
    }
    claimed_ = 0;
    if (!FLAGS_listener.empty()) {
      listener_id =
          GetResponseOrDie(StreamUtil::Run(1, FLAGS_listener)).stream_ids[0];
    }
  }

  void start() {
    for (int i = 0; i < ais_.size(); ++i) {
      states_[i] = setup(i, ais_.size(), map_json_)["state"];
    }
    if (!listener_id.empty()) {
      GetResponseOrDie(StreamUtil::Write(
          listener_id, setup_json(-1, ais_.size(), map_json_).dump()));
    }
    for (int turn = 0; turn < river_to_index_.size(); ++turn) {
      int i = turn % ais_.size();
      auto move_state = gameplay(i, last_moves_, states_[i]);
      auto claim = move_state["claim"];
      int64 s = claim["source"].int_value();
      int64 t = claim["target"].int_value();
      auto it = river_to_index_.find(make_sorted_pair(s, t));
      if (it == river_to_index_.end()) {
        LOG(ERROR) << "invalid river [" << ais_[i] << "]: " << claim.dump();
        last_moves_[i] = Json::object{{"pass", Json::object{{"punter", i}}}};
      } else if (river_claimed_[it->second]) {
        LOG(ERROR) << "river claimed twice [" << ais_[i]
                   << "]: " << claim.dump();
        last_moves_[i] = Json::object{{"pass", Json::object{{"punter", i}}}};
      } else {
        river_claimed_[it->second] = true;
        int s_i = site_id_to_index_[s];
        int t_i = site_id_to_index_[t];
        punter_river_adj_[i][s_i].push_back(t_i);
        punter_river_adj_[i][t_i].push_back(s_i);
        last_moves_[i] = Json::object{{"claim", move_state["claim"]}};
      }
      states_[i] = move_state["state"];
      if (!FLAGS_dot.empty() && FLAGS_dot_all) {
        gen_dot(part_filename(FLAGS_dot, turn));
      }
      if (!listener_id.empty()) {
        GetResponseOrDie(StreamUtil::Write(listener_id, last_moves_[i].dump()));
      }
    }
    LOG(INFO) << "game ends.";

    score();
    if (!FLAGS_dot.empty()) {
      gen_dot(FLAGS_dot);
    }
  }

 private:
  static Json io_once(const string& cmd, const Json& in) {
    string id = GetResponseOrDie(StreamUtil::Run(1, cmd)).stream_ids[0];
    string send = in.dump();
    GetResponseOrDie(StreamUtil::Write(id, StrCat(send.size(), ":", send)));
    string recv = GetResponseOrDie(StreamUtil::Read(id, FLAGS_timeout)).data;
    GetResponseOrDie(StreamUtil::Kill(id));
    size_t i = recv.find(':');
    CHECK_NE(i, string::npos) << "missing prefix: " << recv;
    uint32 n;
    CHECK(SimpleAtoi(recv.substr(0, i), &n)) << recv;
    string err;
    Json out = Json::parse(recv.substr(i + 1), err);
    CHECK(err.empty()) << "parse error: " << err;
    return out;
  }

  static Json setup_json(int p, int total, Json map_json) {
    return Json::object{
        {"punter", p},
        {"punters", total},
        {"map", map_json},
        {"settings", FLAGS_futures ? Json::object{{"futures", true}} : Json()}};
  }

  // state
  Json setup(int p, int total, Json map_json) {
    // S → P {"punter" : p, "punters" : n, "map" : map}
    // P → S {"ready" : p, "state" : state}
    Json got = io_once(ais_[p], setup_json(p, total, map_json));
    CHECK_EQ(got["ready"].int_value(), p);
    if (FLAGS_futures) {
      for (const auto& future : got["futures"].array_items()) {
        future["source"].int_value(), future["target"].int_value();
      }
    }
    return got;
  }

  // move
  Json gameplay(int p, const Json::array& moves, const Json& state) {
    // S → P {"punter" : p, "punters" : n, "map" : map}
    // P → S {"ready" : p, "state" : state}
    Json send = Json::object{{"move", Json::object{{"moves", moves}}},
                             {"state", state}};
    Json got = io_once(ais_[p], send);
    const auto& claim = got["claim"];
    if (claim["punter"].int_value() != p || !claim["source"].is_number() ||
        !claim["target"].is_number()) {
      LOG(ERROR) << ais_[p] << " invalid claim: " << claim.dump();
      return Json::object{{"pass", Json::object{{"punter", p}}},
                          {"state", got["state"]}};
    }
    return got;
  }

  void score() {
    int n = site_ids_.size();
    vector<vector<int>> d(n, vector<int>(n, INT_MAX / 2));
    for (int i = 0; i < n; ++i) d[i][i] = 0;
    for (const auto& r : river_to_index_) {
      int s = site_id_to_index_[r.first.first];
      int t = site_id_to_index_[r.first.second];
      d[s][t] = 1;
      d[t][s] = 1;
    }
    for (int k = 0; k < n; ++k) {
      for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) {
          d[i][j] = min(d[i][j], d[i][k] + d[k][j]);
        }
      }
    }
    Json::array scores;
    for (int p = 0; p < ais_.size(); ++p) {
      int score = 0;
      for (int m : mines_) {
        int bet = -1;
        auto it = futures_[p].find(m);
        if (it != futures_[p].end()) {
          bet = it->second;
        }
        vector<bool> visited(n, false);
        std::stack<int> st;
        st.push(m);
        while (!st.empty()) {
          int s = st.top();
          st.pop();
          if (visited[s]) continue;
          visited[s] = true;
          score += d[m][s] * d[m][s];
          if (bet == s) {
            score += d[m][s] * d[m][s] * d[m][s];
            bet = -1;
          }
          for (int t : punter_river_adj_[p][s]) st.push(t);
        }
        if (bet != -1) {
          score -= d[m][bet] * d[m][bet] * d[m][bet];
        }
      }
      LOG(INFO) << ais_[p] << ": score=" << score;
      scores.emplace_back(Json::object{
          {"punter", p}, {"score", score},
      });
    }
    Json final = Json::object{{"scores", scores}};
    for (const auto& id :
         GetResponseOrDie(StreamUtil::List("communicator")).stream_ids) {
      GetResponseOrDie(StreamUtil::Write(id, final.dump()));
    }
  }

  void gen_dot(const string& file) {
    string dot;
    StrAppend(&dot, "graph{\nnode[shape=point]\nedge[fontsize=8]\n");
    StrAppend(&dot, "graph[bb=\"0,0,", FLAGS_scale, ",", FLAGS_scale,
              "\",margin=\"", FLAGS_scale / 10, "\"]\n");
    double min_x = *std::min_element(site_x_.begin(), site_x_.end());
    double min_y = *std::min_element(site_y_.begin(), site_y_.end());
    double scale =
        FLAGS_scale /
        std::max(*std::max_element(site_x_.begin(), site_x_.end()) - min_x,
                 *std::max_element(site_y_.begin(), site_y_.end()) - min_y);
    for (int i = 0; i < site_ids_.size(); ++i) {
      StrAppend(&dot, site_ids_[i], "[pos=\"", (site_x_[i] - min_x) * scale,
                ",", (site_y_[i] - min_y) * scale, "!\"]\n");
    }
    for (int m : mines_) {
      StrAppend(&dot, site_ids_[m], "[color=red]\n");
    }
    for (int i = 0; i < ais_.size(); ++i) {
      bool legend_rendered = false;
      for (int j = 0; j < site_ids_.size(); ++j) {
        for (int k : punter_river_adj_[i][j]) {
          if (j < k) {
            StrAppend(&dot, site_ids_[j], "--", site_ids_[k],
                      "[color=", kColorPalette[i % 8],
                      legend_rendered
                          ? ""
                          : StrCat(",label=\"", ais_[i], "\"",
                                   ",fontcolor=", kColorPalette[i % 8]),
                      "]\n");
            legend_rendered = true;
          }
        }
      }
    }
    StrAppend(&dot, "}\n");

    std::ofstream ofs(file);
    ofs << dot;
    LOG(INFO) << "dot out: " << file;
  }
};

int main(int argc, char** argv) {
  ParseCommandLineFlags(&argc, &argv);
  if (FLAGS_map.empty()) {
    puts("required --map");
    return 0;
  }

  vector<string> ais;
  for (int i = 1; i < argc; ++i) ais.push_back(argv[i]);

  Game game(ais);
  game.init();
  game.start();

  StreamUtil::Exit();
  return 0;
}
