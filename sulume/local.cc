#include <fstream>
#include <iostream>
#include <iterator>
#include <map>
#include <queue>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>
#include "base/base.h"
#include "json11.hpp"
#include "ninetan/ninestream_util.h"
#include "strings/numbers.h"
#include "strings/strcat.h"

DEFINE_int32(timeout, 10000, "gameplay timeout in millisecs");
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
  string listener_id;
  // Map = {"sites" : [Site], "rivers" : [River], "mines" : [SiteId]}
  // Site = {"id" : SiteId}
  // River = {"source" : SiteId, "target" : SiteId}
  // SiteId = Nat
  Json map_json_;
  vector<int> site_ids_;
  vector<double> site_x_;
  vector<double> site_y_;
  std::unordered_map<int, int> site_id_to_index_;
  map<pair<int, int>, int> river_to_index_;
  vector<int> mines_;
  vector<map<int, int>> futures_;

  vector<Json> states_;
  vector<bool> river_claimed_;
  vector<vector<vector<int>>> punter_river_adj_;
  vector<Json> last_moves_;

 public:
  Game(vector<string> ais) : ais_(ais) {}

  void init() {
    string err;
    std::ifstream ifs(FLAGS_map);
    map_json_ = Json::parse(string((std::istreambuf_iterator<char>(ifs)),
                                   std::istreambuf_iterator<char>()),
                            err);
    CHECK(err.empty()) << "loading " << FLAGS_map << ": " << err;
    for (const auto& site : map_json_["sites"].array_items()) {
      site_ids_.push_back(site["id"].int_value());
      if (!FLAGS_dot.empty()) {
        const auto& x = site["x"];
        const auto& y = site["y"];
        if (x.is_number() && y.is_number()) {
          site_x_.push_back(x.number_value());
          site_y_.push_back(y.number_value());
        }
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
      river_to_index_.emplace(make_sorted_pair(rivers[i]["source"].int_value(),
                                               rivers[i]["target"].int_value()),
                              i);
    }
    river_claimed_.resize(rivers.size());
    for (const auto& mine : map_json_["mines"].array_items()) {
      mines_.push_back(site_id_to_index_[mine.int_value()]);
    }

    punter_river_adj_.resize(ais_.size(),
                             vector<vector<int>>(site_ids_.size()));
    if (FLAGS_futures) futures_.resize(ais_.size());
    states_.resize(ais_.size());
    for (int i = 0; i < ais_.size(); ++i) {
      last_moves_.emplace_back(
          Json::object{{"pass", Json::object{{"punter", i}}}});
    }
    if (!FLAGS_listener.empty()) {
      listener_id =
          GetResponseOrDie(StreamUtil::Run(1, FLAGS_listener)).stream_ids[0];
    } else {
      auto ids = GetResponseOrDie(StreamUtil::List("communicator")).stream_ids;
      if (!ids.empty()) {
        listener_id = ids[0];
      }
    }
  }

  void start() {
    for (int i = 0; i < ais_.size(); ++i) {
      auto got = setup(i, ais_.size(), map_json_);
      states_[i] = got["state"];
      if (FLAGS_futures) {
        for (const auto& future : got["futures"].array_items()) {
          futures_[i].emplace(future["source"].int_value(),
                              future["target"].int_value());
        }
      }
    }
    if (!listener_id.empty()) {
      GetResponseOrDie(StreamUtil::Write(
          listener_id, setup_json(-1, ais_.size(), map_json_).dump()));
    }
    for (int turn = 0; turn < river_claimed_.size(); ++turn) {
      int i = turn % ais_.size();
      auto move_state = gameplay(i, last_moves_, states_[i]);
      const auto& claim = move_state["claim"];
      int s = claim["source"].int_value();
      int t = claim["target"].int_value();
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
        last_moves_[i] = Json::object{{"claim", claim}};
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
  static Json io_once(const string& cmd, const Json& in, int timeout) {
    string id = GetResponseOrDie(StreamUtil::Run(1, cmd)).stream_ids[0];
    string send = in.dump();
    GetResponseOrDie(StreamUtil::Write(id, StrCat(send.size(), ":", send)));
    auto response = StreamUtil::Read(id, timeout);
    if (code == StreamUtil::DEADLINE_EXCEEDED) {
      LOG(WARNING) << "Deadline exceeded: " << cmd;
      return Json();
    }
    string recv = GetResponseOrDie(response).data;
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
    Json got = io_once(ais_[p], setup_json(p, total, map_json), 10000);
    CHECK_EQ(got["ready"].int_value(), p);
    return got;
  }

  // move
  Json gameplay(int p, const Json::array& moves, const Json& state) {
    // S → P {"punter" : p, "punters" : n, "map" : map}
    // P → S {"ready" : p, "state" : state}
    Json send = Json::object{{"move", Json::object{{"moves", moves}}},
                             {"state", state}};
    Json got = io_once(ais_[p], send, FLAGS_timeout);
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
    vector<vector<int>> e(river_to_index_.size());
    for (const auto& r : river_to_index_) {
      int s = site_id_to_index_[r.first.first];
      int t = site_id_to_index_[r.first.second];
      e[s].emplace_back(t);
      e[t].emplace_back(s);
    }
    vector<vector<int>> d(mines_.size(), vector<int>(n, INT_MAX));
    std::priority_queue<pair<int, int>> q;
    for (int i = 0; i < mines_.size(); ++i) {
      vector<bool> v(n);
      q.push(make_pair(0, mines_[i]));
      while (!q.empty()) {
        int c, s;
        std::tie(c, s) = q.top();
        q.pop();
        if (v[s]) continue;
        v[s] = true;
        d[i][s] = -c;
        for (int t : e[s]) q.push(make_pair(c - 1, t));
      }
    }
    Json::array scores;
    for (int p = 0; p < ais_.size(); ++p) {
      int score = 0;
      for (int i = 0; i < mines_.size(); ++i) {
        int bet = -1;
        auto it = futures_[p].find(mines_[i]);
        if (it != futures_[p].end()) {
          bet = it->second;
        }
        vector<bool> visited(n);
        std::stack<int> st;
        st.push(mines_[i]);
        while (!st.empty()) {
          int s = st.top();
          st.pop();
          if (visited[s]) continue;
          visited[s] = true;
          int d0 = d[i][s];
          score += d0 * d0;
          if (bet == s) {
            score += d0 * d0 * d0;
            bet = -1;
          }
          for (int t : punter_river_adj_[p][s]) st.push(t);
        }
        if (bet != -1) {
          int d0 = d[i][bet];
          score -= d0 * d0 * d0;
        }
      }
      LOG(INFO) << ais_[p] << ": score=" << score;
      scores.emplace_back(Json::object{
          {"punter", p}, {"score", score},
      });
    }
    Json final = Json::object{{"scores", scores}};
    if (!listener_id.empty()) {
      GetResponseOrDie(StreamUtil::Write(listener_id, final.dump()));
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
