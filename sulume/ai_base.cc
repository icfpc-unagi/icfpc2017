#include <array>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>
#include "base/base.h"
#include "json11.hpp"
#include "strings/join.h"
#include "strings/numbers.h"
#include "strings/split.h"
#include "strings/strcat.h"
#include "util/gtl/map_util.h"

using json11::Json;

template <typename T>
std::pair<T, T> make_sorted_pair(const T& a, const T& b) {
  return a < b ? make_pair(a, b) : make_pair(b, a);
}
int ConsumeIntAndDelim(StringPiece* sp, char delim) {
  size_t i = sp->find(delim);
  CHECK_NE(i, StringPiece::npos);
  int ret;
  CHECK(SimpleAtoi(sp->substr(0, i).ToString(), &ret));
  sp->remove_prefix(i + 1);
  return ret;
}

Json in() {
  size_t len;
  scanf("%zu:", &len);
  vector<char> b(len + 1);
  for (char *p = &b[0], *e = &b[0] + len; p < e;) {
    p = fgets(p, e - p, stdin);
    if (p == nullptr) return Json();
  }
  string err;
  Json got = Json::parse(string(&b[0]), err);
  CHECK(err.empty()) << err;
  return got;
}

void out(const Json& data) {
  string s = data.dump();
  puts(StrCat(s.size(), ":", s).c_str());
}

struct Site {
  int id;
  set<int> rivers;
};

struct Mine {
  int i;
  vector<int> d;
};

struct State {
  vector<Site> sites;
  vector<Mine> mines;
  int rest;
  int ext;

  Json to_json() const {
    string str;
    StrAppend(&str, rest, "$", ext, "$", sites.size());
    for (const auto& s : sites) {
      StrAppend(&str, "!", s.id, "@", strings::JoinInts(s.rivers, ","));
    }
    StrAppend(&str, "|", mines.size());
    for (const auto& m : mines) {
      StrAppend(&str, "!", m.i, "@", strings::JoinInts(m.d, ","));
    }
    return Json(str);
  }
  void from_json(const Json& json) {
    string str = json.string_value();
    StringPiece sp = str;
    rest = ConsumeIntAndDelim(&sp, '$');
    ext = ConsumeIntAndDelim(&sp, '$');
    std::vector<StringPiece> split = strings::Split(str, "|");
    sites.resize(ConsumeIntAndDelim(&split[0], '!'));
    for (auto& s : sites) {
      s.id = ConsumeIntAndDelim(&split[0], '@');
      SplitStringAndParseToContainer(split[0], ",", &safe_strto32, &s.rivers);
    }
    mines.resize(ConsumeIntAndDelim(&split[1], '!'));
    for (auto& m : mines) {
      m.i = ConsumeIntAndDelim(&split[1], '@');
      SplitStringAndParseToList(split[1], ",", &safe_strto32, &m.d);
    }
  }

  vector<int> djk(int i) const {
    int n = sites.size();
    vector<int> d(n, INT_MAX);
    std::priority_queue<pair<int, int>> q;
    vector<bool> v(n);
    q.push(make_pair(0, i));
    while (!q.empty() && n > 0) {
      int c, s;
      std::tie(c, s) = q.top();
      q.pop();
      if (v[s]) continue;
      v[s] = true;
      n--;
      d[s] = -c;
      for (int t : sites[s].rivers) q.push(make_pair(c - 1, t));
    }
  }
};

int main(int argc, char** argv) {
  ParseCommandLineFlags(&argc, &argv);

  out(Json::object{{"me", ""}});

  while (true) {
    Json got = in();
    if (got.is_null()) break;
    if (!got["you"].is_null()) continue;
    if (!got["punter"].is_null()) {
      // setup
      State state;
      Json map = got["map"];
      std::unordered_map<int, int> byid;
      for (const auto& s : map["sites"].array_items()) {
        Site site;
        site.id = s["id"].int_value();
        byid.emplace(site.id, state.sites.size());
        state.sites.push_back(site);
      }
      for (const auto& r : map["rivers"].array_items()) {
        int s = byid[r["source"].int_value()];
        int t = byid[r["target"].int_value()];
        state.sites[s].rivers.insert(t);
        state.sites[t].rivers.insert(s);
        state.rest++;
      }
      for (const auto& m : map["mines"].array_items()) {
        Mine mine;
        mine.i = byid[m.int_value()];
        mine.d = state.djk(mine.i);
        state.mines.push_back(mine);
      }
      out(Json::object{{"ready", got["punter"]}, {"state", state.to_json()}});
    } else {
      // gameplay
      Json got = in();
      State state;
      state.from_json(got["state"]);

      out(Json::object{{"move", nullptr}, {"state", state.to_json()}});
    }
  }

  return 0;
}
