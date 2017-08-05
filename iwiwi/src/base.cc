#include "base.h"

Map ConstructMap(const json11::Json &j) {
  Map ma;

  auto sites = j["sites"].array_items();

  rep (i, sites.size()) {
    auto s = sites[i];
    int id = s["id"].int_value();
    ma.sites.emplace_back(Site{id, false});
  }

  map<int, int> id_to_idx = ConstructIdToIndexMap(ma);

  auto rivers = j["rivers"].array_items();
  for (auto &&river : rivers) {
    ma.rivers.emplace_back
        (id_to_idx[river["source"].int_value()],
         id_to_idx[river["target"].int_value()]);
  }

  auto mines = j["mines"].array_items();
  for (auto &&mine : mines) {
    int id = mine.int_value();
    int idx = id_to_idx[id];
    ma.mines.emplace_back(idx);
    ma.sites[idx].is_mine = true;
  }
  return ma;
}

GameState ConstructGameState(const json11::Json &j) {  // First turn
  GameState s;
  s.rank = j["punter"].int_value();
  s.size = j["punters"].int_value();
  s.map = ConstructMap(j["map"]);
  s.turn = 0;
  return s;
}

map<int, int> ConstructIdToIndexMap(const Map &s) {
  map<int, int> ma;
  rep (i, s.sites.size()) {
    ma[s.sites[i].id] = i;
  }
  return ma;
}

//
// IO
//
json11::Json InputJSON() {
  std::string stdin((std::istreambuf_iterator<char>(cin)),
                    std::istreambuf_iterator<char>());

  // Remove 'n:' part
  int i = 0;
  while (isdigit(stdin[i])) ++i;
  assert(stdin[i] == ':');
  stdin = stdin.substr(i + 1);

  string err;
  auto j = json11::Json::parse(stdin, err);
  if (!err.empty()) {
    cerr << "JSON Error: " << err << endl;
    assert(err.empty());
  }

  return j;
}

void OutputJSON(const json11::Json &json) {
  string str = json.dump();
  ostringstream os;
  cout << str.length() << ":" << str << endl;
}

bool IsSetup(const json11::Json &json) {
  return json["state"].is_null();
}
