#include "ofApp.h"
#include "json11/json11.hpp"

#define all(c) (c).begin(), (c).end()
#define iter(c) __typeof((c).begin())
#define cpresent(c, e) (find(all(c), (e)) != (c).end())
#define rep(i, n) for (int i = 0; i < (int)(n); i++)
#define tr(c, i) for (iter(c) i = (c).begin(); i != (c).end(); ++i)
#define pb(e) push_back(e)
#define mp(a, b) make_pair(a, b)

//
// From base.{h, cc}
//
namespace {
//
// Utils
//
vector<string> Split(string s, const string &t) {
  vector<string> v;
  for (string::size_type p = s.find(t); p != s.npos; p = s.find(t)) {
    v.push_back(s.substr(0, p));
    s = s.substr(p + t.size());
  }
  v.push_back(s);
  return v;
}

//
// Map
//
struct Site {
  int id;
  double x, y;
  bool is_mine;
};

struct Map {
  vector<Site> sites;
  vector<pair<int, int>> rivers;
  vector<int> mines;
};

struct Claim {
  int rank, source, target;
  bool option;
};

map<int, int> ConstructIdToIndexMap(const Map &s) {
  map<int, int> ma;
  rep (i, s.sites.size()) {
    ma[s.sites[i].id] = i;
  }
  return ma;
}

Map ConstructMap(const json11::Json &j) {
  Map ma;

  auto sites = j["sites"].array_items();

  rep (i, sites.size()) {
    auto s = sites[i];
    int id = s["id"].int_value();
    double x = s["x"].number_value();
    double y = s["y"].number_value();
    ma.sites.emplace_back(Site{id, x, y, false});
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
}  // namespace

namespace {
const double kDrawWidth = 1024;
const double kDrawHeight = 586;
const double kDrawPadding = 15;
const ofVec2f kDrawPaddingVec(kDrawPadding, kDrawPadding);

int size;
Map map_;
vector<bool> is_pass;
vector<Claim> claims;
int turn;
int highlighted_rank;

double draw_scale = 100.0;
ofVec2f draw_offset(-3, -3);  // top left
double draw_base_line_width = 2.0;
bool draw_animation = false;

long long tick;

ofVec2f SiteToScreenPoint(const Site &site) {
  ofVec2f p(site.x, site.y);
  p -= draw_offset;
  p *= draw_scale;
  return kDrawPadding + p;
}
}  // namespace

void SetInput(const string &str) {
  puts("SetInput");
  vector<string> lines = Split(str, "\n");

  string err;
  auto j = json11::Json::parse(lines[0], err);
  if (!err.empty()) {
    cout << "JSON Error at Line 1: " << err << endl;
    return;
  }
  map_ = ConstructMap(j["map"]);
  size = j["punters"].int_value();

  map<int, int> id_to_idx = ConstructIdToIndexMap(map_);

  is_pass.clear();
  claims.clear();
  for (size_t i = 1; i < lines.size(); ++i) {
    const string &line = lines[i];
    if (line.empty()) {
      cout << "JSON Warning at Line " << i << ": This line was empty and ignored." << endl;
      continue;
    }

    j = json11::Json::parse(line, err);
    if (!err.empty()) {
      cout << "JSON Error at Line " << i << ": " << err << endl;
      return;
    }

    if (!j["claim"].is_null()) {
      auto jj = j["claim"];
      is_pass.pb(false);
      claims.emplace_back
        (Claim{jj["punter"].int_value(),
              id_to_idx[jj["source"].int_value()],
              id_to_idx[jj["target"].int_value()],
              false});
    } else if (!j["option"].is_null()) {
      auto jj = j["option"];
      is_pass.pb(false);
      claims.emplace_back
        (Claim{jj["punter"].int_value(),
              id_to_idx[jj["source"].int_value()],
              id_to_idx[jj["target"].int_value()],
              true});
    } else if (!j["splurge"].is_null()) {
      auto jj = j["splurge"];
      auto jr = jj["route"].array_items();
      for (int i = 0; i + 1 < (int)jr.size(); ++i) {
        is_pass.pb(false);
        claims.emplace_back
            (Claim{jj["punter"].int_value(),
                  id_to_idx[jr[i].int_value()],
                  id_to_idx[jr[i + 1].int_value()],
                  false});
      }
    } else {
      is_pass.pb(true);
      claims.emplace_back(Claim{-1, -1, -1, false});
    }
  }

  turn = 1;
  highlighted_rank = -1;
  tick = 0;
  draw_animation = true;

  // Compute offset
  {
    double min_x = 1E9, max_x = -1E9;
    double min_y = 1E9, max_y = -1E9;

    for (const auto &s : map_.sites) {
      min_x = min(min_x, s.x); max_x = max(max_x, s.x);
      min_y = min(min_y, s.y); max_y = max(max_y, s.y);
    }

    draw_offset = ofVec2f(min_x, min_y);
    draw_scale = min((kDrawWidth - kDrawPadding * 2) / (max_x - min_x + 1E-9),
                     (kDrawHeight - kDrawPadding * 2) / (max_y - min_y + 1E-9));

    draw_offset.x -= ((kDrawWidth - kDrawPadding * 2) / draw_scale - (max_x - min_x)) / 2;
    draw_offset.y -= ((kDrawHeight - kDrawPadding * 2) / draw_scale - (max_y - min_y)) / 2;

    printf("SCALE: %f\n", draw_scale);
  }
}

//--------------------------------------------------------------
const int kFrameRate = 15;

void ofApp::setup(){
  ofSetFrameRate(kFrameRate);
  ofBackground(255, 255, 255);
  ofSetWindowShape(kDrawWidth, kDrawHeight);
}

//--------------------------------------------------------------
void ofApp::update(){
  ++tick;
  //ofSetFrameRate(2);

  if (draw_animation) {
    turn += max(1, (int)(claims.size() / (5 * kFrameRate)));
    turn = min(turn, (int)claims.size());
  }
}

//--------------------------------------------------------------
void ofApp::draw(){
  // ofSetColor(255, 255, 255);
  // ofDrawRectangle(0, 0, kDrawWidth, kDrawHeight);

  // double line_width_base = draw_scale / 10;
  // printf("line width: %f\n", line_width_base);
  //ofSetLineWidth(line_width_base);
  // Ofsetlinewidthrepla(4);
  mySetLineWidth(draw_base_line_width);

  ofSetColor(200, 200, 200);
  for (const auto &river : map_.rivers) {
    const Site &a = map_.sites[river.first];
    const Site &b = map_.sites[river.second];
    myDrawLine(SiteToScreenPoint(a), SiteToScreenPoint(b));
  }

  int n = min(turn, (int)claims.size());
  rep (iter, 2) {
    for (int i = 0; i < n; ++i) {
      if (is_pass[i]) continue;

      auto claim = claims[i];
      const Site &a = map_.sites[claim.source];
      const Site &b = map_.sites[claim.target];

      if (highlighted_rank == -1) {
        if (iter == 0) ofSetColor(ofColor::fromHsb((claim.rank + 1) * 255 / (size + 1), 255, 170));
        else continue;
      } else if (claim.rank == highlighted_rank) {
        if (iter == 0) continue;
        else {
          if (claim.option) ofSetColor(ofColor::fromHsb(1.0 * 255 / 3, 255, 170));
          else ofSetColor(ofColor::fromHsb(2.0 * 255 / 3, 255, 170));
        }
      } else {
        if (iter == 0) ofSetColor(100, 100, 100);
        else continue;
      }

      bool is_last = (i + 1 == n);
      /*
        if (highlighted_rank != -1) {
        is_last |= (claim.rank == highlighted_rank && i + size >= n);
        }
      */

      if (is_last) {
        mySetLineWidth(draw_base_line_width * 4);
        // if (tick % 15 == 0) cout << SiteToScreenPoint(a) << "; " << SiteToScreenPoint(b) << endl;
        if (tick % 10 < 5) continue;
      } else {
        mySetLineWidth(draw_base_line_width * 2);
      }
      myDrawLine(SiteToScreenPoint(a), SiteToScreenPoint(b));
    }
  }

  for (const auto &site : map_.sites) {
    if (!site.is_mine) continue;
    ofSetColor(255, 0, 0);
    ofDrawCircle(SiteToScreenPoint(site), draw_base_line_width * 2);
  }
}

//--------------------------------------------------------------
void ofApp::keyPressed(int key){
  bool old_draw_animation = draw_animation;
  draw_animation = false;

  switch (key)  {
case 'j':
++turn;
break;

case 'u':
--turn;
break;

case 'k':
turn += size;
break;

case 'i':
turn -= size;
break;

case 'o':
turn -= claims.size() / 10;
break;

case 'l':
turn += claims.size() / 10;
break;

case 'p':
turn = 0;
break;

case ';':
turn = (int)claims.size();
break;

case '1':
highlighted_rank += 1;
if (highlighted_rank >= size) highlighted_rank = -1;
break;

case '2':
highlighted_rank -= 1;
if (highlighted_rank < -1) highlighted_rank = size - 1;
break;

case 'y':
  draw_base_line_width *= 0.9;
break;

case 'h':
  draw_base_line_width /= 0.9;
break;

case 'a':
  draw_animation = !old_draw_animation;
  break;
}

turn = max(min(turn, (int)claims.size()), 1);
printf("Turn %d / %d, Last player: %d / %d (%d-%d, option=%d), Highlighted player: %d\n",
       turn, (int)claims.size(), claims[turn - 1].rank, size,
       claims[turn - 1].source, claims[turn - 1].target, (int)claims[turn - 1].option,
       highlighted_rank);

//ofSetFrameRate(60);
//draw();
}

//--------------------------------------------------------------
void ofApp::keyReleased(int key){

}

//--------------------------------------------------------------
void ofApp::mouseMoved(int x, int y ){

}

//--------------------------------------------------------------
void ofApp::mouseDragged(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mousePressed(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mouseReleased(int x, int y, int button){

}

//--------------------------------------------------------------
void ofApp::mouseEntered(int x, int y){

}

//--------------------------------------------------------------
void ofApp::mouseExited(int x, int y){

}

//--------------------------------------------------------------
void ofApp::windowResized(int w, int h){

}

//--------------------------------------------------------------
void ofApp::gotMessage(ofMessage msg){

}

//--------------------------------------------------------------
void ofApp::dragEvent(ofDragInfo dragInfo){

}


#ifdef EMSCRIPTEN
#include <emscripten.h>
#include <emscripten/bind.h>

EMSCRIPTEN_BINDINGS(my_module) {
  emscripten::function("setInput", &SetInput);
}
#endif
