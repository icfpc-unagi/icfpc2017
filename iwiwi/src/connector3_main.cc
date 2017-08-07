#include "base.h"
#include "random.h"
#include "graph.h"
#include "jlog.h"
#include "agl.h"
#include "agl/cut_tree/cut_tree.h"
#include "agl/cut_tree/dinitz.h"
#include "agl/cut_tree/bi_dinitz.h"

namespace agl {
G to_directed_graph(G&& g) {
  vector<pair<V, V>> ret;
  for (auto& e : g.edge_list()) {
    if (e.first < to(e.second)) ret.emplace_back(e.first, to(e.second));
    else if (to(e.second) < e.first) ret.emplace_back(to(e.second), e.first);
  }
  sort(ret.begin(), ret.end());
  ret.erase(unique(ret.begin(), ret.end()), ret.end());
  return G(ret);
}
}  // hogeee

namespace {
pair<int, int> Greedy(const GameState &game) {
  map<pair<int, int>, int> score;

  Graph G = ConstructGraph(game), H;
  UnionFind uf;
  tie(H, uf) = ConstructContractedGraph(G, game.rank);
  int N = G.size();

  for (int mine: game.map.mines) {
    auto dst1 = SSSP(G, mine);
    vector<double> component_score(N);
    rep (v, N) component_score[uf.root[v]] += dst1[v].first * dst1[v].first;

    int root = uf.root[mine];
    for (auto &e : H[root]) {
      score[mp(min(root, e.to), max(root, e.to))] += component_score[e.to];
    }
  }

  tuple<int, int, int> bst(-1, -1, -1);
  for (auto &i : score) bst = max(bst, make_tuple(i.second, i.first.first, i.first.second));

  if (get<0>(bst) != -1) {
    return FindOriginalEdge(get<1>(bst), get<2>(bst), uf, G);
  } else {
    return RandomRemaining(game);
  }
}
}

namespace {
struct MyAIState {
  int u, v;

  template<class Archive>
  void serialize(Archive& ar, unsigned int ver) {
    ar & u & v;
  }
};

using MyState = State<MyAIState>;

GameState game;

int N;
Graph G, H;
UnionFind uf;

pair<MyAIState, vector<pair<int, int>>> Setup(const GameState &game) {
  JLOG_PUT_BENCHMARK("setup") {
    // itsumono
    ::game = game;
    G = ConstructGraph(game);

    agl::G aglg(game.map.rivers);
    aglg = agl::to_directed_graph(move(aglg));
    N = aglg.num_vertices();
    auto ct = agl::cut_tree(aglg);

    vector<tuple<int, int, int, int>> ord(N);

    rep (i, game.map.mines.size()) {
      int s = game.map.mines[i];

      vector<pair<int, int>> dst1 = SSSP(G, s);
      // rep (v, N) if (v != s) cerr << ct.query(s, v) << " ";

      rep (v, N) {
        if (game.map.sites[v].is_mine) continue;
        if (dst1[v].first == INT_MAX) continue;
        ord.emplace_back(dst1[v].first, ct.query(s, v), s, v);
      }
    }

    sort(all(ord));
    reverse(all(ord));
    const int kNumTopEntries = 30;

    rep (i, kNumTopEntries) {
      auto o = ord[i];
      cerr << get<0>(o) << " " << get<1>(o) << " " << get<2>(o) << "-" << get<3>(o) << endl;
    }

    int max_connectivity = 0;
    rep (i, kNumTopEntries) max_connectivity = max(max_connectivity, get<1>(ord[i]));
    rep (i, kNumTopEntries) {
      if (get<1>(ord[i]) == max_connectivity) {
        cerr << ord[i] << endl;
        int s = get<2>(ord[i]), t = get<3>(ord[i]);

        // agl::cut_tree_internal::dinitz din(aglg);
        // cerr << din.max_flow(s, t) << endl;

        return make_pair(MyAIState{s, t}, vector<pair<int, int>>{{s, t}});
      }
    }
  }

  assert(false);
}

pair<tuple<string, int, int>, MyAIState> Play(const MyState &state) {
  // return make_pair(Greedy(state.game), state.ai);

  JLOG_PUT_BENCHMARK("time") {
    rep (iter, 2) {
      bool allow_option = iter;
      // iter == 0 -> normal edges
      // iter == 1 -> option edges
      cerr << "--------------------------------------------------------" << endl;
      cerr << "Iter: " << iter << endl;

      game = state.game;
      G = ConstructGraph(game);
      N = G.size();
      tie(H, uf) = ConstructContractedGraph(G, game.rank, iter == 1 /* allow option? */);

      int S = state.ai.u, T = state.ai.v;
      S = uf.root[S];
      T = uf.root[T];

      auto dst2 = SSSP(H, S);
      cerr << "Distance: " << dst2[T].first << endl;
      if (dst2[T].second == -1) continue;
      if (dst2[T].first == 0) {
        cerr << "S U C C E S S !!!" << endl;
        break;
      }

      // Shortest path
      vector<int> shortest_path;
      for (int v = T; v != S; v = dst2[v].second) shortest_path.emplace_back(v);
      shortest_path.emplace_back(S);
      reverse(all(shortest_path));

      // Dinitz
      vector<pair<int, int>> es;
      rep (v, N) for (const auto &e: G[v]) {
        if (e.owner == -1 || (allow_option && e.owner2 == -1)) {
          int a = uf.root[v];
          int b = uf.root[e.to];
          if (a >= b) continue;
          es.emplace_back(a, b);
        }
      }
      agl::cut_tree_internal::dinitz dnz(agl::G(es, N));
      cerr << "Cut: " << dnz.max_flow(S, T) << endl;
      vector<pair<int, int>> cut_es = dnz.cut(S);
      for (auto &e : cut_es) if (e.first > e.second) swap(e.first, e.second);
      set<pair<int, int>> se(all(cut_es));

      // Find cut edge
      for (int i = 0; i + 1 < (int)shortest_path.size(); ++i) {
        int u = shortest_path[i], v = shortest_path[i + 1];
        if (u > v) swap(u, v);
        if (se.count(make_pair(u, v))) {
          auto ans = FindOriginalEdge(u, v, uf, G);

          if (iter == 0) return mp(make_tuple("claim",  ans.first, ans.second), state.ai);
          else           return mp(make_tuple("option", ans.first, ans.second), state.ai);
        }
      }
    }
  }

  cerr << "MODUAMEPO!! " << endl;
  auto ans = Greedy(state.game);
  return mp(make_tuple("claim", ans.first, ans.second), state.ai);
}
}  // namespace

template<typename AIState, typename SetupFunc, typename PlayFunc>
void RunWithFutures(SetupFunc setup, PlayFunc play) {
  using MyState = State<AIState>;

  // Input
  json11::Json in_json = InputJSON(), out_json;

  if (IsSetup(in_json)) {
    // Setup
    MyState s;
    vector<pair<int, int>> futures;
    s.game = ConstructGameState(in_json);
    tie(s.ai, futures) = setup(s.game);

    vector<json11::Json> futures_json;
    for (auto &f : futures) {
      futures_json.emplace_back(
          json11::Json::object{{"source", f.first}, {"target", f.second}});
    }

    out_json = json11::Json::object{
      {"ready", s.game.rank},
      {"state", DumpState(s)},
      {"futures", futures_json},
    };

    cerr << json11::Json(futures_json).dump() << endl;
  } else {
    cerr << in_json.dump() << endl;

    // Play
    MyState s = GetState<AIState>(in_json);
    pair<tuple<string, int, int>, AIState> res = play(s);
    s.ai = res.second;

    out_json = json11::Json::object{
      {get<0>(res.first) /* claim or option */, json11::Json::object{
          {"punter", s.game.rank},
          {"source", s.game.map.sites[get<1>(res.first)].id},
          {"target", s.game.map.sites[get<2>(res.first)].id}}},
      {"state", DumpState(s) }};

    cerr << out_json.dump() << endl;
  }

  // Output
  OutputJSON(out_json);
}

int main() {
  srand(getpid() * time(NULL));
  RunWithFutures<MyAIState>(Setup, Play);
  return 0;
}
