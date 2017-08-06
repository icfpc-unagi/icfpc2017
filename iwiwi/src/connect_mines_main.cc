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

MyAIState Setup(const GameState &game) {
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

      rep (j, i) {
        int t = game.map.mines[j];
        ord.emplace_back(dst1[t].first, ct.query(s, t), s, t);
      }
    }

    sort(all(ord));
    reverse(all(ord));

    rep (i, 10) {
      auto o = ord[i];
      cerr << get<0>(o) << " " << get<1>(o) << " " << get<2>(o) << "-" << get<3>(o) << endl;
    }

    int max_connectivity = 0;
    rep (i, 10) max_connectivity = max(max_connectivity, get<1>(ord[i]));
    rep (i, 10) {
      if (get<1>(ord[i]) == max_connectivity) {
        cerr << ord[i] << endl;
        int s = get<2>(ord[i]), t = get<3>(ord[i]);

        // agl::cut_tree_internal::dinitz din(aglg);
        // cerr << din.max_flow(s, t) << endl;

        return MyAIState{s, t};
      }
    }
  }

  assert(false);
}

pair<pair<int, int>, MyAIState> Play(const MyState &state) {
  JLOG_PUT_BENCHMARK("time") {
    game = state.game;
    G = ConstructGraph(game);
    N = G.size();
    tie(H, uf) = ConstructContractedGraph(G, game.rank);

    int S = state.ai.u, T = state.ai.v;
    S = uf.root[S];
    T = uf.root[T];

    auto dst2 = SSSP(H, S);
    if (dst2[T].second == -1 || dst2[T].first == 0) {
      // TODO
      cerr << "MOUDAMEPO" << endl;
      return make_pair(RandomRemaining(state.game), state.ai);
    }

    vector<int> shortest_path;
    for (int v = T; v != S; v = dst2[v].second) shortest_path.emplace_back(v);
    shortest_path.emplace_back(S);
    reverse(all(shortest_path));

    tuple<int, int, int> bst(-1, -1, -1);
    for (int i = 0; i + 1 < (int)shortest_path.size(); ++i) {
      UnionFind uf2 = uf;
      uf2.Merge(shortest_path[i], shortest_path[i + 1]);

      // map<pair<int, int>, int> es;
      vector<pair<int, int>> es;
      rep (v, N) for (const auto &e: G[v]) {
        if (e.owner == -1) {
          int a = uf2.root[v];
          int b = uf2.root[e.to];
          if (a >= b) continue;
          // es[mp(min(a, b), max(a, b))] += 1;
          es.emplace_back(a, b);
        }
      }

      int s = uf2.root[S], t = uf2.root[T], f;
      if (s == t) {
        f = INT_MAX;
      } else {
        agl::bi_dinitz bdz(es, N);
        f = bdz.max_flow(uf2.root[S], uf2.root[T]);
        cerr << f << " ";
      }
      bst = max(bst, make_tuple(f, shortest_path[i], shortest_path[i + 1]));
    }
    cerr << endl;

    auto ans = FindOriginalEdge(get<1>(bst), get<2>(bst), uf, G);
    return make_pair(ans, state.ai);
  }

  assert(false);
}
}  // namespace

int main() {
  srand(getpid() * time(NULL));
  Run<MyAIState>(Setup, Play);
  return 0;
}
