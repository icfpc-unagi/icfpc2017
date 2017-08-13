#include "ofMain.h"
#include "ofApp.h"
#include <fstream>
using namespace std;

//========================================================================
int main(int argc, char **argv){
	ofSetupOpenGL(1024,768,OF_WINDOW);			// <-------- setup the GL context

    assert(argc < 3);
    if (argc == 1) {
      SetInput(R"({"map": {"mines": [1, 5], "rivers": [{"source": 0, "target": 1}, {"source": 1, "target": 2}, {"source": 0, "target": 7}, {"source": 7, "target": 6}, {"source": 6, "target": 5}, {"source": 5, "target": 4}, {"source": 4, "target": 3}, {"source": 3, "target": 2}, {"source": 1, "target": 7}, {"source": 1, "target": 3}, {"source": 7, "target": 5}, {"source": 5, "target": 3}], "sites": [{"id": 0, "x": 0, "y": 0}, {"id": 1, "x": 1, "y": 0}, {"id": 2, "x": 2, "y": 0}, {"id": 3, "x": 2, "y": -1}, {"id": 4, "x": 2, "y": -2}, {"id": 5, "x": 1, "y": -2}, {"id": 6, "x": 0, "y": -2}, {"id": 7, "x": 0, "y": -1}]}, "punter": -1, "punters": 4, "settings": {"futures": true}}
{"claim": {"punter": 0, "source": 1, "target": 3}}
{"claim": {"punter": 1, "source": 3, "target": 5}}
{"claim": {"punter": 2, "source": 0, "target": 1}}
{"claim": {"punter": 3, "source": 5, "target": 6}}
{"claim": {"punter": 0, "source": 1, "target": 2}}
{"claim": {"punter": 1, "source": 3, "target": 4}}
{"claim": {"punter": 2, "source": 0, "target": 7}}
{"claim": {"punter": 3, "source": 1, "target": 7}}
{"claim": {"punter": 0, "source": 2, "target": 3}}
{"claim": {"punter": 1, "source": 4, "target": 5}}
{"claim": {"punter": 2, "source": 5, "target": 7}}
{"claim": {"punter": 3, "source": 6, "target": 7}})");
    } else {
      ifstream ifs(argv[1]);
      string str((std::istreambuf_iterator<char>(ifs)),
                      std::istreambuf_iterator<char>());
      SetInput(str);
    }

	ofRunApp(new ofApp());
}
