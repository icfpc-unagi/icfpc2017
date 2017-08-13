#pragma once

#include "ofMain.h"

void SetInput(const string &str);

class ofApp : public ofBaseApp{

 public:
  void setup();
  void update();
  void draw();

  void keyPressed(int key);
  void keyReleased(int key);
  void mouseMoved(int x, int y );
  void mouseDragged(int x, int y, int button);
  void mousePressed(int x, int y, int button);
  void mouseReleased(int x, int y, int button);
  void mouseEntered(int x, int y);
  void mouseExited(int x, int y);
  void windowResized(int w, int h);
  void dragEvent(ofDragInfo dragInfo);
  void gotMessage(ofMessage msg);

  float line_width;
  void mySetLineWidth(float w) {
    line_width = w;
  }

  void myDrawLine(ofVec2f a, ofVec2f b) {
    ofVec2f v = (b - a);
    v *= line_width / 2 / v.length();

    ofSetPolyMode(OF_POLY_WINDING_NONZERO);
    ofBeginShape();
    ofVertex(a + v.getRotated(90));
    ofVertex(a - v.getRotated(90));
    ofVertex(b - v.getRotated(90));
    ofVertex(b + v.getRotated(90));
    ofEndShape();
  }
};
