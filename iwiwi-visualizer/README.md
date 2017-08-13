# ビジュアライザ

## 概要

* emscripten でビルドして Javascript を吐きます
* ビルドされたファイルはこの辺に放り込まれてます https://github.com/imos/icfpc2017/tree/master/www/vis


## ビルドの仕方

1. システムに esmcripten をちゃんとセットアップする。
2. openframeworks を落としてくる。
3. `openFrameworks/scripts/emscripten/download_libs.sh` を実行する。
4. なんか適当に共通Makefileを探してemccへの引数に `--bind` を足して回る。（これをしないとJavascriptからC++の関数が呼べない）
5. このディレクトリを `openFrameworks/apps/` に設置
5. `emmake make`

* openFrameworks はやっつけ仕事ビジュアライザを作るには以下の点で割と便利。
  * 描画とか入力とかの便利関数が割と揃ってて楽
  * Makefile等がはじめからある程度揃ってるので色んなターゲットに向けたビルドができる
* 以前はこんなことなかった気がするので、このバージョンだけの問題かもしれないが、`openFrameworks/scripts/emscripten/download_libs.sh` を叩くと、Macのネイティブなビルドができなくなる。 `openFrameworks/scripts/osx/download_libs.sh` を叩くとMacのネイティブなバイナリが作れるようになるが、これをやるとemscriptenのビルドができなくなる。
* openframeworks 全部突っ込みたいのは山々なんだけど数百 MB あるので辛い。
