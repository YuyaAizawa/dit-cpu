# MomoRISC

教育用のCPUエミュレータ．主にメモリ上のデータ構造の解説に使う予定．

とりあえず[動くもの](https://yuyaaizawa.github.io/momorisc/)．

## build

```bash
$ elm make src/Main.elm --output=public/js/elm.js
```

## メモ

- メモリ上でのデータ表現のイメージを掴む
- 基本演算は10進2桁
- プログラム-データ分離式
  - どちらもアドレスは100個
  - プログラムのアドレスは何命令目かを表現（命令長固定）