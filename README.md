# MomoRISC

教育用のCPUエミュレータ．主にメモリ上のデータ構造の解説に使う予定．

とりあえず[動くもの](https://yuyaaizawa.github.io/momorisc/)．


## 特徴

- アセンブリの記述とステップ実行が可能
- 10進数を採用


## 操作方法

`Edit`タブでプログラムを書き，`Check & Run`タブで動作確認する

- `Edit`タブ
  - プログラムを書き換えることができる
- `Check & Run`タブ
  - エラー個所は赤で表示
  - 左下の`Step`を押すと1命令ずつ実行
  - 行頭の三角形は実行箇所
- `Resister`
  - 各種レジスタの値を表示
  - 変化した箇所が赤で強調される
- `Memory`
  - データメモリの値を表示
  - 変化した箇所が赤で強調される


## 今後増やしたい内容

- CPU機能
  - [x] ステップ実行
  - [ ] 入出力
    - [x] 数字
    - [x] 文字列
    - [ ] グラフプロッタ
- エディタ機能
  - [x] アセンブラの実装 
  - [x] `Check & Run`に命令用メモリのアドレスを表示
  - [x] 自動ステップ実行
  - [ ] メモリのダンプ＆ロード
  - [ ] サンプルプログラムのロード
  - [ ] ブロック図でデータ転送を表示
- アセンブリチュートリアル（サンプルプログラムの実装後）
  - レジスタ間演算
  - データメモリ入出力
  - 条件分岐
  - 4桁の加算
  - ループ
  - バブルソート
  - コールスタック
  - クイックソート
  - etc.


## アーキテクチャ

```
                                      +-IO_DEVICE--------+      
                                      |                  |      
     +-INST_MEM---------+             | +-DATA_MEM---------+    
     |                  |             +-|                  |    
     |                  |               |                  |    
     +------------------+               +------------------+    
  addr ^        inst |               addr ^   data  ^    | data 
       |             |                    |    store|    |  load
+-CPU--+-------------+--------------------+---------+----+--+   
|      |             |                    |         |    |  |   
|      |     +-------+                    |         |    |  |   
|      |     |       |                    |         |    |  |   
|      |  +----+  +-----+               +----+    +----+ |  |   
|      |  | OP |  | IMM |               | MA |    | MD | |  |   
|      |  +----+  +-----+               +----+    +----+ |  |   
|      |             |                    ^         ^    |  |   
|      |    aBUS     v                    |         |    |  |   
|   +--|--+  ====================================== |    |  |   
|   |  |  |   |    |          ^     ^     ^     ^   |    |  |   
|   |  |  v   v    |   oBUS   |     |     |     |   |    |  |   
|   |  | =======   |    ==============================   |  |   
|   |  |    |      |     |    | ^   | ^   | ^   | ^      |  |   
|   |  |    v      v     v    | |   | |   | |   | |      |  |   
|   |  |  +----+ +---+ +---+ +---+ +---+ +---+ +---+     |  |   
|   |  |  | PC |  \   V   /  | A | | B | | C | | D |     |  |   
|   |  |  +----+   \ ALU /   +---+ +---+ +---+ +---+     |  |   
|   |  |    |       \___/      ^     ^     ^     ^       |  |   
|   |  +----+         |        |     |     |     |       |  |   
|   |       |    iBUS v        |     |     |     |       v  |   
|   |       v     ========================================= |   
|   |   +-------+  ^                                        |   
|   |    \ +1  /   |                                        |   
|   |     \___/    |                                        |   
|   |      | |     |                                        |   
|   +------+ +-----+                                        |   
|                                                           |   
+-----------------------------------------------------------+   
```

- bit数は10進2桁（1 dudit）
- メモリは命令用-データ用を分離
  - どちらも100エントリ
- 演算はレジスタ-レジスタ/即値間のみ


## 命令セット
形式の`R`ではじまるものは汎用レジスタ（`A`, `B`, `C`, `D`）を表す．
`imm`は`0~99`までの値．

### データ読み書き

#### LDI
- 形式：LDI R imm
- 疑似コード：R ← imm, PC++

#### LD
- 形式：LD R1 R2
- 疑似コード：R1 ← Mem[R2], PC++

#### ST
- 形式：ST R1 R2
- 疑似コード：Mem[R1] ← R2, PC++

### 演算

#### ADI
- 形式：ADI R1 R2 imm
- 疑似コード：R1 ← R2 + imm, PC++
- 補足：結果はwrap-aroundされる

#### ADD
- 形式：ADD R1 R2 R3
- 疑似コード：R1 ← R2 + R3, PC++
- 補足：結果はwrap-aroundされる

#### SUB
- 形式：SUB R1 R2 R3
- 疑似コード：R1 ← R2 - R3, PC++
- 補足：結果はwrap-aroundされる

### 条件分岐

#### BEQ
- 形式：BEQ R1 R2 imm
- 疑似コード：if(R1 = R2) then PC ← imm else PC++

#### BLT
- 形式：BLT R1 R2 imm
- 疑似コード：if(R1 < R2) then PC ← imm else PC++

### 無条件分岐

#### JPI
- 形式：JPI R imm
- 疑似コード：R ← PC + 1, PC ← imm

#### JP
- 形式：JP R1 R2
- 疑似コード：R1 ← PC + 1, PC ← R2

### その他

#### HLT
- 形式：HLT
- 疑似コード：-
- 補足：実行を止める．エラー行などに適宜補われる．


## 入出力
90番地以降のアドレスへの読み書きは入出力デバイスへのアクセスとなる．

### 数値入出力
- 90番地から`LD`はInput欄の数字を読み取る
  - 読み取りは2桁ごと
  - 数字以外の文字は読み飛ばす
  - 読み取られるか飛ばされた文字は消費
- 90番地へ`ST`してOutput欄へ数字を書き出す
  - 書き出しは2桁ごと

### 文字入出力
- 91番地から`LD`はInput欄の文字を読み取る
  - 読取りは1文字誤と
  - 文字コードに無い文字は読み飛ばす
  - 読み取られるか飛ばされた文字は消費
- 90番地へ`ST`してOutput欄へ文字を書き出す
  - 書き出しは1文字ごと

### 文字コード
|      | 0 |  1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |  9 |
| ----:|:-:|:--:|:-:|:-:|:-:|:-:|:-:|:-:|:-:|:--:|
| **0**|   |空白| , | . | ! | ? | ' | " | : |  ; |
|**10**| 0 |  1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |  9 |
|**20**| A |  B | C | D | E | F | G | H | I |  J |
|**30**| K |  L | M | N | O | P | Q | R | S |  T |
|**40**| U |  V | W | X | Y | Z | ( | ) | { |  } |
|**50**| a |  b | c | d | e | f | g | h | i |  j |
|**60**| k |  l | m | n | o | p | q | r | s |  t |
|**70**| u |  v | w | x | y | z | < | > | [ |  ] |
|**80**| + |  - | / |\\ |\| | _ | $ | % | & |  @ |
|**90**| ^ |  ~ | = | * |\` |   |   |   |   |改行|


## Build

[Elm](https://elm-lang.org/)コンパイラが必要．Elm言語については[日本語ガイド](https://guide.elm-lang.jp/)が詳しい．

```bash
$ elm make src/Main.elm --output=public/js/elm.js
```

上記コマンドを実行した後，`public/index.html`をブラウザで開いて実行する．
