# Project Euler by Fortran 2008
## テスト環境
gfortran 8.1.0

# Problem 0001 #

## Problem0001_01.f08 ##

### `module Problem0001` ###

#### `function Problem0001_01` ####
- 当該 `function` の引数は `limit` の1個．3か5の倍数であるかを判定する自然数の上限を与える．本問題なら `1000` を与えればよい．
- 反復子 `itr` を `1` から `limit-1` まで `1` ずつ増加させ，組み込み関数 [`mod`](https://gcc.gnu.org/onlinedocs/gfortran/MOD.html) を用いて，`itr` が3か5の倍数であるかを判定する
- 反復子 `itr` は `itr=itr+1` で更新し，`limit` 未満であるか否かは `do while` 構文で判定する．

#### `function Problem0001_02` ####
- 当該 `function` の引数は `limit` の1個．3か5の倍数であるかを判定する自然数の上限を与える．本問題なら `1000` を与えればよい．
- 反復子 `itr` を `1` から `limit-1` まで `1` ずつ増加させ，組み込み関数 [`mod`](https://gcc.gnu.org/onlinedocs/gfortran/MOD.html) を用いて，`itr` が3か5の倍数であるかを判定する
- 反復子 `itr` は `do` 構文で更新する

#### `function Problem0001_03_sub` ####
- 当該 `function` の引数は `divisor` と `limit` の2個．
- `1` から `limit` までの自然数の内，`divsor` の倍数だけの和を求めるための `function`
- 求める `divsor` の倍数の総和の算出には，等差数列の総和の公式を用いる

#### `function Problem0001_03` ####
- 当該 `function` の引数は `limit` の1個．3か5の倍数であるかを判定する自然数の上限を与える．本問題なら `1000` を与えればよい．
- `function Problem0001_03_sub` を用いて，`1` から `limit` までの自然数の内，`3`, `5` および `15` の倍数の総和を求める．`3` ならびに `5` の倍数には，各々 `15` の倍数が含まれているので，`15` の倍数の総和も算出して，その補正を行っている．