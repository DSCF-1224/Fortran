# Project Euler by Fortran 2008

## テスト環境 ##
gfortran 8.1.0

## 構成 ##
- [Problem 0001](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#problem-0001)
	- [Problem0001_01.f08](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#problem0001_01f08)
		- [`module Problem0001`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#module-problem0001)
			- [`function Problem0001_01`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_01)
			- [`function Problem0001_02`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_02)
			- [`function Problem0001_03_sub`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_03_sub)
			- [`function Problem0001_03`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_03)
	- [main.f08](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#mainf08)
- [Problem 0002](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#problem-0002)
	- [Problem0002_01.f08](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#problem0002_01f08)
		- [`module Problem0002`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#module-problem0002)
			- [`function Calc_FibonacciNum`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-calc_fibonaccinum)
			- [`function Problem0002_01`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0002_01)
			- [`function Problem0002_02`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0002_02)

# Problem 0001 #

## Problem0001_01.f08 ##

### `module Problem0001` ###
- 組み込みモジュール [`module iso_fortran_env`](https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html) を必須とする

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

## main.f08 ##

### `program main` ###
- 組み込みモジュール [`module iso_fortran_env`](https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html) と自作モジュール [`module Problem0001`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#module-problem0001) を必須とする
- [`function Problem0001_01`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_01), [`function Problem0001_02`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_02), [`function Problem0001_03`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-problem0001_03) を運用するための `program` 文

# Problem 0002 #

## Problem0002_01.f08 ##

### `module Problem0002` ###
- 組み込みモジュール [`module iso_fortran_env`](https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html) を必須とする
- `Fibonacci_1st` : Fibonacci 数列の第1項（本問題での定義値）を格納する `parameter` 属性の整数型変数
- `Fibonacci_2nd` : Fibonacci 数列の第2項（本問題での定義値）を格納する `parameter` 属性の整数型変数

#### `function Calc_FibonacciNum` ####
- 当該 `function` の引数は `term` の1個．Fibonacci 数列の第 `term` 項を求める
- 当該 `function` には `recursive` 属性を付与することで，Fibonacci 数列の第 `term` 項を再帰的に求めている

#### `function Problem0002_01` ####
- 当該 `function` の引数は `limit` の1個．Fibonacci 数列の項の値の上限を与える．本問題なら `4*10**6` を与えればよい．
- [`function Calc_FibonacciNum`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-calc_fibonaccinum) を用いて Fibonacci 数列の各項の値を求め，組み込み関数 [`mod`](https://gcc.gnu.org/onlinedocs/gfortran/MOD.html) を用いて，その値が偶数か否かを判定している

#### `function Problem0002_02` ####
- 当該 `function` の引数は `limit` の1個．Fibonacci 数列の項の値の上限を与える．本問題なら `4*10**6` を与えればよい．
- 再帰処理を用いる [`function Calc_FibonacciNum`](https://github.com/DSCF-1224/Fortran/tree/master/ProjectEuler#function-calc_fibonaccinum) では，Fibonacci 数列の各項の値を求めるのに時間を要するため，その演算過程をメモ化している