# miniparts for Fortran #

## 項目 ##

* [GCD_LCM.f08](#gcd_lcmf08)

## GCD_LCM.f08 ##

### 目的 ###

* Fortran 2008 による、最大公約数 (Greatest Common Divisor:GCD) と最小公倍数 (Least Common Mulitiple:LCM) の算出。

### 実装された `function` と `interface` ###

#### `interface` `GCD_RCR` ####

* 引数は整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を*再帰的手続き*により返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`GCD_RCR_INT08`](#function-gcd_rcr_int08)
  * [`GCD_RCR_INT16`](#function-gcd_rcr_int16)
  * [`GCD_RCR_INT32`](#function-gcd_rcr_int32)
  * [`GCD_RCR_INT64`](#function-gcd_rcr_int64)

#### `interface` `GCD_IND` ####

* 引数は整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を*非再帰的手続き*により返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`GCD_IND_INT08`](#function-gcd_ind_int08)
  * [`GCD_IND_INT16`](#function-gcd_ind_int16)
  * [`GCD_IND_INT32`](#function-gcd_ind_int32)
  * [`GCD_IND_INT64`](#function-gcd_ind_int64)

#### `interface` `LCM_RCR` ####

* 引数は整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を*再帰的手続き*により返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`LCM_RCR_INT08`](#function-lcm_rcr_int08)
  * [`LCM_RCR_INT16`](#function-lcm_rcr_int16)
  * [`LCM_RCR_INT32`](#function-lcm_rcr_int32)
  * [`LCM_RCR_INT64`](#function-lcm_rcr_int64)

#### `interface` `LCM_IND` ####

* 引数は整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を*非再帰的手続き*により返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`LCM_IND_INT08`](#function-lcm_ind_int08)
  * [`LCM_IND_INT16`](#function-lcm_ind_int16)
  * [`LCM_IND_INT32`](#function-lcm_ind_int32)
  * [`LCM_IND_INT64`](#function-lcm_ind_int64)

#### `function` `GCD_RCR_INT08` ####

* 引数は 8bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 8bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` により自身を呼び出しています。
* 当該 `function` は　`interface` [`GCD_RCR`](#interface-gcd_rcr) の構成要素です。

#### `function` `GCD_RCR_INT16` ####

* 引数は 16bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 16bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` により自身を呼び出しています。
* 当該 `function` は　`interface` [`GCD_RCR`](#interface-gcd_rcr) の構成要素です。

#### `function` `GCD_RCR_INT32` ####

* 引数は 32bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 32bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` により自身を呼び出しています。
* 当該 `function` は　`interface` [`GCD_RCR`](#interface-gcd_rcr) の構成要素です。

#### `function` `GCD_RCR_INT64` ####

* 引数は 64bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 64bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` により自身を呼び出しています。
* 当該 `function` は　`interface` [`GCD_RCR`](#interface-gcd_rcr) の構成要素です。

#### `function` `GCD_IND_INT08` ####

* 引数は 8bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 8bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`GCD_IND`](#interface-gcd_ind) の構成要素です。

#### `function` `GCD_IND_INT16` ####

* 引数は 16bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 16bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`GCD_IND`](#interface-gcd_ind) の構成要素です。

#### `function` `GCD_IND_INT32` ####

* 引数は 32bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 32bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`GCD_IND`](#interface-gcd_ind) の構成要素です。

#### `function` `GCD_IND_INT64` ####

* 引数は 64bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最大公約数を 64bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`GCD_IND`](#interface-gcd_ind) の構成要素です。

#### `function` `LCM_RCR_INT08` ####

* 引数は 8bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 8bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` が指定されている関数 `function` [`GCD_RCR_INT08`](#function-gcd_rcr_int08) を用いています。
* 当該 `function` は　`interface` [`LCM_RCR`](#interface-lcm_rcr) の構成要素です。

#### `function` `LCM_RCR_INT16` ####

* 引数は 16bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 16bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` が指定されている関数 `function` [`GCD_RCR_INT16`](#function-gcd_rcr_int16) を用いています。
* 当該 `function` は　`interface` [`LCM_RCR`](#interface-lcm_rcr) の構成要素です。

#### `function` `LCM_RCR_INT32` ####

* 引数は 32bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 32bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` が指定されている関数 `function` [`GCD_RCR_INT32`](#function-gcd_rcr_int32) を用いています。
* 当該 `function` は　`interface` [`LCM_RCR`](#interface-lcm_rcr) の構成要素です。

#### `function` `LCM_RCR_INT64` ####

* 引数は 64bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 64bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` が指定されている関数 `function` [`GCD_RCR_INT64`](#function-gcd_rcr_int64) を用いています。
* 当該 `function` は　`interface` [`LCM_RCR`](#interface-lcm_rcr) の構成要素です。

#### `function` `LCM_IND_INT08` ####

* 引数は 8bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 8bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`LCM_IND`](#interface-lcm_ind) の構成要素です。

#### `function` `LCM_IND_INT16` ####

* 引数は 16bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 16bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`LCM_IND`](#interface-lcm_ind) の構成要素です。

#### `function` `LCM_IND_INT32` ####

* 引数は 32bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 32bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`LCM_IND`](#interface-lcm_ind) の構成要素です。

#### `function` `LCM_IND_INT64` ####

* 引数は 64bit 整数型変数 `n1` と `n2` の 2 個です。
* 整数 `n1` と `n2` の最小公倍数を 64bit 整数型変数として返します。
* 当該 `function` は キーワード `recursive` を用いていません。
* 当該 `function` は　`interface` [`LCM_IND`](#interface-lcm_ind) の構成要素です。

## 参考文献 ##

* [［改訂新版］C言語による標準アルゴリズム事典](https://gihyo.jp/book/2018/978-4-7741-9690-9)