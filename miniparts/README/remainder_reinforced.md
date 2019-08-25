# miniparts for Fortran : remainder_reinforced.f08 #

## 目的 ##

* Fortran 2008 の組み込み関数 `mod` の機能強化番 `function` の定義

## 実装された `function`, `interface` ##

### `interface` `mod_sum` ###

* 引数は整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**和**の `p`による剰余を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`mod_sum_INT8`](#function-mod_sum_int8)
  * [`mod_sum_INT16`](#function-mod_sum_int16)
  * [`mod_sum_INT32`](#function-mod_sum_int32)
  * [`mod_sum_INT64`](#function-mod_sum_int64)

### `interface` `mod_mul` ###

* 引数は整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**積**の `p`による剰余を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`mod_mul_INT8`](#function-mod_mul_int8)
  * [`mod_mul_INT16`](#function-mod_mul_int16)
  * [`mod_mul_INT32`](#function-mod_mul_int32)
  * [`mod_mul_INT64`](#function-mod_mul_int64)

### `function` `mod_sum_INT8` ###

* 引数は 8bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**和**の `p`による剰余を 8bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_sum`](#interface-mod_sum) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_sum_INT16` ###

* 引数は 16bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**和**の `p`による剰余を 16bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_sum`](#interface-mod_sum) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_sum_INT32` ###

* 引数は 32bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**和**の `p`による剰余を 32bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_sum`](#interface-mod_sum) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_sum_INT64` ###

* 引数は 64bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**和**の `p`による剰余を 64bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_sum`](#interface-mod_sum) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_mul_INT8` ###

* 引数は 8bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**積**の `p`による剰余を 8bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_mul`](#interface-mod_mul) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_mul_INT16` ###

* 引数は 16bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**積**の `p`による剰余を 16bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_mul`](#interface-mod_mul) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_mul_INT32` ###

* 引数は 32bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**積**の `p`による剰余を 32bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_mul`](#interface-mod_mul) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

### `function` `mod_mul_INT64` ###

* 引数は 64bit 整数型変数 `a`, `b` 及び `p` の 3 個です。
* 整数 `a` と `b` の**積**の `p`による剰余を 64bit 整数型変数として返します。
* 当該 `function` は　`interface` [`mod_mul`](#interface-mod_mul) の構成要素です。
* `private` 属性を設定しているため，当該 `module` 外から利用することはできません。

<!-- EOF -->
