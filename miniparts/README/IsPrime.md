# miniparts for Fortran : IsPrime.f08 #

## 目的 ##

* Fortran 2008 による、素数であるかどうかを判定する `function` の定義。

## 依存関係 ##

* 組み込み `module` [`module iso_fortran_env`](http://fortranwiki.org/fortran/show/iso_fortran_env) を必要とします。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) を必要とします。

## 実装された `function` と `interface` ##

### `interface` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|[`IsPrime`](#interface-prime)|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`logical`|
|[`IsPrime_list`](#interface-prime_list)|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`logical`|

### `function` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|[`IsPrime_INT8`](#function-isprime_int8)|`private`|`INT8`|`logical`|
|[`IsPrime_INT16`](#function-isprime_int16)|`private`|`INT16`|`logical`|
|[`IsPrime_INT32`](#function-isprime_int32)|`private`|`INT32`|`logical`|
|[`IsPrime_INT64`](#function-isprime_int64)|`private`|`INT64`|`logical`|
|[`IsPrime_list_INT8`](#function-isprime_list_int8)|`private`|`INT8`|`logical`|
|[`IsPrime_list_INT16`](#function-isprime_list_int16)|`private`|`INT16`|`logical`|
|[`IsPrime_list_INT32`](#function-isprime_list_int32)|`private`|`INT32`|`logical`|
|[`IsPrime_list_INT64`](#function-isprime_list_int64)|`private`|`INT64`|`logical`|

---

### `interface` `IsPrime` ###

* 引数は整数型変数 `target` の 1 個です。
* 整数 `target` が素数であるかどうかを返します。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`IsMultiple_INT8`](#function-isprime_int8)
  * [`IsMultiple_INT16`](#function-isprime_int16)
  * [`IsMultiple_INT32`](#function-isprime_int32)
  * [`IsMultiple_INT64`](#function-isprime_int64)

---

### `interface` `IsPrime_list` ###

* 引数は整数型変数 `target` と、整数型変数の 1 次元配列 `list` の 1 個です。
* 引数の整数型変数の 1 次元配列 `list` の要素数の指定はありません。
* 整数 `target` が素数であるかどうかを返します。ただし、 1 次元配列 `list` 中の全要素が素数であることが必要です。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`IsPrime_list_INT8`](#function-isprime_int8)
  * [`IsPrime_list_INT16`](#function-isprime_int16)
  * [`IsPrime_list_INT32`](#function-isprime_int32)
  * [`IsPrime_list_INT64`](#function-isprime_int64)

---

### `function` `IsMultiple_INT8` ###

* 引数は 8bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が素数であるかどうかを返します。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime`](#interface-isprime) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsMultiple_INT16` ###

* 引数は 16bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が素数であるかどうかを返します。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime`](#interface-isprime) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsMultiple_INT32` ###

* 引数は 32bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が素数であるかどうかを返します。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime`](#interface-isprime) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsMultiple_INT64` ###

* 引数は 64bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が素数であるかどうかを返します。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime`](#interface-isprime) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

---

### `function` `IsPrime_list_INT8` ###

* 引数は整数型変数 `target` と、整数型変数の 1 次元配列 `list` の 1 個です。
* 引数の整数型変数の 1 次元配列 `list` の要素数の指定はありません。
* 整数 `target` が素数であるかどうかを返します。ただし、 1 次元配列 `list` 中の全要素が素数であることが必要です。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime_list`](#interface-isprime_list) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsPrime_list_INT16` ###

* 引数は整数型変数 `target` と、整数型変数の 1 次元配列 `list` の 1 個です。
* 引数の整数型変数の 1 次元配列 `list` の要素数の指定はありません。
* 整数 `target` が素数であるかどうかを返します。ただし、 1 次元配列 `list` 中の全要素が素数であることが必要です。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime_list`](#interface-isprime_list) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsPrime_list_INT32` ###

* 引数は整数型変数 `target` と、整数型変数の 1 次元配列 `list` の 1 個です。
* 引数の整数型変数の 1 次元配列 `list` の要素数の指定はありません。
* 整数 `target` が素数であるかどうかを返します。ただし、 1 次元配列 `list` 中の全要素が素数であることが必要です。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime_list`](#interface-isprime_list) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。

### `function` `IsPrime_list_INT64` ###

* 引数は整数型変数 `target` と、整数型変数の 1 次元配列 `list` の 1 個です。
* 引数の整数型変数の 1 次元配列 `list` の要素数の指定はありません。
* 整数 `target` が素数であるかどうかを返します。ただし、 1 次元配列 `list` 中の全要素が素数であることが必要です。
* 整数 `target` が素数**であれば**、`.true.` を返します。
* 整数 `target` が素数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsPrime_list`](#interface-isprime_list) で用いられています。
* 当該リポジトリ中の自作 `module` [`mod_IsMultiple`](https://github.com/DSCF-1224/Fortran/tree/master/miniparts) で定義された `interface` `IsMultiple` および同 `IsEven` を用いています。
