# miniparts for Fortran : IsMultiple.f08 #

## 目的 ##

* Fortran 2008 による、倍数であるかどうかを判定する `function` の定義。

## 実装された `function` と `interface` ##

### `interface` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|[`IsMultiple`](#interface-ismultiple)|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`logical`|
|`IsEven`|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`logical`|
|`IsOdd`|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`logical`|

### `function` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|[`IsMultiple_INT8`](#function-ismultiple_int8)|`private`|`INT8`|`logical`|
|[`IsMultiple_INT16`](#function-ismultiple_int16)|`private`|`INT16`|`logical`|
|[`IsMultiple_INT32`](#function-ismultiple_int32)|`private`|`INT32`|`logical`|
|[`IsMultiple_INT64`](#function-ismultiple_int64)|`private`|`INT64`|`logical`|
|[`IsEven_INT8`](#function-iseven_int8)|`private`|`INT8`|`logical`|
|[`IsEven_INT16`](#function-iseven_int16)|`private`|`INT16`|`logical`|
|[`IsEven_INT32`](#function-iseven_int32)|`private`|`INT32`|`logical`|
|[`IsEven_INT64`](#function-iseven_int64)|`private`|`INT64`|`logical`|
|[`IsOdd_INT8`](#function-isodd_int8)|`private`|`INT8`|`logical`|
|[`IsOdd_INT16`](#function-isodd_int16)|`private`|`INT16`|`logical`|
|[`IsOdd_INT32`](#function-isodd_int32)|`private`|`INT32`|`logical`|
|[`IsOdd_INT64`](#function-isodd_int64)|`private`|`INT64`|`logical`|

### `interface` `IsMultiple` ###

* 引数は整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 同 `ref` の倍数**であれば**、`.true.` を返します。
* 整数 `target` が 同 `ref` の倍数**でなければ**、`.false.` を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`IsMultiple_INT8`](#function-ismultiple_int8)
  * [`IsMultiple_INT16`](#function-ismultiple_int16)
  * [`IsMultiple_INT32`](#function-ismultiple_int32)
  * [`IsMultiple_INT64`](#function-ismultiple_int64)

### `interface` `IsEven` ###

* 引数は整数型変数 `target` の 1 個です。
* 整数 `target` が 偶数であるかどうかを返します。
* 整数 `target` が 偶数**であれば**、`.true.` を返します。
* 整数 `target` が 偶数**でなければ**、`.false.` を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`IsEven_INT8`](#function-iseven_int8)
  * [`IsEven_INT16`](#function-iseven_int16)
  * [`IsEven_INT32`](#function-iseven_int32)
  * [`IsEven_INT64`](#function-iseven_int64)

### `interface` `IsOdd` ###

* 引数は整数型変数 `target` の 1 個です。
* 整数 `target` が 奇数であるかどうかを返します。
* 整数 `target` が 奇数**であれば**、`.true.` を返します。
* 整数 `target` が 奇数**でなければ**、`.false.` を返します。
* 当該 `interface` は以下の `function` から構成されます。
  * [`IsOdd_INT8`](#function-isodd_int8)
  * [`IsOdd_INT16`](#function-isodd_int16)
  * [`IsOdd_INT32`](#function-isodd_int32)
  * [`IsOdd_INT64`](#function-isodd_int64)

### `function` `IsMultiple_INT8` ###

* 引数は 8bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 同 `ref` の倍数**であれば**、`.true.` を返します。
* 整数 `target` が 同 `ref` の倍数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) で用いられています。

### `function` `IsMultiple_INT16` ###

* 引数は 16bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 同 `ref` の倍数**であれば**、`.true.` を返します。
* 整数 `target` が 同 `ref` の倍数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) で用いられています。

### `function` `IsMultiple_INT32` ###

* 引数は 32bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 同 `ref` の倍数**であれば**、`.true.` を返します。
* 整数 `target` が 同 `ref` の倍数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) で用いられています。

### `function` `IsMultiple_INT64` ###

* 引数は 64bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 同 `ref` の倍数**であれば**、`.true.` を返します。
* 整数 `target` が 同 `ref` の倍数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) で用いられています。

### `function` `IsEven_INT8` ###

* 引数は 8bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 偶数**であれば**、`.true.` を返します。
* 整数 `target` が 偶数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsEven`](#interface-iseven) で用いられています。

### `function` `IsEven_INT16` ###

* 引数は 16bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 偶数**であれば**、`.true.` を返します。
* 整数 `target` が 偶数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsEven`](#interface-iseven) で用いられています。

### `function` `IsEven_INT32` ###

* 引数は 32bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 偶数**であれば**、`.true.` を返します。
* 整数 `target` が 偶数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsEven`](#interface-iseven) で用いられています。

### `function` `IsEven_INT64` ###

* 引数は 64bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 偶数**であれば**、`.true.` を返します。
* 整数 `target` が 偶数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsEven`](#interface-iseven) で用いられています。

### `function` `IsOdd_INT8` ###

* 引数は 8bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 奇数**であれば**、`.true.` を返します。
* 整数 `target` が 奇数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsOdd`](#interface-isodd) で用いられています。

### `function` `IsOdd_INT16` ###

* 引数は 16bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 奇数**であれば**、`.true.` を返します。
* 整数 `target` が 奇数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsOdd`](#interface-isodd) で用いられています。

### `function` `IsOdd_INT32` ###

* 引数は 32bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 奇数**であれば**、`.true.` を返します。
* 整数 `target` が 奇数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsOdd`](#interface-isodd) で用いられています。

### `function` `IsOdd_INT64` ###

* 引数は 64bit 整数型変数 `target` と `ref` の 2 個です。
* 整数 `target` が 同 `ref` の倍数であるかどうかを返します。
* 整数 `target` が 奇数**であれば**、`.true.` を返します。
* 整数 `target` が 奇数**でなければ**、`.false.` を返します。
* 当該 `function` は `interface` [`IsMultiple`](#interface-ismultiple) を用いています。
* 当該 `function` は `interface` [`IsOdd`](#interface-isodd) で用いられています。
