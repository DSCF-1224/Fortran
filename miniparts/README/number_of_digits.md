# miniparts for Fortran : number_of_digits.f08 #

## 目的 ##

* Fortran 2008 による、正整数の 10 進法表記時の桁数の取得。

## 実装された `function` と `interface` ##

### `interface` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|number_of_digits_INT8|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`INT8`|
|number_of_digits_INT16|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`INT16`|
|number_of_digits_INT32|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`INT32`|
|number_of_digits_INT64|`public`|`INT8`, `INT16`, `INT32`, `INT64`|`INT64`|

### `function` ###

|name|`public` / `private`|引数の変数型|戻り値の変数型|
|:-|:-:|:-:|:-:|
|number_of_digits_INT8_to_INT8|`private`|`INT8`|`INT8`|
|number_of_digits_INT16_to_INT8|`private`|`INT16`|`INT8`|
|number_of_digits_INT32_to_INT8|`private`|`INT32`|`INT8`|
|number_of_digits_INT64_to_INT8|`private`|`INT64`|`INT8`|
|number_of_digits_INT8_to_INT16|`private`|`INT8`|`INT16`|
|number_of_digits_INT16_to_INT16|`private`|`INT16`|`INT16`|
|number_of_digits_INT32_to_INT16|`private`|`INT32`|`INT16`|
|number_of_digits_INT64_to_INT16|`private`|`INT64`|`INT16`|
|number_of_digits_INT8_to_INT32|`private`|`INT8`|`INT32`|
|number_of_digits_INT16_to_INT32|`private`|`INT16`|`INT32`|
|number_of_digits_INT32_to_INT32|`private`|`INT32`|`INT32`|
|number_of_digits_INT64_to_INT32|`private`|`INT64`|`INT32`|
|number_of_digits_INT8_to_INT64|`private`|`INT8`|`INT64`|
|number_of_digits_INT16_to_INT64|`private`|`INT16`|`INT64`|
|number_of_digits_INT32_to_INT64|`private`|`INT32`|`INT64`|
|number_of_digits_INT64_to_INT64|`private`|`INT64`|`INT64`|
