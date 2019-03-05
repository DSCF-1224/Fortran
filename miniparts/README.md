# miniparts for Fortran #

## 項目 ##

* [GCD_LCM.f08](#gcd_lcmf08)

## GCD_LCM.f08 ##

### 目的 ###

* Fortran 2008 による、最大公約数 (Greatest Common Divisor:GCD) と最小公倍数 (Least Common Mulitiple:LCM) の算出。

### 実装された `function` と `interface` ###

* `interface` `GCD_RCR`
  * 引数は整数型変数 `n1` と `n2` の 2 個です。
  * 整数 `n1` と `n2` の最大公約数を*再帰的手続き*にを用いて返します。
  * 当該 `interface` は以下の `function` から構成されます。
    * [`GCD_RCR_INT08`](#function-gcd_rcr_INT08)
    * [`GCD_RCR_INT16`](#function-gcd_rcr_INT16)
    * [`GCD_RCR_INT32`](#function-gcd_rcr_INT32)
    * [`GCD_RCR_INT64`](#function-gcd_rcr_INT64)

* `interface` `GCD_IND`
  * 引数は整数型変数 `n1` と `n2` の 2 個です。
  * 整数 `n1` と `n2` の最大公約数を*非再帰的手続き*にを用いて返します。
  * 当該 `interface` は以下の `function` から構成されます。
    * [`GCD_IND_INT08`](#function-gcd_ind_INT08)
    * [`GCD_IND_INT16`](#function-gcd_ind_INT16)
    * [`GCD_IND_INT32`](#function-gcd_ind_INT32)
    * [`GCD_IND_INT64`](#function-gcd_ind_INT64)

* `interface` `LCM_RCR`
  * 引数は整数型変数 `n1` と `n2` の 2 個です。
  * 整数 `n1` と `n2` の最小公倍数を*再帰的手続き*にを用いて返します。
  * 当該 `interface` は以下の `function` から構成されます。
    * [`LCM_RCR_INT08`](#function-lcm_rcr_INT08)
    * [`LCM_RCR_INT16`](#function-lcm_rcr_INT16)
    * [`LCM_RCR_INT32`](#function-lcm_rcr_INT32)
    * [`LCM_RCR_INT64`](#function-lcm_rcr_INT64)

* `interface` `GCD_IND`
  * 引数は整数型変数 `n1` と `n2` の 2 個です。
  * 整数 `n1` と `n2` の最小公倍数を*非再帰的手続き*にを用いて返します。
  * 当該 `interface` は以下の `function` から構成されます。
    * [`LCM_IND_INT08`](#function-lcm_ind_INT08)
    * [`LCM_IND_INT16`](#function-lcm_ind_INT16)
    * [`LCM_IND_INT32`](#function-lcm_ind_INT32)
    * [`LCM_IND_INT64`](#function-lcm_ind_INT64)
