# `Fortran` の処理補助モジュール

## [`support_system_clock.f08`](../support_system_clock.f08)

### 当該 `module` の実行テスト済み環境

* GNU Fortran (i686-posix-dwarf-rev0, Built by MinGW-W64 project) 8.1.0

### 当該 `module` の構成

* [`support_system_clock.f08`](../support_system_clock.f08)
  * `typeSysClock` (public type)
  * `getDataSystemClock` (private subroutine)
  * `calcElapsedTime` (public function)

### 当該 `module` の詳細

#### `typeSysClock` (type)

* `Fortran` の組み込み関数 `system_clock` の全ての戻し値を格納するための構造型です．
* 当該構造型の成分は以下の3個です．
  * `INT64` 型整数型変数 `count`
  * `INT64` 型整数型変数 `count_rate`
  * `INT64` 型整数型変数 `count_max`
* 当該構造型の成分はいずれも `private` 属性です．

#### `getDataSystemClock( objToStoreData )` (subroutine)

* 構造型 `typeSysClock` に `Fortran` の組み込み関数 `system_clock` の全ての戻し値を格納するため `subroutine` です．
* 当該 `subroutine` の引数は以下の1個です．
  * 構造型 `typeSysClock` 型オブジェクト `objToStoreData`
    * `Fortran` の組み込み関数 `system_clock` の全ての戻し値が格納されます．
* 当該 `subroutine` は `private` 属性を指定されているため，当該 `module` を使用したとき，当該 `subroutine` は `system_clock( objToStoreData )` としてのみ使用することができます．

#### `calcElapsedTime ( objClockBegin, objClockEnd )` (function)

* `Fortran` の組み込み関数 `system_clock` を利用して，経過時間 `real64` 型浮動小数点数として得るための `function` です．
* 当該関数の引数は以下の2個です．
  * 構造型 `typeSysClock` 型インスタンス `objClockBegin`
    * 求めたい経過時間区間の**開始時刻**を格納したインスタンスです．
  * 構造型 `typeSysClock` 型インスタンス `objClockEnd`
    * 求めたい経過時間区間の**終了時刻**を格納したインスタンスです．

<!-- EOF -->
