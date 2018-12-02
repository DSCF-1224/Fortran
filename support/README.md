# `Fortran`の処理補助モジュール
# `Fortran 95`

# `Fortran 2008`
## `support_support.f08`
### テスト環境
gfortran 8.1.0

### subroutine : `PrintOnConsoleStatementName`
- `PRINT` 文を用い、引数 `name` に渡したステートメントの呼称に、 `statement` を `name` の後に付加してコンソールに出力するだけ。

### subroutine : `PrintOnConsoleStatement`
- `PRINT` 文を用い、引数 `name` に渡したステートメントの呼称に、 `[statement]` を `name` の前の行に付加してコンソールに出力するだけ。

### subroutine : `PrintOnConsoleError`
- `PRINT` 文を用い、`[Error]` とコンソールに出力するだけ。

### subroutine : `  PrintOnConsoleErrMsg`
- `PRINT` 文を用い、`[Error Message]` とコンソールに出力するだけ。

### subroutine : `  PrintOnConsoleStatus`
- `PRINT` 文を用い、`[Status]` とコンソールに出力するだけ。