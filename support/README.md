# `Fortran`の処理補助モジュール
# `Fortran 95`

# `Fortran 2008`
## テスト環境
gfortran 8.1.0
## `support_support.f08`

### subroutine : `PrintOnConsoleStatementName`
- `PRINT` 文を用い、引数 `name` に渡したステートメントの呼称に、 `statement` を `name` の後に付加してコンソールに出力する
```fortran
call PrintOnConsoleStatementName( 'hoge' )
!
! hoge Statement
!
```

### subroutine : `PrintOnConsoleStatement`
- `PRINT` 文を用い、引数 `name` に渡したステートメントの呼称に、 `[statement]` を `name` の前の行に付加してコンソールに出力する。
```fortran
call PrintOnConsoleStatement( 'hoge' )
!
! [Statement]
! hoge
!
```

### subroutine : `PrintOnConsoleError`
- `PRINT` 文を用い、`[Error]` とコンソールに出力する。
```fortran
call PrintOnConsoleStatementName( 'hoge' )
!
! [Error]
!
```

### subroutine : `  PrintOnConsoleErrMsg`
- `PRINT` 文を用い、`[Error Message]` とコンソールに出力する。
```fortran
call PrintOnConsoleStatementName( 'hoge' )
!
! [Error Message]
!
```

### subroutine : `  PrintOnConsoleStatus`
- `PRINT` 文を用い、`[Status]` とコンソールに出力する。
```fortran
call PrintOnConsoleStatementName( 'hoge' )
!
! [Status]
!
```

### function : `  JointPath`
- 引数は　`parent` と `child` の2個
- 引数 `parent` には親ディレクトリ、同 `child` には子ディレクトリにを示す文字列を渡す
- 引数 `parent` に渡した文字列の末尾における `\` の有無は当該 `subroutine` 内で判断させ、`\` がない場合には `parent` と `child` の間に付加することができる
```fortran
call PrintOnConsoleStatementName( parent='hoge', child='fuga' )
!
! hoge\fuga
!

call PrintOnConsoleStatementName( parent='hoge/', child='fuga' )
!
! hoge\fuga
!
```

### subroutine : `  StopWithMessage`
- `STOP` 文を用い、`<STOP> statement was activated !` とコンソールに出力する
```fortran
call StopWithMessage
!
! <STOP> statement was activated !
!
```

### subroutine : `  WaitEnter`
- `READ` 文を用い、キーボードから Enter が入力されるまで、プログラムの実行を中断する
- `READ *` を用いてプログラムの実行を一時停止した場合、何もコンソールに表示されないと、処理が進行中なのか、一時停止中なのか判断がつかない。当該サブルーチンでは、`PRINT` 文を用い、`[TEMPORARY STOP] Please press Enter:`とコンソールすることで、一時停止中であることを明示している
```fortran
call WaitEnter
!
! [TEMPORARY STOP]
! Please press Enter:
!
```

### subroutine : `  ReachedTheEnd`
- `PROGRAM` 文の最後に、「すべての処理が終了しました」という意味の文を表示し、キーボードから Enter が入力されるのを待つ
```fortran
call ReachedTheEnd
!
! All processes have finished successfully.
! press any key to end this process.
!
```