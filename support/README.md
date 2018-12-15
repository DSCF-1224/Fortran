# Fortranの処理補助モジュール
# Fortran 95
工事中

# Fortran 2008
## テスト環境
gfortran 8.1.0

## 構成
- [support_io.f08](https://github.com/DSCF-1224/Fortran/tree/master/support#support_iof08)
	- [subroutine : CheckStatAllocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatallocate)
	- [subroutine : CheckStatDeallocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatdeallocate)
	- [subroutine : CheckIostatClose](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatclose)
	- [subroutine : CheckIostatOpen](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatlopen)
	- [subroutine : CheckIostatRead](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatread)
	- [subroutine : CheckIostatWrite](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatwrite)
- [support_support.f08](https://github.com/DSCF-1224/Fortran/tree/master/support#support_supportf08)
	- [subroutine : PrintOnConsoleStatementName](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--printonconsolestatementname)
	- [subroutine : PrintOnConsoleError](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--printonconsolestatement)
	- [subroutine : PrintOnConsoleErrMsg](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine----printonconsoleerrmsg)
	- [subroutine : PrintOnConsoleStatus](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine----printonconsolestatus)
	- [function : JointPath](https://github.com/DSCF-1224/Fortran/tree/master/support#function----jointpath)
	- [subroutine : StopWithMessage](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine----stopwithmessage)
	- [subroutine : WaitEnter](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine----waitenter)
	- [subroutine : ReachedTheEnd](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine----reachedtheend)

## support_io.f08
### subroutine : `CheckStatAllocate`
- 引数は `stat` 、 `errmsg` 及び `silent` の3個。`errmsg` と `silent` は省略可能
- 引数 `stat` には `ALLOCATE` 文の `STAT` の戻り値を渡す
- 引数 `errmsg` には `ALLOCATE` 文の `ERRMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `stat` に渡された値によって、当該 `subroutine` の処理は変化する
	- `stat` が*ゼロに等しい*場合、動的配列の割り付けに成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- `stat` が*ゼロに等しくない*場合、動的配列の割り付けに失敗したことと `stat` の値をコンソールに出力し、当該 `subroutine` を `call` した `program` 文の実行を中断する
	- 引数 `errmsg` が与えられていれば、`stat` の値に続けてコンソールに出力する
- 引数 `silent` に渡された値によって、当該 `subroutine` の処理は変化する
	- 「 `silent` が `.true.` 」かつ「動的配列の割り付けに成功した」場合、コンソールには何も出力されず、当該 `subroutine` から正常に離脱する
	- 「 `silent` が `.false.` 」かつ「動的配列の割り付けに成功した」場合、動的配列の割り付けに成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- 「動的配列の割り付けに失敗した」場合、 `silent` の状態に依らずその旨がコンソールに出力される

```fortran
program sample

	implicit none

	character( len=128 ) :: buf_errmsg
	integer              :: statval
	real, allocatable    :: target(:)

	allocate( target(1:10), stat= statval, errmsg= buf_errmsg )
	call CheckStatAllocate( stat= statval, errmsg= buf_errmsg )

end sample
```

### subroutine : `CheckStatDeallocate`
- [subroutine : CheckStatAllocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatallocate) の機能を `DEALLOCATE` 文に置き換えたもの
- 用法容量は [subroutine : CheckStatAllocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatallocate) に等しい
```fortran
program sample

	implicit none

	character( len=128 ) :: buf_errmsg
	integer              :: statval
	real, allocatable    :: target(:)

	allocate( target(1:10), stat= statval, errmsg= buf_errmsg )
	call CheckStatAllocate( stat= statval, errmsg= buf_errmsg )

	deallocate( target,       stat= statval, errmsg= buf_errmsg )
	call CheckStatDeallocate( stat= statval, errmsg= buf_errmsg )

end sample
```

### subroutine : CheckIostatClose ###
- 引数は `iostat` 、 `iomsg` 及び `silent` の3個。`iomsg` と `silent` は省略可能
- 引数 `iostat` には `CLOSE` 文の `IOSTAT` の戻り値を渡す
- 引数 `iomsg` には `CLOSE` 文の `IOMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `iostat` に渡された値によって、当該 `subroutine` の処理は変化する
	- `iostat` が*ゼロに等しい*場合、 `CLOSE` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- `iostat` が*ゼロに等しくない*場合、 `CLOSE` 文の実行に失敗したことと `iostat` の値をコンソールに出力し、当該 `subroutine` を `call` した `program` 文の実行を中断する
	- 引数 `iomsg` が与えられていれば、`iostat` の値に続けてコンソールに出力する
- 引数 `silent` に渡された値によって、当該 `subroutine` の処理は変化する
	- 「 `silent` が `.true.` 」かつ「 `CLOSE` 文の実行に成功した」場合、コンソールには何も出力されず、当該 `subroutine` から正常に離脱する
	- 「 `silent` が `.false.` 」かつ「 `CLOSE` 文の実行に成功した」場合、 `CLOSE` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- 「 `CLOSE` 文の実行に失敗した」場合、 `silent` の状態に依らずその旨がコンソールに出力される

### subroutine : CheckIostatOpen ###
- 引数は `iostat` 、 `iomsg` 及び `silent` の3個。`iomsg` と `silent` は省略可能
- 引数 `iostat` には `OPEN` 文の `IOSTAT` の戻り値を渡す
- 引数 `iomsg` には `OPEN` 文の `IOMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `iostat` に渡された値によって、当該 `subroutine` の処理は変化する
	- `iostat` が*ゼロに等しい*場合、 `OPEN` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- `iostat` が*ゼロに等しくない*場合、 `OPEN` 文の実行に失敗したことと `iostat` の値をコンソールに出力し、当該 `subroutine` を `call` した `program` 文の実行を中断する
	- 引数 `iomsg` が与えられていれば、`iostat` の値に続けてコンソールに出力する
- 引数 `silent` に渡された値によって、当該 `subroutine` の処理は変化する
	- 「 `silent` が `.true.` 」かつ「 `OPEN` 文の実行に成功した」場合、コンソールには何も出力されず、当該 `subroutine` から正常に離脱する
	- 「 `silent` が `.false.` 」かつ「 `OPEN` 文の実行に成功した」場合、 `OPEN` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- 「 `OPEN` 文の実行に失敗した」場合、 `silent` の状態に依らずその旨がコンソールに出力される

### subroutine : CheckIostatRead ###
- 引数は `iostat` 、 `iomsg` 及び `silent` の3個。`iomsg` と `silent` は省略可能
- 引数 `iostat` には `READ` 文の `IOSTAT` の戻り値を渡す
- 引数 `iomsg` には `READ` 文の `IOMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `iostat` に渡された値によって、当該 `subroutine` の処理は変化する
	- `iostat` が*ゼロに等しい*場合、 `READ` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- `iostat` が*ゼロに等しくない*場合、 `READ` 文の実行に失敗したことと `iostat` の値をコンソールに出力し、当該 `subroutine` を `call` した `program` 文の実行を中断する
	- 引数 `iomsg` が与えられていれば、`iostat` の値に続けてコンソールに出力する
- 引数 `silent` に渡された値によって、当該 `subroutine` の処理は変化する
	- 「 `silent` が `.true.` 」かつ「 `READ` 文の実行に成功した」場合、コンソールには何も出力されず、当該 `subroutine` から正常に離脱する
	- 「 `silent` が `.false.` 」かつ「 `READ` 文の実行に成功した」場合、 `READ` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- 「 `READ` 文の実行に失敗した」場合、 `silent` の状態に依らずその旨がコンソールに出力される

### subroutine : CheckIostatWrite ###
- 引数は `iostat` 、 `iomsg` 及び `silent` の3個。`iomsg` と `silent` は省略可能
- 引数 `iostat` には `WRITE` 文の `IOSTAT` の戻り値を渡す
- 引数 `iomsg` には `WRITE` 文の `IOMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `iostat` に渡された値によって、当該 `subroutine` の処理は変化する
	- `iostat` が*ゼロに等しい*場合、 `WRITE` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- `iostat` が*ゼロに等しくない*場合、 `WRITE` 文の実行に失敗したことと `iostat` の値をコンソールに出力し、当該 `subroutine` を `call` した `program` 文の実行を中断する
	- 引数 `iomsg` が与えられていれば、`iostat` の値に続けてコンソールに出力する
- 引数 `silent` に渡された値によって、当該 `subroutine` の処理は変化する
	- 「 `silent` が `.true.` 」かつ「 `WRITE` 文の実行に成功した」場合、コンソールには何も出力されず、当該 `subroutine` から正常に離脱する
	- 「 `silent` が `.false.` 」かつ「 `WRITE` 文の実行に成功した」場合、 `WRITE` 文の実行に成功したことをコンソールに出力し、当該 `subroutine` から正常に離脱する
	- 「 `WRITE` 文の実行に失敗した」場合、 `silent` の状態に依らずその旨がコンソールに出力される

## support_support.f08

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