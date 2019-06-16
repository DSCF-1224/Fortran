# Fortranの処理補助モジュール
# Fortran 95
工事中

# Fortran 2008
## テスト環境
gfortran 8.1.0

## 構成
- [support_allocation.f08](#support_allocationf08)
	- `PUBLIC`
		- [function : set_mode](function#--initialize_class)
		- [subroutine : set_mode](#subroutine--set_mode)
		- [subroutine : set_stat](#subroutine--set_stat)
		- [subroutine : set_errmsg](#subroutine--set_errmsg)
		- [function : get_mode](#function--get_mode)
		- [function : get_stat](#function--get_stat)
		- [function : get_errmsg](#function--get_errmsg)
		- [subroutine : evaulate](#subroutine--evaulate)
- [support_io.f08](#support_iof08)
	- `PUBLIC`
		- [subroutine : CheckStatAllocate](#subroutine--checkstatallocate)
		- [subroutine : CheckStatDeallocate](#subroutine--checkstatdeallocate)
		- [subroutine : CheckIostatClose](#subroutine--checkiostatclose)
		- [subroutine : CheckIostatOpen](#subroutine--checkiostatlopen)
		- [subroutine : CheckIostatRead](#subroutine--checkiostatread)
		- [subroutine : CheckIostatWrite](#subroutine--checkiostatwrite)
	- `PRIVATE`
		- [subroutine : CheckIostatReadWrite](#subroutine--checkiostatreadwrite)
		- [subroutine : CheckIostatOpenClose](#subroutine--checkiostatopenclose)
- [support_support.f08](#support_supportf08)
	- [subroutine : PrintOnConsoleStatementName](#subroutine--printonconsolestatementname)
	- [subroutine : PrintOnConsoleError](#subroutine--printonconsolestatement)
	- [subroutine : PrintOnConsoleErrMsg](#subroutine----printonconsoleerrmsg)
	- [subroutine : PrintOnConsoleStatus](#subroutine----printonconsolestatus)
	- [function : JointPath](#function----jointpath)
	- [subroutine : StopWithMessage](#subroutine----stopwithmessage)
	- [subroutine : WaitEnter](#subroutine----waitenter)
	- [subroutine : ReachedTheEnd](#subroutine----reachedtheend)

## support_allocation.f08

### function : `initialize_class`

- 引数は不要です．
- `class(class_status_allocate)` のインスタンスの初期化を行います．

### subroutine : `set_mode`

- 引数には `.true.` または `.false.` を渡します．
- 対象のインスタンスを `ALLOCATE` 文に使用する際には，引数に `.true.` を渡します．
- 対象のインスタンスを `DEALLOCATE` 文に使用する際には，引数に `.false.` を渡します．

### subroutine : `set_stat`

- 対象のインスタンスに，`ALLOCATE/DEALLOCATE` 文の `STAT` の戻し値を渡すために使用します．
- 引数には `ALLOCATE/DEALLOCATE` 文の `STAT` の戻し値を渡します．

### subroutine : `set_errmsg`

- 対象のインスタンスに，`ALLOCATE/DEALLOCATE` 文の `ERRMSG` の戻し値を渡すために使用します．
- 引数には `ALLOCATE/DEALLOCATE` 文の `ERRMSG` の戻し値を渡します．

### function : `get_mode`

- 引数には `.true.` または `.false.` を渡します．
- 対象のインスタンスを `ALLOCATE` 文に使用する際には，引数に `.true.` を渡します．
- 対象のインスタンスを `DEALLOCATE` 文に使用する際には，引数に `.false.` を渡します．

### function : `get_stat`

- 対象のインスタンスに記録された，`ALLOCATE/DEALLOCATE` 文の `STAT` の戻し値を取得するために使用します．
- 対象のインスタンスに，`STAT` の戻し値が記録されていない場合，`STOP` 文が呼び出されます．

### function : `get_errmsg`

- 対象のインスタンスに記録された，`ALLOCATE/DEALLOCATE` 文の `STAT` の戻し値を取得するために使用します．
- 対象のインスタンスに，`STAT` の戻し値が記録されていない場合，`STOP` 文が呼び出されます．

### subroutine : `evaulate`

- `ALLOCATE/DEALLOCATE` 文の `STAT` 及び `ERRMSG` を評価するために使用します．
- 引数は `unit` と `silence` の2個です．
- 引数 `unit` には，上述の評価結果を出力する装置番号を渡します．
- 引数 `silence` には `.true.` または `.false.` を渡します．
- 引数 `silence` に `.true.` を渡したとき，`ALLOCATE/DEALLOCATE` 文が正常に終了していれば，何も出力されません．
- 引数 `silence` に `.false.` を渡したとき，`ALLOCATE/DEALLOCATE` 文の終了状態に関わらず，その状況を示すログが出力されます．
- `ALLOCATE/DEALLOCATE` 文が正常に終了していなければ，引数 `silence` に渡された論理値に関わらず，その結果を示すログが出力されます．


### 使用例 : `support_allocation.f08`

```fortran
program sample
	
	use,     intrinsic :: iso_fortran_env
	use, non_intrinsic :: support_allocation

	implicit none

	! <instance> for this <program>
	class(class_status_allocate) :: stauts

	! variables for this <program>
	character(len=512, kind=1) :: buf_errmsg
	integer  (kind=INT32)      :: buf_stat

	! arrays for this <program>
	real(kind=REAL64), allocatable, dimension(:) :: x

	! STEP.01
	! initialize the instance
	status = initialize_class()

	! STEP.02
	! allocate the target array
	allocate(x(1:10), stat=buf_stat, errmsg=buf_errmsg)
	call status%set_mode  (.true.)
	call status%set_stat  (buf_stat)
	call status%set_errmsg(buf_errmsg)
	call status%evaulate  (unit=OUTPUT_UNIT, silent=silent)

	! STEP.03
	! deallocate the target array
	deallocate(x, stat=buf_stat, errmsg=buf_errmsg)
	call status%set_mode  (.false.)
	call status%set_stat  (buf_stat)
	call status%set_errmsg(buf_errmsg)
	call status%evaulate  (unit=OUTPUT_UNIT, silent=silent)

end program sample
```

### subroutine : `get_mode`

### subroutine : `get_stat`

### subroutine : `get_errmsg`

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
	
	use, intrinsic :: iso_fortran_env 

	implicit none

	character( len=128 ) :: buf_errmsg
	integer              :: statval
	real, allocatable    :: target(:)

	allocate( target(1:10), stat= statval, errmsg= buf_errmsg )
	call CheckStatAllocate( stat= statval, errmsg= buf_errmsg )

end program sample
```

### subroutine : `CheckStatDeallocate`
- [subroutine : CheckStatAllocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatallocate) の機能を `DEALLOCATE` 文に置き換えたもの
- 用法容量は [subroutine : CheckStatAllocate](https://github.com/DSCF-1224/Fortran/tree/master/support#subroutine--checkstatallocate) に等しい
```fortran
program sample
	
	use, intrinsic :: iso_fortran_env 

	implicit none

	character( len=128 ) :: buf_errmsg
	integer              :: statval
	real, allocatable    :: target(:)

	allocate( target(1:10), stat= statval, errmsg= buf_errmsg )
	call CheckStatAllocate( stat= statval, errmsg= buf_errmsg )

	deallocate( target,       stat= statval, errmsg= buf_errmsg )
	call CheckStatDeallocate( stat= statval, errmsg= buf_errmsg )

end program sample
```

### subroutine : `CheckIostatClose` ###
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
```fortran
program sample
	
	use, intrinsic :: iso_fortran_env 

	implicit none

	character( len=128 ) :: buf_iomsg
	integer              :: statval

	open( unit= ???, file= ???, iostat= statval, iomsg= buf_iomsg )
	call CheckIostatOpen( iostat= statval, iomsg= buf_iomsg, silent= .true.\.false. )

	! something to do

	close( unit= ???, iostat= statval, iomsg= buf_iomsg )
	call CheckIostatClose( iostat= statval, iomsg= buf_iomsg, silent= .true.\.false. )

end program sample
```

### subroutine : `CheckIostatOpen` ###
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

```fortran
program sample
	
	use, intrinsic :: iso_fortran_env 

	implicit none

	character( len=128 ) :: buf_iomsg
	integer              :: statval

	open( unit= ???, file= ???, iostat= statval, iomsg= buf_iomsg )
	call CheckIostatOpen( iostat= statval, iomsg= buf_iomsg, silent= .true.\.false. )

	! something to do

	close( unit= ???, iostat= statval, iomsg= buf_iomsg )
	call CheckIostatClose( iostat= statval, iomsg= buf_iomsg, silent= .true.\.false. )

end program sample
```

### subroutine : `CheckIostatRead` ###
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

### subroutine : `CheckIostatWrite` ###
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

### subroutine : `CheckIostatReadWrite` ###
- 当該 `subroutine` は `private` 属性が指定されており，**当該 `module` でしか参照できない．**
- 当該 `subroutine` は当該 `module` 中の `subroutine CheckIostatRead` と `CheckIostatWrite` の共通部分を取り出したものである
- 引数は `iostat`、`iomsg`、`silent` 及び `mode` の4個。
- 引数 `iostat` には `WRITE` 文の `IOSTAT` の戻り値を渡す
- 引数 `iomsg` には `WRITE` 文の `IOMSG` の戻り値を渡す
- 引数 `silent` には論理値を渡す
- 引数 `mode` には当該 `module` 中の `subroutine CheckIostatRead` と `CheckIostatWrite` のどちらに用いるのかを識別するための 32bit 整数型を渡す
	- 当該 `module` 中の 32bit 整数型定数 `mode_CheckIostatReadWrite_READ` を渡した場合は、 `CheckIostatRead` に対応する．
	- 当該 `module` 中の 32bit 整数型定数 `mode_CheckIostatReadWrite_WRITE` を渡した場合は、 `CheckIostatWrite` に対応する．

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