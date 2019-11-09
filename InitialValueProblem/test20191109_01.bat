@REM STEP.01
@REM mingw-w64を環境変数に一時登録
@SET PATH=C:\Program Files\WinOLS_64\Develop\mingw-w64\i686-8.1.0-posix-dwarf-rt_v6-rev0\mingw32\bin;%PATH%

@REM STEP.02
@REM 当該バッチファイルの位置するフォルダをカレントディレクトリに指定する
@CD /D %~dp0

@REM STEP.03
@REM コンパイル開始時刻をコンソールに表示
@ECHO START : %DATE% %TIME%

@REM STEP.04
@REM コンパイル

@gfortran -o test20191109_01.exe -fbounds-check -pedantic-errors -std=f2008 -Wall -Wuninitialized test20191109_01.f08

@REM STEP.03
@REM コンパイル終了時刻をコンソールに表示
@ECHO END  :  %DATE% %TIME%
