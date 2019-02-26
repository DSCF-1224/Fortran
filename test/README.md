# Fortran/test #

## 項目 ##
- [`test_20181220_01.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20181220_01f08)
- [`test_20181220_02.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20181220_02f08)
- [`test_20190121_01.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20190121_01f08)
- [`test_20190121_02.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20190121_02f08)
- [`test_20190131_01.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20190131_01f08)
- [`test_20190201_01.f08`](https://github.com/DSCF-1224/Fortran/tree/master/test#test_20190201_01f08)

## `test_20181220_01.f08` ##
- 「`gnuplot` の `plot` に用いるバイナリデータファイルの生成」が目的

## `test_20181220_02.f08` ##
- [`'Fortran/support/support_date_and_time.f08'` ](https://github.com/DSCF-1224/Fortran/tree/master/support)の実行テスト
- [`'Fortran/support/support_system_clock.f08'` ](https://github.com/DSCF-1224/Fortran/tree/master/support)の実行テスト

## `test_20190121_01.f08` ##
- 組み込みモジュール `iso_fortran_env` 中の `COMPILER_VERSION()` の使用方法の確認
- 組み込みモジュール `iso_fortran_env` 中の `COMPILER_OPTIONS()` の使用方法の確認

## `test_20190121_02.f08` ##
- 自作モジュール `Mod_Interpolation_1D` 中の `Calc_Coefficients_CubicSpline_FF` の実行テスト
- 自作モジュール `Mod_Interpolation_1D` 中の `Calc_Coefficients_CubicSpline_SS` の実行テスト
- 自作モジュール `Mod_Integration_1D` 中の `Calc_CubicSplineIntegration_ByData` の実行テスト

## `test_20190131_01.f08` ##
- OpenMP の練習
- `!$omp parallel ... !$omp end parallel' の利用方法の確認

## `test_20190201_01.f08` ##
- OpenMP の練習
- `!$omp parallel ... !$omp end parallel' の利用方法の確認
- `!$omp sections ... !$omp end sections' の利用方法の確認
- `!$omp section' の利用方法の確認
- [gnuplot](http://www.gnuplot.info/) を OpenMP の `!$omp parallel ...` と `!$omp sections ...` を介して複数同時に起動し, グラフを描画可能なのかの検証
	- 外部データの読み込みを伴わない `plot` に成功. 読み込み対象は以下の2個の PLT ファイル
		- test_20190201_01_01.plt
		- test_20190201_01_02.plt

