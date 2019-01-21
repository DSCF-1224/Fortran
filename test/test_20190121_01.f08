!----------------------------------------------------------------------------------------------------------------------------------!
!                                                                                                                                  !
! [objective]                                                                                                                      !
! how to use `COMPILER_VERSION()` and `COMPILER_OPTIONS()` in module `iso_fortran_env`                                             !
!                                                                                                                                  !
! [reference]                                                                                                                      !
! https://www.ibm.com/support/knowledgecenter/ja/SSGH4D_14.1.0/com.ibm.xlf141.aix.doc/language_ref/env_compilerversion.html        !
! https://www.ibm.com/support/knowledgecenter/ja/SSGH4D_14.1.0/com.ibm.xlf141.aix.doc/language_ref/env_compileroptions.html        !
!                                                                                                                                  !
!----------------------------------------------------------------------------------------------------------------------------------!
program test_20190121_01

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! variables for this <program>
  character( len=*, kind=1 ), parameter :: chr_version = COMPILER_VERSION()
  character( len=*, kind=1 ), parameter :: chr_options = COMPILER_OPTIONS()

  ! STEP.01/01 !
  write( unit=OUTPUT_UNIT, fmt=*, delim='quote' ) chr_version
  write( unit=OUTPUT_UNIT, fmt=*, delim='quote' ) chr_options
  read *

end program test_20190121_01
!----------------------------------------------------------------------------------------------------------------------------------!
! [compile options]                                                                                                                !
! gfortran ^                                                                                                                       !
! -Wall ^                                                                                                                          !
! -pedantic ^                                                                                                                      !
! -fbounds-check ^                                                                                                                 !
! -O -Wuninitialized ^                                                                                                             !
! -fbacktrace ^                                                                                                                    !
! -ffpe-trap=invalid,zero,overflow ^                                                                                               !
! -o test_20190121_01.exe ^                                                                                                        !
! GitHub\Fortran\test\test_20190121_01.f08                                                                                         !
!                                                                                                                                  !
! [result]                                                                                                                         !
! "GCC version 8.1.0"                                                                                                              !
! "-mtune=generic -march=i686 -O -Wall -Wpedantic -Wuninitialized -fbounds-check -fbacktrace -ffpe-trap=invalid,zero,overflow"     !
!----------------------------------------------------------------------------------------------------------------------------------!