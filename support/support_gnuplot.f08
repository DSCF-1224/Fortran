!----------------------------------------------------------------------------------------------------------------------------------!
! [target]                                                                                                                         !
! it is to support to use GNUPLOT                                                                                                  !
!----------------------------------------------------------------------------------------------------------------------------------!
module support_gnuplot

  ! <module>s to import
  use, intrinsic :: iso_fortran_env


  ! require all variables to be explicitly declared
  implicit none


  ! accessibility of <subroutine>s, <function>s and <type>s in this <module>
  public :: comment_gnuplot ! character, constant


  ! constants for this <module>
  character( len= 1, kind= 1 ), parameter :: comment_gnuplot = '#'


end module support_gnuplot