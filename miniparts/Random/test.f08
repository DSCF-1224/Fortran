program main

  ! <module>s to import
  use,     intrinsic :: iso_fortran_env
  use, non_intrinsic :: procedures_random_numbers

  ! require all variables to be explicitly declared
  implicit none

  ! parameters for this <module>
  integer, parameter :: UNIT_SAVE = 101
  integer, parameter :: num_data  = 2**15

  ! variables for this <program>
  integer                   :: itr_elem
  real(real32), allocatable :: data_rand(:)

  ! STEP.01
  ! open the file to save the result
  open(unit=UNIT_SAVE, file='D:\Desk\test_20190422_01.dat', action='write', status='replace')

  ! STEP.02
  ! allocate to store the random numbers
  allocate(data_rand(1:num_data))

  ! STEP.03
  ! generate the normal random numbers
  do itr_elem = 1, num_data, 1
    call random_number_exp(data_rand(itr_elem))
  end do

  ! STEP.04
  ! save the data
  do itr_elem = 1, num_data, 1
    write(unit=UNIT_SAVE, fmt='(ES23.15e3)', advance='yes') data_rand(itr_elem)
  end do

  print *, sum(data_rand(:), dim=1) / num_data

  ! STEP.05
  ! close the file to save the result
  close(unit=UNIT_SAVE, status='keep')

  ! STEP.06
  ! deallocate to store the random numbers
  deallocate(data_rand)

end program main