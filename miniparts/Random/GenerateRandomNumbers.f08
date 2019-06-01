module procedures_random_numbers

  ! <module>s to import
  use, intrinsic :: iso_fortran_env

  ! require all variables to be explicitly declared
  implicit none

  ! accessibility of the <subroutine>s and <function>s in this <module>
  public  :: random_number_normal
  private :: random_number_normal_REAL32
  private :: random_number_normal_REAL64

  ! <interface>s for this <module>
  interface random_number_exp
    module procedure random_number_exp_REAL32
    module procedure random_number_exp_REAL64
  end interface random_number_exp

  interface random_number_normal
    module procedure random_number_normal_REAL32
    module procedure random_number_normal_REAL64
  end interface random_number_normal

  ! contained <subroutine>s and <function>s are below
  contains

  subroutine random_number_exp_REAL32(rand)

    ! arguments for this <subroutine>
    real(REAL32), intent(inout) :: rand

    ! variables for this <subroutine>
    real(REAL32) :: buffer

    call random_number(buffer)

    do while(buffer .eq. 0.0e+000_REAL32)
      call random_number(buffer)
    end do

    rand = - log(buffer)
    return 

  end subroutine random_number_exp_REAL32

  subroutine random_number_exp_REAL64(rand)

    ! arguments for this <subroutine>
    real(REAL64), intent(inout) :: rand

    ! variables for this <subroutine>
    real(REAL64) :: buffer

    call random_number(buffer)

    do while(buffer .eq. 0.0e+000_REAL64)
      call random_number(buffer)
    end do

    rand = - log(buffer)
    return 

  end subroutine random_number_exp_REAL64

  subroutine random_number_normal_REAL32(rand1, rand2)

    ! arguments for this <subroutine>
    real(REAL32), intent(inout) ::  rand1, rand2

    ! variables for this <subroutine>
    real(REAL32) ::rsq, fac

    ! STEP.01
    ! initialization of the variable
    rsq = huge(rsq)

    ! STEP.02
    ! get the set of random numbers which on the unit disk
    do while (rsq .ge. 1.0e+000_REAL32)

      call random_number(rand1)
      call random_number(rand2)

      rand1 = 2.0e+000_REAL32 * rand1 - 1.0e+000_REAL32
      rand2 = 2.0e+000_REAL32 * rand2 - 1.0e+000_REAL32

      rsq = rand1 * rand1 + rand2 * rand2

    end do

    ! STEP.03
    ! convert the uniform random number to the normal random number
    fac   = sqrt(- 2.0e+000_REAL32 * log(rsq) / rsq)
    rand1 = rand1 * fac
    rand2 = rand2 * fac

    ! STEP.END
    return

  end subroutine random_number_normal_REAL32

  subroutine random_number_normal_REAL64(rand1, rand2)

    ! arguments for this <subroutine>
    real(REAL64), intent(inout) ::  rand1, rand2

    ! variables for this <subroutine>
    real(REAL64) ::rsq, fac

    ! STEP.01
    ! initialization of the variable
    rsq = huge(rsq)

    ! STEP.02
    ! get the set of random numbers which on the unit disk
    do while (rsq .ge. 1.0e+000_REAL64)

      call random_number(rand1)
      call random_number(rand2)

      rand1 = 2.0e+000_REAL64 * rand1 - 1.0e+000_REAL64
      rand2 = 2.0e+000_REAL64 * rand2 - 1.0e+000_REAL64

      rsq = rand1 * rand1 + rand2 * rand2

    end do

    ! STEP.03
    ! convert the uniform random number to the normal random number
    fac   = sqrt(- 2.0e+000_REAL64 * log(rsq) / rsq)
    rand1 = rand1 * fac
    rand2 = rand2 * fac

    ! STEP.END
    return

  end subroutine random_number_normal_REAL64

end module procedures_random_numbers