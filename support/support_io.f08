!----------------------------------------------------------------------!
! [target]                                                             !
! check return value of <STAT> or <STATVAL>                            !
!----------------------------------------------------------------------!
module support_io

	! <module>s to import
	use,     intrinsic :: iso_fortran_env ! gfortran 8.1.0
	use, non_intrinsic :: support_support

	! require all variables to be explicitly declared
	implicit none

	! accessibility of <subroutine>s and <function>s in this <module>
	public :: CheckStatAllocate
	public :: CheckStatDeallocate

	! variables for this <module>
	character( len=len_ErrMsg ), public :: buf_ErrMsg_io


	! <subroutine>s and <function>s for this <module>
	contains


	! chsck <stat> of <allocate> statement
	subroutine CheckStatAllocate( stat, errmsg )

		! argument for this <subroutine>
		integer,                     intent(in)           :: stat
		character( len=len_ErrMsg ), intent(in), optional :: errmsg

		call PrintOnConsoleStatement( "ALLOCATE" )
		call PrintOnConsoleStatus

		select case( stat )
			case(0)
				print '(A/)', 'it have succeeded to allocate the array.'
				return
				! TRUE_END
			case default
				select case( stat )
					case(1:)
						print '(A,$)', 'An unrecoverable error'
					case(:-1)
						print '(A,$)', 'An undefined error'
				end select

				print '(A)', 'was detected !'
				print '(A,I8)', 'stat value is ', stat
				
				if( present(errmsg) ) then
					call PrintOnConsoleErrMsg
					print '(A/)', trim( errmsg )
				end if
				
				call WaitEnter
				call StopWithMessage
				! BAD_END
		end select

	end subroutine


	! chsck <stat> of <allocate> statement
	subroutine CheckStatDeallocate( stat, errmsg )

		! argument for this <subroutine>
		integer,                     intent(in)           :: stat
		character( len=len_ErrMsg ), intent(in), optional :: errmsg

		call PrintOnConsoleStatement( "DEALLOCATE" )
		call PrintOnConsoleStatus

		select case( stat )
			case(0)
				print '(A/)', 'it have succeeded to deallocate the array.'
				return
				! TRUE_END
			case default
				select case( stat )
					case(1:)
						print '(A,$)', 'An unrecoverable error'
					case(:-1)
						print '(A,$)', 'An undefined error'
				end select

				print '(A)', 'was detected !'
				print '(A,I8)', 'stat value is ', stat
				
				if( present(errmsg) ) then
					call PrintOnConsoleErrMsg
					print '(A/)', trim( errmsg )
				end if
				
				call WaitEnter
				call StopWithMessage
				! BAD_END
		end select

	end subroutine

end module