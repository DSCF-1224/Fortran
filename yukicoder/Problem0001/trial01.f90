! yukicoder Problem 0001
! 
! code created : 2018.11.02
! code updated : 2018.11.02
! 
! [reference]
! http://mmxsrup.hatenablog.com/entry/2016/07/16/111859
! http://www.deqnotes.net/acmicpc/dijkstra/


! http://numerical.recipes/pubdom/nrtype.f90.txt
MODULE nrtype

	! Require all variables to be explicitly declared
	implicit none

	integer, parameter :: I4B = SELECTED_INT_KIND(9)
	integer, parameter :: I2B = SELECTED_INT_KIND(4)
	integer, parameter :: I1B = SELECTED_INT_KIND(2)
	integer, parameter :: SPR = KIND(1.0)
	integer, parameter :: DPR = KIND(1.0D0)
	integer, parameter :: SPC = KIND((1.0,1.0))
	integer, parameter :: DPC = KIND((1.0D0,1.0D0))
	integer, parameter :: LGT = KIND(.true.)

END MODULE nrtype



module Exception

	! import modules to use
	USE nrtype

	! Require all variables to be explicitly declared
	implicit none

	contains

	! Exception for the `stat` of <allocate> statement
	subroutine exception_allocalte( val, name )

		! declaration of the arguments
		integer(kind= I4B), intent(in) :: val  ! return value of the <STAT>
		character(len= *),  intent(in) :: name ! name of the target array

		if ( val .gt. 0 ) then
			print "(A)"  , "Error allocating `" // name // "`"
			print "(A,I3)", "<STAT> is", val
			call stop_simple
		else
			return
		endif

	end subroutine exception_allocalte

	! Exception for the `stat` of <deallocate> statement
	subroutine exception_deallocalte( val, name )

		! declaration of the arguments
		integer(kind= I4B), intent(in) :: val  ! return value of the <STAT>
		character(len= *),  intent(in) :: name ! name of the target array

		if ( val .gt. 0 ) then
			print "(A)"  , "Error deallocating `" // name // "`"
			print "(A,I3)", "<STAT> is", val
			call stop_simple
		else
			return
		endif

	end subroutine exception_deallocalte

	! Exception for the `iostat` of <read> statement
	subroutine exception_read( val, name )

		! declaration of the arguments
		integer(kind= I4B), intent(in) :: val  ! return value of the <IOSTAT>
		character(len= *),  intent(in) :: name ! name of the target array

		if ( val .gt. 0 ) then
			print "(A)"  , "Error reading `" // name // "`"
			print "(A,I3)", "<STAT> is", val
			call stop_simple
		else
			return
		endif

	end subroutine exception_read

	! <STOP> statement with fixed and simple comment
	subroutine stop_simple
		stop "<STOP> statement have been activated!"
	end subroutine stop_simple
	

end module Exception



! Main Process
program main

	! import modules to use
	USE nrtype
	USE Exception

	! Require all variables to be explicitly declared
	implicit none

	! constants in this <program>
	integer(kind= I4B), parameter :: Unitnum_Input = 10




	! variables in this <program>
	integer(kind= I4B) :: num_cities ! the number of the cities
	integer(kind= I4B) :: lim_cost   ! the limit of the cost
	integer(kind= I4B) :: inf_cost   ! the upper range of data type of `cost`
	integer(kind= I4B) :: num_roads  ! the number of the loads
	integer(kind= I4B) :: sum_time   ! the total time of the most suitable route

	integer(kind= I4B), dimension(:), allocatable :: city_start ! index of the city of the start of load
	integer(kind= I4B), dimension(:), allocatable :: city_end   ! index of the city of the end of load
	integer(kind= I4B), dimension(:), allocatable :: cost       ! cost of the load to pass
	integer(kind= I4B), dimension(:), allocatable :: time       ! time of the load to pass

	integer(kind= I4B), dimension(:,:), allocatable :: workspace ! work space for dynamic programming

	! support variables in this <program>
	integer(kind= I4B) :: itr_city, itr_cost, itr_road
	integer(kind= I4B) :: statval

	character(len= 128) :: buff_msg

	! STEP.01
	! get the settings of the problem

		! STEP.01.01
		! open the data file
		! open( & 
		! 	unit        = Unitnum_Input , &
		! 	defaultfile = path_working  , & 
		! 	file        = file_setting  , &
		! 	iostat      = statval       , & 
		! 	status      = "old"         , &
		! 	action      = "read"          &
		! )

		! if ( statval .ne. 0 ) stop "Error opening " // path_working // file_setting



		! STEP.01.02
		! read out the number of the cities
		read( unit= *, fmt= *, iostat= statval ) num_cities
		if ( statval .ne. 0 ) stop "Error reading `the number of the cities`" 

		! STEP.01.03
		! read out the limit of the cost
		read( unit= *, fmt= *, iostat= statval ) lim_cost
		if ( statval .ne. 0 ) stop "Error reading `the limit of the cost`" 

		! STEP.01.04
		! read out the number of the loads
		read( unit= *, fmt= *, iostat= statval ) num_roads
		if ( statval .ne. 0 ) stop "Error reading `the number of the loads`" 

		! STEP.01.05
		! allocate the arrays to store data of the problem setting
		allocate( city_start(1:num_roads), stat= statval )
		call exception_allocalte( VAL= statval, NAME= "city_start" )

		allocate( city_end(1:num_roads), stat= statval )
		call exception_allocalte( VAL= statval, NAME= "city_end" )

		allocate( cost(1:num_roads), stat= statval )
		call exception_allocalte( VAL= statval, NAME= "cost" )

		allocate( time(1:num_roads), stat= statval )
		call exception_allocalte( VAL= statval, NAME= "time" )

		allocate( workspace( 1:num_cities, 0:lim_cost ), stat= statval )
		call exception_allocalte( VAL= statval, NAME= "work space" )



		! STEP.01.06
		! read out the number of the city as the start of the load
		read( unit= *, fmt= *, iostat= statval ) city_start(:)
		call exception_read( VAL= statval, NAME= "city_start" )

		! STEP.01.07
		! read out the number of the city as the end of the load
		read( unit= *, fmt= *, iostat= statval ) city_end(:)
		call exception_read( VAL= statval, NAME= "city_end" )

		! STEP.01.08
		! read out the cost of the load
		read( unit= *, fmt= *, iostat= statval ) cost(:)
		call exception_read( VAL= statval, NAME= "cost" )

		! STEP.01.09
		! read out the time of the load
		read( unit= *, fmt= *, iostat= statval ) time(:)
		call exception_read( VAL= statval, NAME= "time" )

		! STEP.01.10
		! close the file
		! close( unit= *, iostat= statval, status="keep" )
		! if ( statval .ne. 0 ) stop "Error closing file" // path_working // file_setting

		! STEP.01.??
		! output the read out values
		! print     "(I5)", num_cities
		! print     "(I5)", lim_cost
		! print     "(I5)", num_roads
		! print   "(50I5)", city_start
		! print   "(50I5)", city_end
		! print  "(300I5)", cost
		! print "(1000I5)", time

	! STEP.02
	! calculation the minimum time

		! STEP.02.01
		! initialize the work space for dynamic programming
		inf_cost              = huge( inf_cost )
		workspace(:,:)        = inf_cost
		workspace(1,lim_cost) = 0

		! STEP.02.02
		! calculate the target using dynamic programming
		do itr_city = 1, num_cities, 1
			do itr_cost = lim_cost, 0, -1
				if ( workspace( itr_city, itr_cost ) .ne. inf_cost ) then
					do itr_road = 1, num_roads, 1
						if ( &
							( itr_city .eq. city_start(itr_road) ) .and. & ! if the start city of `itr_road`-th load was No.`itr_city` city
							( itr_cost - cost(itr_road) .ge. 0 )         & ! if the rest of cost was greater than or equal to the cost of `itr_road`-th load
						) then
							workspace( city_end(itr_road), itr_cost - cost(itr_road) ) &
							= min( &
								workspace( itr_city, itr_cost ) + time(itr_road)           , &
								workspace( city_end(itr_road), itr_cost - cost(itr_road) )   &
							)
						endif
					enddo
				endif
			enddo
		enddo

		! STEP.02.03
		! calculate the total time of the most suitable route
		sum_time = minval( workspace(num_cities,:) )

		if ( sum_time .ge. huge(sum_time) ) then
			sum_time = -1
		endif

		print *, sum_time

	! STEP.03
	! deallocate the arrays to store data of the problem setting
	deallocate( city_start, stat= statval ); call exception_deallocalte( VAL= statval, NAME= "city_start" )
	deallocate(   city_end, stat= statval ); call exception_deallocalte( VAL= statval, NAME= "city_end" )
	deallocate(       cost, stat= statval ); call exception_deallocalte( VAL= statval, NAME= "cost" )
	deallocate(       time, stat= statval ); call exception_deallocalte( VAL= statval, NAME= "time" )
	deallocate(  workspace, stat= statval ); call exception_deallocalte( VAL= statval, NAME= "work space" )

	! STEP.END
	! print *, ""
	! print *, "All processes have finished successfully."

end program main