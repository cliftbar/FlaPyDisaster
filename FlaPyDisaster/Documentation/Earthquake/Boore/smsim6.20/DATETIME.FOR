!--------------------- BEGIN GET_DATE  ---------------------
      subroutine get_date(date_c)

! Dates: 04/29/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character date_c*(*)
      integer*2 iyr, imon, iday

! Microsoft compiler version:
!      call GETDAT (iyr, imon, iday)

!      date_c = ' '         ! character date_c*10

!      date_c(3:3) = '/'
!      date_c(6:6) = '/'

!      write(date_c(1:2),'(i2.2)') imon      
!      write(date_c(4:5),'(i2.2)') iday      
!      write(date_c(7:10),'(i4.4)') iyr      

! Lahey compiler version:
      call DATE(date_c)   ! character date_c*8, but *10 is OK

      return
      end
!--------------------- END GET_DATE  ---------------------

!--------------------- BEGIN GET_TIME  ---------------------
      subroutine get_time(time_c)

! Dates: 04/29/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character time_c*(*)
      integer*2 ihr, imin, isec, i100th

! Microsoft compiler version:
!      call GETTIM (ihr, imin, isec, i100th)

!      time_c = ' '             ! character time_c*11 for both compilers

!      time_c(3:3) = ':'
!      time_c(6:6) = ':'
!      time_c(9:9) = '.'

!      write(time_c(1:2),'(i2.2)') ihr      
!      write(time_c(4:5),'(i2.2)') imin      
!      write(time_c(7:8),'(i2.2)') isec      
!      write(time_c(10:11),'(i2.2)') i100th

! Lahey compiler version:
      call TIME(time_c)

      return
      end
!--------------------- END GET_TIME  ---------------------

 