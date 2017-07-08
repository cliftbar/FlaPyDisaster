      Program ratios_of_loop_program_output
      
!!Control file for program RATIOS_OF_LOOP_PROGRAM_OUTPUT
!! As many comment lines as desired, each starting with "!"
!! The string "pp:" indicates a new set of processing parameters
!! to be applied to the following smc files.  The parameters are given on the
!! following lines.
!! Revision of program involving a change in the control file on this date:
!! This version of the program assumes that the format of the two files to be used in computing
!! ratios is the same.  Look at the program ratios_from_rv_td_loop_drvr when that might not
!! be the case.
!   03/03/16
!!Name of summary file:
!  ratios_of_loop_program_output.sum
!!Name of output file:
!  ratios_of_loop_program_output.out
!!Relative accuracy of period to be equal (dPer/Per):
! 0.001
!!Output from which program?
!! 1 = tmrsk_loop_rv_drvr
!! 2 = tmrsk_loop_td_drvr
!! 3 = fmrsk_loop_fas_drvr
! 1
!!Lines to skip in files 1 and 2 to reach simulation values (for tmrsk_loop_rv_drvr output
!! this is 3).  Enter values for file 1, then file 2
! 3 
! 3
!!ncols2read and ncol4gmim for files 1 and 2. 
!! For tmrsk_loop_rv_drvr or tmrsk_loop_td_drvr output with the option for less output:
!!   ncols2read=14, and ncol4gmim = 13 or 14 for units of g or cgs, respectively. 
!! For fmrsk_loop_fas_drvr:
!!   ncols2read=13, and ncol4gmim = 12 or 13 for D or A FS respectively. 
!!Note that this version of the program assumes the less output option 
!! for the tmrsk_loop_rv_drvr or tmrsk_loop_td_drvr programs
!! Enter values for file 1, then file 2
! 14 13
! 14 13
!!List of smsim files,  The ratios will be both file1/file2 and file2/file1
!! NOTE: This version is not completely general, as it assumes that the
!! first file was made using the RV program (this assumption is needed
!! in order to extract the various durations).
!!
!tmrsk_loop_rv_drvr.ena.scf.bt15scr_fff.bs11_atten.bt15_dp.dmb_3kps_amps_aoi_00.bt15e_drms.col
!tmrsk_loop_rv_drvr.ena.scf.bt15scr_fff.bs11_atten.bt15_dp.dmb_avg_bc_amps.bt15e_drms.col

 
! Dates: 03/03/16 - Written by D. Boore, based on ratios_from_rv_td_loop_drvr.for

      real dum(60) 
      real per_f2, per_f1   
      real m_f2, m_f1   
      real rjb_f1, rjb_f2, h_f1, h_f2, rrup_f1, rrup_f2, fff_f1, fff_f2 
      real stress_f1, stress_f2
      integer numsrc_f1, numsrc_f2, which_program
      real sd_f2_am, sd_f2_gm, sd_f1  
      real durex_f1, trms_f1
      real k0_f1, k0_f2
      
      character f_f2*200, f_f1*200, f_out*200
      character cmnts2skip(50)*80, buf*200, buf4*4
      
      integer, parameter :: ncol2read_f1 = 16, ncol2read_f2 = 16



      integer status
      
      character date_ctl_correct*8, date_ctl_in*30

      character f_ctl*200, f_sum*200 
      logical f_exist, skip_pga, skip_pgv

      f_exist = .false.
      do while (.not. f_exist)
        f_ctl = ' '
        write(*, '(a\)') 
     :    ' Enter name of control file '//
     :    '(cr=ratios_of_loop_program_output.ctl;'//
     :    ' ctl-brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl(1:4) .eq. '    ')
     :               f_ctl = 'ratios_of_loop_program_output.ctl'
        call trim_c(f_ctl, nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_exist)
        if (.not. f_exist) then
          write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        end if
      end do

      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl(1:nc_f_ctl),status='unknown')

      call skipcmnt(nu_ctl, cmnts2skip,nc_cmnts2skip)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)
      
      date_ctl_correct = ' '
      date_ctl_correct = '03/03/16'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      if (date_ctl_correct(1:nc_date_ctl_correct) .ne. 
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong date; update your '//
     :       'control file and rerun the program!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      f_sum = ' '
      read(nu_ctl,'(a)') f_sum
      call trim_c(f_sum,nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')

      write(nu_sum,'(a)') 
     :          ' Output of program ratios_from_rv_td_loop_drvr:'
      write(nu_sum,'(2a)') '   Control file = ', f_ctl(1:nc_f_ctl)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      f_out = ' '
      read(nu_ctl,'(a)') f_out
      call trim_c(f_out,nc_f_out)
      call get_lun(nu_out)
      open(unit=nu_out,file=f_out(1:nc_f_out),status='unknown')
      write(nu_sum,'(2a)') '   Output file = ', f_out(1:nc_f_out)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) per_epsilon
      write(nu_sum,'(a, 1x,es10.3)') 
     :          ' Relative accuracy of periods for equality = ', 
     :            per_epsilon
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) which_program      
! 1 = tmrsk_loop_rv_drvr
! 2 = tmrsk_loop_td_drvr
! 3 = fmrsk_loop_fas_drvr
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) nskip_f1, nskip_f2      
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) ncols2read_f1, ncol4gmim_f1,      
     :               ncols2read_f2, ncol4gmim_f2      

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)

      f_f1 = ' '
      read(nu_ctl,'(a)') f_f1
      call trim_c(f_f1,nc_f_f1)
      call get_lun(nu_f1)
      open(unit=nu_f1,file=f_f1(1:nc_f_f1),status='unknown')
      write(nu_sum,'(a)') '   Input file 1 = '//f_f1(1:nc_f_f1)
      write(nu_out,'(a)') '   Input file 1 = '//f_f1(1:nc_f_f1)

      f_f2 = ' '
      read(nu_ctl,'(a)') f_f2
      call trim_c(f_f2,nc_f_f2)
      call get_lun(nu_f2)
      open(unit=nu_f2,file=f_f2(1:nc_f_f2),status='unknown')
      write(nu_sum,'(a)') '   Input file 2 = '//f_f2(1:nc_f_f2)
      write(nu_out,'(a)') '   Input file 2 = '//f_f2(1:nc_f_f2)

      close(nu_ctl)
      
      if (which_program == 1 .or. which_program == 2) then
        write(nu_out,'(
     :     2x,a,
     :     10x,a, 9x,a,
     :     4x,a,
     :     2x,a, 2x,a,
     :     4x,a, 4x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     A     2x,a, 2x,a,
     :     1x,a, 1x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     :     9x,a, 9x,a, 
     :     6x,a, 6x,a,
     :     2x,a, 2x,a
     :                     )') 
     :    'damp',
     :    'per', 'freq',
     :    'm', 
     :    'Rjb_f1', 'Rjb_f2',
     :    'h_f1', 'h_f2',
     :    'Rrup_f1', 'Rrup_f2',
     :    'FFF_f1', 'FFF_f2',
     A    'Rps_f1', 'Rps_f2',
     :    'k0_f1', 'k0_f2',
     :    'numsrc_f1', 'numsrc_f2',
     :    'stress_f1', 'stress_f2',
     :    'y_f1', 'y_f2',  
     :    'f1/f2', 'f2/f1',
     :    '(f1/f2)^2', '(f2/f1)^2'
      else if (which_program == 3) then   ! only difference is damp, so for simplicity do not change this
        write(nu_out,'(
     :     2x,a,
     :     10x,a, 9x,a,
     :     4x,a,
     :     2x,a, 2x,a,
     :     4x,a, 4x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     A     2x,a, 2x,a,
     :     1x,a, 1x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     :     9x,a, 9x,a, 
     :     6x,a, 6x,a,
     :     2x,a, 2x,a
     :                     )') 
     :    'damp',
     :    'per', 'freq',
     :    'm', 
     :    'Rjb_f1', 'Rjb_f2',
     :    'h_f1', 'h_f2',
     :    'Rrup_f1', 'Rrup_f2',
     :    'FFF_f1', 'FFF_f2',
     A    'Rps_f1', 'Rps_f2',
     :    'k0_f1', 'k0_f2',
     :    'numsrc_f1', 'numsrc_f2',
     :    'stress_f1', 'stress_f2',
     :    'y_f1', 'y_f2',  
     :    'f1/f2', 'f2/f1',
     :    '(f1/f2)^2', '(f2/f1)^2'
      else
        write(*,'(a, 1x,i3)') 
     :     ' Incorrect value of which_program; QUITTING; '//
     :     'which_program = ', which_program
        STOP
      end if

      call skip(nu_f1, nskip_f1)
      call skip(nu_f2, nskip_f2)
      
      n_tmrs = 0
      
      loop over tmrsk values: DO

!output from tmrsk_loop_rv_drvr
!   1	        2	    3	   4	   5	6	7	 8	 9     10        11       12           13             14
!damp      T:tmrs      F:tmrs      M     Rjb    h    Rrup     F_FF     Rps     k0 numsource   S:tmrs gmim_dk80(g) gmim_dk80(cgs)

!output from tmrsk_loop_td_drvr
!   1	        2	    3	   4	   5	6	7	8	9     10        11       12           13             14
!damp      T:tmrs      F:tmrs      M     Rjb    h    Rrup    F_FF     Rps     k0 numsource   S:tmrs gmim_dk80(g) gmim_dk80(cgs)

!output from fmrsk_loop_fas_drvr
!      1	   2	  3	  4    5       6       7       8      9        10           11            12            13  
! f:fmrs      T:fmrs      M     Rjb    h    Rrup    F_FF     Rps     k0 numsource       S:fmrs fds(cgs):fmrs fas(cgs):fmrs


          dum = 0.0
          read(nu_f1,*,iostat=status) (dum(i), i=1,ncols2read_f1)
          if (status /= 0) then
            close(nu_f1)
            EXIT loop over tmrsk values
          end if
          if (which_program == 1 .or. which_program == 2) then
            damp_f1 = dum(1)
            per_f1 = dum(2)
            freq_f1 = dum(3)
            m_f1   = dum(4)
            rjb_f1    = dum(5)
            h_f1      = dum(6)
            rrup_f1   = dum(7)
            fff_f1    = dum(8)
            rps_f1    = dum(9)
            k0_f1     = dum(10)
            numsrc_f1 = int(dum(11))
            stress_f1 = dum(12)
            y_f1 = dum(ncol4gmim_f1)
          else                           ! fmrsk_loop_fas_drvr
            damp_f1 = 9.999
            freq_f1 = dum(1)
            per_f1 = dum(2)
            m_f1   = dum(3)
            rjb_f1    = dum(4)
            h_f1      = dum(5)
            rrup_f1   = dum(6)
            fff_f1    = dum(7)
            rps_f1    = dum(8)
            k0_f1     = dum(9)
            numsrc_f1 = int(dum(10))
            stress_f1 = dum(11)
            y_f1 = dum(ncol4gmim_f1)
          end if
          
          dum = 0.0
          read(nu_f2,*,iostat=status) (dum(i), i=1,ncols2read_f2)
          if (status /= 0) then
            close(nu_f2)
            EXIT loop over tmrsk values
          end if
          if (which_program == 1 .or. which_program == 2) then
            damp_f2 = dum(1)
            per_f2 = dum(2)
            freq_f2 = dum(3)
            m_f2   = dum(4)
            rjb_f2    = dum(5)
            h_f2      = dum(6)
            rrup_f2   = dum(7)
            fff_f2    = dum(8)
            rps_f2    = dum(9)
            k0_f2     = dum(10)
            numsrc_f2 = int(dum(11))
            stress_f2 = dum(12)
            y_f2 = dum(ncol4gmim_f2)
          else                           ! fmrsk_loop_fas_drvr
            damp_f2 = 9.999
            freq_f2 = dum(1)
            per_f2 = dum(2)
            m_f2   = dum(3)
            rjb_f2    = dum(4)
            h_f2      = dum(5)
            rrup_f2   = dum(6)
            fff_f2    = dum(7)
            rps_f2    = dum(8)
            k0_f2     = dum(9)
            numsrc_f2 = int(dum(10))
            stress_f2 = dum(11)
            y_f2 = dum(ncol4gmim_f2)
          end if
         
        n_tmrs = n_tmrs + 1
 
        if (per_f1 <= 0.0 .or. per_f2 <= 0.0) then
          if (per_f1 /= per_f2) then
            write(*,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' n_tmrs, per_f2, per_f1 = ', n_tmrs, per_f2, per_f1     
            EXIT loop over tmrsk values
          end if
        else
          if ( abs(per_f2 - per_f1)/per_f2 > per_epsilon) then
            write(*,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' n_tmrs, per_f2, per_f1 = ', n_tmrs, per_f2, per_f1     
            EXIT loop over tmrsk values
          end if
        end if

        if ( m_f2 /= m_f1 ) then
          print *, 
     :       ' ERROR:  m_f2, m_f1  not equal; QUIT'
          print *, 
     :       ' n_tmrs, m_f2, m_f1 = ', n_tmrs, m_f2, m_f1     
          EXIT loop over tmrsk values
        end if

        if ( abs(rjb_f2 - rjb_f1)/rjb_f2 > per_epsilon ) then  ! use per_epsilon for distance
          print *, 
     :       ' ERROR:  rjb_f2, rjb_f1  not epsilon; QUIT'
          print *, 
     :       ' n_tmrs, rjb_f2, rjb_f1 = ', n_tmrs, rjb_f2, rjb_f1     
            EXIT loop over tmrsk values
        end if

        if ( abs(rps_f2 - rps_f1)/rjb_f2 > per_epsilon ) then  ! use per_epsilon for distance
          print *, 
     :       ' ERROR:  rps_f2, rps_f1  not within epsilon; QUIT'
          print *, 
     :       ' n_tmrs, rps_f2, rps_f1 = ', n_tmrs, rps_f2, rps_f1     
            EXIT loop over tmrsk values
        end if

        if (y_f1 < 0) then
          f1_div_f2 = -999.0
          f1_div_f2_sq = -999.0
          f2_div_f1 = -999.0
          f2_div_f1_sq = -999.0
        else
          f1_div_f2 = y_f1/y_f2
          f1_div_f2_sq = (y_f1/y_f2)**2
          f2_div_f1 = y_f2/y_f1
          f2_div_f1_sq = (y_f2/y_f1)**2
        end if        
          
   
        write(nu_out,'(
     :    1x,f5.3,
     :    1x,es12.5, 1x,es12.5,
     :    1x,f4.2, 
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     A    1x,f7.2, 1x,f7.2,
     :    1x,f5.3, 1x,f5.3,
     :    8x,i2,   8x,i2,
     :    4x,f7.2, 4x,f7.2,
     :    1x,es12.5, 1x,es12.5,
     :    2x,f9.4, 2x,f9.4, 
     :    2x,f9.4, 2x,f9.4 
     :                             )') 
     :    damp_f1,
     :    per_f1, freq_f1,
     :    m_f1, 
     :    rjb_f1, rjb_f2,
     :    h_f1, h_f2,
     :    rrup_f1, rrup_f2,
     :    fff_f1, fff_f2,
     A    rps_f1, rps_f2,
     :    k0_f1, k0_f2,
     :    numsrc_f1, numsrc_f2,
     :    stress_f1, stress_f2,
     :    y_f1, y_f2, 
     :    f1_div_f2, f2_div_f1,
     :    f1_div_f2_sq, f2_div_f1_sq
         
      END DO loop over tmrsk values
     
     
      close(nu_out)
      close(nu_f1)
      close(nu_f2)
        
       
      close(nu_sum)
      
      END
      
      
      include '\forprogs\upstr.for'
      include '\forprogs\skipcmnt.for'
      include '\forprogs\trim_c.for'
      include '\forprogs\get_lun.for'
      include '\forprogs\skip.for'
      

      
      
      
      