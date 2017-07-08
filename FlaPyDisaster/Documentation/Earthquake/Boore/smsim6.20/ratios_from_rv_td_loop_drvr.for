      Program ratios_from_rv_td_loop_drvr
      
!!Control file for program Ratios_from_RV_TD_Loop_Drvr
!! As many comment lines as desired, each starting with "!"
!! The string "pp:" indicates a new set of processing parameters
!! to be applied to the following smc files.  The parameters are given on the
!! following lines.
!! Revision of program involving a change in the control file on this date:
!   11/14/15
!!Name of summary file:
!  ratios_from_rv_td_loop_drvr.sum
!!Name of output file:
!  td_rv_ratios_wna.out
!!Relative accuracy of period to be equal (dPer/Per):
! 0.001
!!Skip pgv? (Y/N):
! N
!!Skip pga? (Y/N):
! N
!!Note: The above input is needed to make sure that the periods for consecutive lines read in
!! files 1 and 2 are the same.  The input is needed because if an output file was made using tmrs_loop_td_drvr, 
!! then output always contains pgv and pga.  The file output was made using tmrs_loop_rv_drvr, may start 
!! with spectral values (unless the control file specifically specifies that pgv and pga are to be computed), 
!! in which case no lines should be skipped.
!!
!!Lines to skip in files 1 and 2 to reach simulation values (1 before I
!!  added, on 15 June 2011, the SMSIM parameter file name to the first line in the 
!!  tmrs_loop output files; 2 after that date, but then I added parameters for Rjb2Rrup conversion,
!!  so the value should be 3).  Enter values for file 1, then file 2
! 3 3
!!List of smsim files,  The ratios will be both file1/file2 and file2/file1
!! NOTE: This version is not completely general, as it assumes that the
!! first file was made using the RV program (this assumption is needed
!! in order to extract the various durations).
!!
!tmrs_loop_rv_drvr.ena_scf_250b_hr_xdrms.many_m_r.col
!tmrs_loop_td_drvr.ena_scf_250b_hr_xdrms.many_m_r.col
 
! Dates: 03/05/11 - Written by D. Boore, based on rv_td_ratios.for
!        04/05/11 - More decimal places for period in output
!        04/12/11 - Change way of skipping pgv, pga entries
!        04/14/11 - Add relative accuracy of periods for equality
!        05/25/11 - Generalize control file to compute ratios of any two files made 
!                   by tmrs_loop_rv_drvr and/or tmrs_loop_td_drvr. Because I have decided
!                   that the time-domain arithmetic mean values are preferred to the
!                   geometric mean values, this version only uses the arithmetic means
!                   if the motions were computed using the time domain program.  This
!                   simplifies the coding, because the grounf-motion intensity
!                   values are in the same columns for RV and TD output.
!                 - NOTE: This version is not completely general, as it assumes that the
!                   first file was made using the RV program (this assumption is needed
!                   in order to extract the various durations).
!        06/15/11 - Specify lines to skip in files 1 and 2 to get to simulation
!        08/05/11 - Added ratios**2 (rv improvements are based on (rv_xosc/td)**2)
!                 - Add names of files to output.
!        07/11/14 - Revised to account for changes in output of tmrs_loop_td and rv_drvr
!        07/18/14 - Set ratios to -999 if the file 1 amplitude is less than 0.
!                 - Increase length of output field for ratios and for distance.
!                 - Format period and frequency as scientific format es12.5 in the output.
!        07/25/14 - Read and write Rmod, in addition to Rjb
!                 - Add spectral moments and deltay to output
!        07/25/14 - Add spectral moments to output, move deltay to last column, remove
!                   dk_y as the DK results are now included earlier.
!                 - Replace "Rmod" with "Rps" in the output ("ps" = "point source") to emphasize
!                   that Rps is the distance used in the point source calculations. The following text from
!                   a postscript in a 25 July 2014 email to Eric Thompson is an explanation of why this is desirable:
!                     "I need to keep in mind that the simulations in SMSIM are point-source simulations. 
!                      As such, one distance --- Rmod (which probably should be called "Rps",
!                      where "ps" stands for "point source")--- is used in the simulations to obtain Dp and 
!                      the FAS of the ground motion. The option to include a FFF is for the convenience of the user, 
!                      but I could equally well have not included it in the params file, forcing the user to make
!                      whatever mapping he/she desires between the distance used in the program and the physical distance
!                      for which the simulations are being computed.".
!                   For consistency I should also change all of the variable names containing "rmod" to "rps",
!                   but I am afraid of missing some changes, which could lead to headaches in debugging.  Perhaps
!                   I will make this change at a later date.
!        07/28/12 - Replace "trms" with "DurRMS" in output column headers
!        07/29/14 - Small changes, including not requiring absolute equality of distances (I noticed that
!                   Rjb from Eric's TD run was 20.00, whereas it was 19.99 from my RV run).
!        08/26/15 - Fixed error that CL68 was used for file 2, even if DK80 was specified (it was correctly used for file 1 if
!                   specified).
!                 - Fixed error in specification of ncol2read_f2 (it was set to 14, not 44)
!        08/28/15 - Add h, Rrup, F_FF, numsource, and stress to output
!        11/14/15 - A revised version that only reads RV the output made using the DK80 rms-to-peak factors;
!                   the previous version of the program has been renamed ratios_from_rv_td_loop_drvr_cl_and_dk

      real dum(60) 
      real per_f2, per_f1   
      real m_f2, m_f1   
      real rjb_f1, rjb_f2, h_f1, h_f2, rrup_f1, rrup_f2, fff_f1, fff_f2 
      real stress_f1, stress_f2
      integer numsrc_f1, numsrc_f2
      real sd_f2_am, sd_f2_gm, sd_f1  
      real durex_f1, trms_f1
      
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
     :    '(cr=Ratios_from_rv_td_Loop_Drvr.CTL;'//
     :    ' ctl-brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl(1:4) .eq. '    ')
     :               f_ctl = 'ratios_from_rv_td_loop_drvr.ctl'
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
      date_ctl_correct = '11/14/15'
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
      buf = ' '
      read(nu_ctl,'(a)') buf
      call trim_c(buf, nc_buf)
      call upstr(buf)
      if (buf(1:1) == 'Y') then
        skip_pgv = .true.
      else
        skip_pgv = .false.
      end if      
      write(nu_sum,'(a, 1x,l1)') ' skip_pgv = ', skip_pgv
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      buf = ' '
      read(nu_ctl,'(a)') buf
      call trim_c(buf, nc_buf)
      call upstr(buf)
      if (buf(1:1) == 'Y') then
        skip_pga = .true.
      else
        skip_pga = .false.
      end if      
      write(nu_sum,'(a, 1x,l1)') ' skip_pga = ', skip_pga      
       
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) nskip_f1, nskip_f2      
      

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
      
      write(nu_out,'(
     :     10x,a, 9x,a,
     :     4x,a,
     :     2x,a, 2x,a,
     :     4x,a, 4x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     :     2x,a, 2x,a,
     :     1x,a, 1x,a,
     :     2x,a, 2x,a,
     :     9x,a, 9x,a, 
     :     6x,a, 6x,a,
     :     2x,a, 2x,a,
     :     3x,a, 2x,a,
     :     2x,a,
     :     1x,a, 4x,a, 7x,a, 1x,a,
     :     7x,a, 7x,a, 7x,a, 7x,a, 4x,a
     :                     )') 
     :    'per', 'freq',
     :    'm', 
     :    'Rjb_f1', 'Rjb_f2',
     :    'h_f1', 'h_f2',
     :    'Rrup_f1', 'Rrup_f2',
     :    'FFF_f1', 'FFF_f2',
     :    'Rps_f1', 'Rps_f2',
     :    'numsrc_f1', 'numsrc_f2',
     :    'stress_f1', 'stress_f2',
     :    'y_f1', 'y_f2',  
     :    'f1/f2', 'f2/f1',
     :    '(f1/f2)^2', '(f2/f1)^2',
     :    'DurEx', 'DurRMS',
     :    'per/DurRMS',
     :    'DurRMS/DurEx', 'per/DurEx', 'DurOsc', 'DurOsc/DurEx',
     :    'amom0','amom1','amom2','amom4','dk-delta' 

      call skip(nu_f1, nskip_f1)
      call skip(nu_f2, nskip_f2)
      
      n_tmrs = 0
      
      loop over tmrs values: DO

! file 1 must be made by tmrs_loop_rv_drvr
        get value from file 1: DO    ! Use this as a way of skipping lines with PGA or PGV, if desired.

!1	2	3	4	5	6	7	8	9	10	        11	           12		13	14			15		16		17		18		19	     20	     21	            22		23		24	       25		26	     27	    28	          29	       30	       31	       32	      33	       34	   35		        36		      37		        38		      39		 40	41
!damp	T:tmrs	F:tmrs	M	Rjb	h	Rrup	F_FF	Rmod	numsource	S:tmrs	psa(cgs)_cl68	psa(g)_cl68	psv(cgs)_cl68	sd(cgs)_cl68	psa(cgs)_dk80	psa(g)_dk80	psv(cgs)_dk80	sd(cgs)_dk80	fa:tmrs	fb:tmrs	DurSource:tmrs	DurPath:tmrs	DurEx:tmrs	Trms:tmrs	Trms/DurEx	T/DurEx	DurOsc	DurOsc/DurEx	avib_used	avib_bj84	avib_lp99	dk-delta	y(cgs)_dk	y_rms	log(psa_cl68(cgs))	log(psa_cl68(g))	log(psa_dk80(cgs))	log(psa_dk80(g))	na_dk80_eq2	anz


          dum = 0.0
          read(nu_f1,*,iostat=status) (dum(i), i=1,ncol2read_f1)
          if (status /= 0) then
            close(nu_f1)
            EXIT loop over tmrs values
          end if
          per_f1 = dum(2)
          freq_f1 = dum(3)
          m_f1   = dum(4)
          rjb_f1    = dum(5)
          h_f1      = dum(6)
          rrup_f1   = dum(7)
          fff_f1    = dum(8)
          rps_f1    = dum(9)
          numsrc_f1 = int(dum(10))
          stress_f1 = dum(11)
          durex_f1   = dum(24)
          trms_f1   = dum(25)
          trms_div_durex   = dum(26)
          per_div_durex   = dum(27)
          durosc   = dum(28)
          durosc_div_durex   = dum(29)
          amom0 = dum(40)
          amom1 = dum(41)
          amom2 = dum(42)
          amom4 = dum(43)
          deltay = dum(44)
          
        
          if (per_f1 == -1.0) then
            if (skip_pgv) then
              CYCLE get value from file 1
            else
              y_f1 = dum(14)
              EXIT get value from file 1
            end if
          else if (per_f1 == 0.0) then
            if (skip_pga) then
              CYCLE get value from file 1
            else
              y_f1 = dum(12)
              EXIT get value from file 1
            end if
          else
            y_f1 = dum(12)
            EXIT get value from file 1
          end if
        END DO get value from file 1
  
        get value from file 2: DO
          dum = 0.0
          read(nu_f2,*,iostat=status) (dum(i), i=1,ncol2read_f2)
          if (status /= 0) then
            close(nu_f2)
            EXIT loop over tmrs values
          end if
          per_f2 = dum(2)
          freq_f2 = dum(3)
          m_f2   = dum(4)
          rjb_f2    = dum(5)
          h_f2      = dum(6)
          rrup_f2   = dum(7)
          fff_f2    = dum(8)
          rps_f2    = dum(9)
          numsrc_f2 = int(dum(10))
          stress_f2 = dum(11)
          trms_f2   = dum(25)
         
          if (per_f2 == -1.0) then
            if (skip_pgv) then
              CYCLE get value from file 2
            else
              y_f2 = dum(14)
              EXIT get value from file 2
            end if
          else if (per_f2 == 0.0) then
            if (skip_pga) then
              CYCLE get value from file 2
            else
              y_f2 = dum(12)
              EXIT get value from file 2
            end if
          else
              y_f2 = dum(12)
            EXIT get value from file 2
          end if
        END DO get value from file 2
  

         
        n_tmrs = n_tmrs + 1
 
        if (per_f1 <= 0.0 .or. per_f2 <= 0.0) then
          if (per_f1 /= per_f2) then
            write(*,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' n_tmrs, per_f2, per_f1 = ', n_tmrs, per_f2, per_f1     
            EXIT loop over tmrs values
          end if
        else
          if ( abs(per_f2 - per_f1)/per_f2 > per_epsilon) then
            write(*,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' ERROR:  per_f2, per_f1 not equal; QUIT'
            write(nu_sum,*) 
     :       ' n_tmrs, per_f2, per_f1 = ', n_tmrs, per_f2, per_f1     
            EXIT loop over tmrs values
          end if
        end if

        if ( m_f2 /= m_f1 ) then
          print *, 
     :       ' ERROR:  m_f2, m_f1  not equal; QUIT'
          print *, 
     :       ' n_tmrs, m_f2, m_f1 = ', n_tmrs, m_f2, m_f1     
          EXIT loop over tmrs values
        end if

        if ( abs(rjb_f2 - rjb_f1)/rjb_f2 > per_epsilon ) then  ! use per_epsilon for distance
          print *, 
     :       ' ERROR:  rjb_f2, rjb_f1  not epsilon; QUIT'
          print *, 
     :       ' n_tmrs, rjb_f2, rjb_f1 = ', n_tmrs, rjb_f2, rjb_f1     
            EXIT loop over tmrs values
        end if

        if ( abs(rps_f2 - rps_f1)/rjb_f2 > per_epsilon ) then  ! use per_epsilon for distance
          print *, 
     :       ' ERROR:  rps_f2, rps_f1  not within epsilon; QUIT'
          print *, 
     :       ' n_tmrs, rps_f2, rps_f1 = ', n_tmrs, rps_f2, rps_f1     
            EXIT loop over tmrs values
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
     :    1x,es12.5, 1x,es12.5,
     :    1x,f4.2, 
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    1x,f7.2, 1x,f7.2,
     :    8x,i2,   8x,i2,
     :    4x,f7.2, 4x,f7.2,
     :    1x,es12.5, 1x,es12.5,
     :    2x,f9.4, 2x,f9.4, 
     :    2x,f9.4, 2x,f9.4, 
     :    1x,f7.3, 1x,f7.3,
     :    1x,es11.4,
     :    1x,es12.5, 1x,es12.5, 1x,es12.5, 1x,es12.5,
     :    1x,es11.4, 1x,es11.4, 1x,es11.4, 1x,es11.4, 1x,es11.4
     :                             )') 
     :    per_f1, freq_f1,
     :    m_f1, 
     :    rjb_f1, rjb_f2,
     :    h_f1, h_f2,
     :    rrup_f1, rrup_f2,
     :    fff_f1, fff_f2,
     :    rps_f1, rps_f2,
     :    numsrc_f1, numsrc_f2,
     :    stress_f1, stress_f2,
     :    y_f1, y_f2, 
     :    f1_div_f2, f2_div_f1,
     :    f1_div_f2_sq, f2_div_f1_sq,
     :    durex_f1, trms_f1,
     :    per_f1/trms_f1,
     :    trms_div_durex, per_div_durex, durosc, durosc_div_durex,
     :    amom0, amom1, amom2, amom4, deltay
         
      END DO loop over tmrs values
     
     
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
      

      
      
      
      