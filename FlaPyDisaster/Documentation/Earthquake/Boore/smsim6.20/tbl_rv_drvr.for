! ----------------- BEGIN tbl_rv_drvr ---------------------------
      Program tbl_rv_drvr

!  Use SMSIM random vibration to make a table of motions.  Also will
!  include the BJF94 factors given two velocities.

! Notes regarding modification of driver:
!  1. All "include" statements assume all subprograms are in the same folder
!     as this program.
!  2. If values of stress other than those in the input parameter file are
!     to be used, set "stressc" equal to the new value, rather than "stress"
!     (and also set the other parameters if the stress changes with moment).

!  The control file contains up to 60 comment lines, each line preceeded 
!  by "!", with a line of input parameters following each group of 
!  comment lines.  Here is a sample of the control file:

!! Control file for program tbl_rv_drvr
!! Revision of program involving a change in the control file on this date:
!   08/24/10
!!
!!Name of Summary File:
! tbl_rv_drvr.sum
!!Comment:
! Test case for Open-File report
!!Name of File with Input Parameters:
! ofr4.params
!!Name of Tag to Add to Output File (up to 60 characters):
! tbl_
!!Enter V1 and V2 for BJF (a(V2)/A(V1) is computed; if pgv computed, no amps)
! 310.0 310.0
!!Enter mag_start, dmag, nmag
! 5.0 1.0 3
!!Enter rlow, rhigh, ndist (log spaced):
! 1.0 100.0 11
!!Units (0=cgs; 1=g):
! 1
!!Type of Response Spectrum (0=Sd; 1=PSV; 2=PSA):
! 2
!!Enter damp, nper 
! 0.05 6
!!per, h (<0 = pgv; 0.0 = pga)
! -1     4.4
!  0.0   3.2
!  0.1   3.4
!  0.2   4.6
!  1.0   4.0
!  3.0   5.0
 
 
! Dates: 07/24/96 - Written by D.M. Boore; Modified from DRVR4ART.  This
!                   initial version is Drvr4Art with the addition of the
!                   BJF soil amps.  Future versions should allow the
!                   specification of periods.
!                   Also, the BJF factors are only applied to PRV, not to 
!                   PGA or PGV
!        07/30/96 - Added V1, V2, Amp to output
!        02/13/99 - Minor changes (use get_lun, etc)
!        03/05/99 - Add include statements at end and write version #.
!        03/12/99 - Change from 'a\' to 'a' in format statements.  This is
!                   less pleasing aesthetically, but it allows
!                   for redirection of input/output, which did not
!                   work before (a test program showed that redirection
!                   works as long as the total of the sum of the characters
!                   in the query strings written to the screen do not exceed 
!                   some undetermined number). Also, change "call banner(5)" 
!                   to "call banner(6)".
!        06/09/00 - Changed directory for bjfv1v2f in the include statement
!                   from site_amp to smsim for use of smsim by other people
!                   (who have been instructed to dump all programs to a 
!                   directory named "smsim")
!        07/28/00 - Obtain input from control file.
!        07/29/00 - Express pga in cm/s/s rather than g, and compute psv 
!                   rather than psa
!        08/08/00 - Put option in control file for units and type of 
!                   response spectrum
!        10/09/00 - Bring in bjfv1v2f through include rvtdsubs.for
!        10/10/00 - Write name of tbl file into summary file
!        10/11/00 - Increased output format for period to 4 decimals
!        06/13/02 - Renamed "smsim_rv" to "gm_rv"
!        06/14/02 - Suggest name of control file.
!        02/05/03 - Changed units of g from 980 cm/s/s to 981 cm/s/s.
!        02/09/03 - Trim leading, trailing blanks from input file names,
!                   using trim_c 
!        07/15/05 - Write r, rs (not log) also
!        07/20/05 - Allow tag to be up to 20 characters
!        08/16/05 - Changed name from "tbldrvr" to "tbl_rv_drvr"
!        08/18/05 - Add pseudo-depth h to input (could be 0.0) and 
!                   extra r columns to output
!        05/11/07 - Use skipcmnt to read in parameters, allowing up to 60 comment
!                   lines.
!        05/11/07 - Removed "\smsim\" from include statements
!        02/26/09 - Updated calls to get date and time.
!        12/01/09 - Redo computation of fup, depending on the values of fmax and kappa
!        12/20/09 - Use coefficients relating log10_f_ff to M to modify the distance.
!        08/24/10 - Add version date to control file
!        03/02/12 - Calculate rmod by calling function rmod_calc
!        10/15/14 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc

      character  ctl_cmmnts(60)*100

      character date_ctl_correct*8, date_ctl_in*30
 

      character f_param*80, f_ctl*80, f_sum*80, f_tbl*80, 
     :  tag4fname*60, temp_c*8, buf_c*80
      character message*80
      character date*8, time_start*10, time_stop*10,
     :          time_begin*10, time_end*10

      real smsim_out(30), h

      real amag4print(30)
      real per

      logical tdflag, f_ctl_exist

      include 'smsim.fi'

      DO
        f_ctl = ' '
        write(*, '(a)') 
     :    ' Enter name of control file '//
     :    '(cr = tbl_rv_drvr.ctl; ctl-brk to quit): '
        read(*, '(a)') f_ctl
        IF (f_ctl(1:4) .eq. '    ') f_ctl = 'tbl_rv_drvr.ctl'
        call trim_c(f_ctl,nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_ctl_exist)
        IF (f_ctl_exist) EXIT
        write(*,'(a)') ' ******* CONTROL FILE '//f_ctl(1:nc_f_ctl)//
     :     ' DOES NOT EXIST, ENTER A VALID FILE NAME ******* '
      END DO
 
      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl,status='unknown')

!Check version of control file
      date_ctl_correct = ' '
      date_ctl_correct = '08/24/10'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)
      
      if (date_ctl_correct(1:nc_date_ctl_correct) /=
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong date; STOP!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_sum = ' '
      read(nu_ctl, '(a)') f_sum
      call trim_c(f_sum, nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')
      call banner(nu_sum)     
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      write(nu_sum,'(a)') f_sum(1:nc_f_sum)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      message = ' '
      read(nu_ctl, '(a)') message
      call trim_c(message, nc_message)
      write(nu_sum,'(1x,a)') message(1:nc_message)
    
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts) 
      f_param = ' '
      read(nu_ctl, '(a)') f_param
      call trim_c(f_param, nc_f_param)
      write(nu_sum,'(a)') f_param(1:nc_f_param)
    
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      tag4fname = ' '
      read(nu_ctl, '(a)') tag4fname
      call trim_c(tag4fname,nc_tag4fname)
      write(nu_sum,'(a)') tag4fname(1:nc_tag4fname)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) V1, V2
      write(nu_sum, '(2x,f7.2, 1x,f7.2)' ) V1, V2

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) amaglow, damag, nmag
      write(nu_sum, '(2x,f4.2, 1x,f4.2, 1x,i2)' ) 
     :             amaglow, damag, nmag
      do imag = 1, nmag
        amag4print(imag) = float(imag - 1)*damag + amaglow
      end do
      amaghigh = amag4print(nmag)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) rrlow, rrhigh, ndist
      write(nu_sum, '(2x,f6.2, 1x,f7.2, 1x,i3)' ) rrlow, rrhigh, ndist
      dlogr = (1.0/float(ndist-1))*alog10(rrhigh/rrlow)

      write(nu_sum, '(a)') 
     :  ' *** Results computed using tbl_rv_drvr ***'

! Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_begin )
! Date is returned as 'CCYYMMDD' (character date*8) 
! Time is returned as 'hhmmss.sss' (character time*10)
!     character datx*8, time_start*10 
      write(nu_sum, *)
      write(nu_sum, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nu_sum, '(a)') 
     : ' Time Start: '//
     : time_begin(1:2)//':'//time_begin(3:4)//':'//time_begin(5:10)
      write(nu_sum, *)
     
      write(nu_sum, '(2a)') ' file with parameters: ',
     :   f_param(1:20)

      tdflag = .false.
      call get_params( f_param, tdflag )

      call write_params( nu_sum, tdflag)

! Compute fup.  Because the same code appears in a number of drivers, I've
! consolidated it into a subroutine.

      call get_fup(fm, akappa, amp_cutoff, nu_sum, fup)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) i_units
      write(nu_sum, *) i_units

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) i_type_rs
      write(nu_sum, *) i_type_rs

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) damp, nper
      write(nu_sum, '(2x,f5.3, 1x,i3)' ) damp, nper

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)

      do i = 1, nper
        read(nu_ctl,*) perosc, h
        write(nu_sum, *) 

        if (perosc .gt. 0.0) freq_osc = twopi/perosc

        call Construct_Fname(tag4fname, perosc, f_tbl)
        call trim_c(f_tbl,nc_f_tbl)
        call get_lun(nu_tbl)
        open(unit=nu_tbl, file=f_tbl(1:nc_f_tbl), status='unknown')

        if (perosc .lt. 0.0) then    ! pgv
          write(nu_tbl, '(a)') ' log pgv(cm/s)' 
          iaorins = 1
          idva = 1
          AmpBJF = 1.0
          unit_fctr = 1.0
        else if (perosc .eq. 0.0) then  ! pga  
          if(i_units .eq. 0) then
            write(nu_tbl, '(a)') ' log pga(cm/s/s)' 
            unit_fctr = 1.0
          else
            write(nu_tbl, '(a)') ' log pga(g)' 
            unit_fctr = 1.0/981.0
          end if
          iaorins = 1
          idva = 2
          AmpBJF = bjfv1v2f(perosc, V1, V2)
        else                            ! response spectrum
          iaorins = 2
          idva = 0
          AmpBJF = bjfv1v2f(perosc, V1, V2)
          if (i_type_rs .eq. 0) then
            write(nu_tbl, '(a,f8.4,1x,f5.2)') 
     :            ' log Sd(cm), T, damp = ', perosc, damp 
            unit_fctr = 1.0
          else if (i_type_rs .eq. 1) then
            write(nu_tbl, '(a,f8.4,1x,f5.2)') 
     :            ' log psv(cm/s), T, damp = ', perosc, damp 
            unit_fctr = 1.0
          else if (i_type_rs .eq. 2) then
            if(i_units .eq. 0) then
              write(nu_tbl, '(a,f8.4,1x,f5.2)') 
     :            ' log psa(cm/s/s), T, damp = ', perosc, damp 
              unit_fctr = 1.0
            else
              write(nu_tbl, '(a,f8.4,1x,f5.2)') 
     :            ' log psa(g), T, damp = ', perosc, damp 
              unit_fctr = 1.0/981.0
            end if
          else
              write(*, '(a,i3,a)') 
     :             ' QUITTING!!! Illegal value of i_type_rs (=',
     :             i_type_rs, ')'
          end if
        end if             

        write(nu_sum,'(a,1x,a)') 
     :    ' Writing results into table ', f_tbl
        write(nu_sum, '(a, 1x,f7.3, 1x,f6.2, 1x,2f6.1, 1x,es10.3)')
     :    ' Perosc, h, V1, V2, AmpBJF: ',
     :      perosc, h, V1, V2, AmpBJF

        write(nu_tbl, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
        write(nu_tbl, '(a)') 
     : ' Time Start: '//
     : time_begin(1:2)//':'//time_begin(3:4)//':'//time_begin(5:10)
        write(nu_tbl, '(2a)') ' file with input parameters: ',
     :      f_param(1:20)
!        write(nu_tbl, '(3a, 2f6.1)') ' file with input parameters: ',
!     :     f_param(1:20), '   V1, V2 = ', V1, V2
        write(nu_tbl, '(a)') ' maglow, maghigh, dmag, nmag:'
        write(nu_tbl, '(2x,3(1x,f5.2),1x,i3)') 
     :     amaglow, amaghigh, damag, nmag
        write(nu_tbl, '(a)') ' rlow, rhigh, dlogr, ndist:'
        write(nu_tbl, '(2x,3(1x,f6.1),1x,i3)') 
     :     rrlow, rrhigh, dlogr, ndist
        write(nu_tbl, '(5x,a, 1x,a, 
     :                  4x,a,
     :                  7x,a, 2x,a, 
     :                  3x,a, 4x,a,
     :                  20(7x,f4.2,1x,a))') 
     :    'Rxh', 'logRxh', 
     :    'h', 
     :    'R', 'logR', 
     :    'f_ff', 'rmod',
     :    (amag4print(imag), 'logPSA', imag = 1, nmag)

        do idist = 1, ndist
          rxh = rrlow*10.0**(dlogr*float(idist-1))
          r = sqrt(rxh**2 + h**2)
          rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
         
        
          do imag = 1, nmag
            amag = amag4print(imag)
            if (perosc .lt. 0.0) then
              write(*,'(a,1x,f7.3,1x,f6.1,1x,f4.1,1x,f6.1,1x,f5.2)') 
     :        '+ Compute pgv: '//
     :             'per, r, f_ff, rmod, m = ', 
     :              perosc, r, f_ff, rmod, amag 
            else if (perosc .eq. 0.0) then
              write(*,'(a,1x,f7.3,1x,f6.1,1x,f4.1,1x,f6.1,1x,f5.2)') 
     :        '+ Compute pga: '//
     :             'per, r, f_ff, rmod, m = ', 
     :              perosc, r, f_ff, rmod, amag 
            else
              write(*,'(a,1x,f7.3,1x,f6.1,1x,f4.1,1x,f6.1,1x,f5.2)') 
     :        '+ Compute rs: '//
     :             'per, r, f_ff, rmod, m = ', 
     :              perosc, r, f_ff, rmod, amag 
            end if
            call gm_rv(smsim_out(imag))
            if (perosc .gt. 0.0) then  ! only do for response spectra
              if(i_type_rs .eq. 0) then  ! Sd
                smsim_out(imag) = smsim_out(imag) / freq_osc
              end if 
              if(i_type_rs .eq. 2) then  ! PSA
                smsim_out(imag) = smsim_out(imag) * freq_osc
              end if 
            end if
          end do                                     ! end loop over magnitude

          write(nu_tbl,'(1x,f7.2, 2x,f5.2, 
     :      1x,f4.1,
     :      1x,f7.2, 1x,f5.2, 
     :      1x,f7.2, 1x,f7.2, 
     :      40(1x,es10.3,1x,f6.3))') 
     :      rxh, alog10(rxh), 
     :      h, 
     :        r, alog10(r), 
     :        f_ff, rmod,
     :      (ampbjf * unit_fctr * smsim_out(imag), 
     :       alog10(ampbjf * unit_fctr * smsim_out(imag)), 
     :      imag=1,nmag)

        end do                                       ! end loop over distance

        close(unit=nu_tbl)


      end do                                         ! loop over period
     
999   continue      

      write(nu_sum, *)
      call DATE_AND_TIME( date, time_end )
      call time_diff(time_begin, time_end, time_elapsed)
      write(nu_sum, '(a,1x,1pe10.3)') 
     :     ' Total elapsed time (sec): ', time_elapsed



      close(unit=nu_sum)
      stop
      end
! ----------------- END tbl_rv_drvr ---------------------------

! ----------------- BEGIN Construct_Fname ------------------
      subroutine Construct_Fname(tag, period, fname)
      character tag*(*), fname*(*), per_c*4

      fname = ' '

      if (period .lt. 0.0) then
        per_c = '_pgv'
      else if (period .eq. 0.0) then
        per_c = '_pga'
      else 
        if (period .ge. 10.0) then
          per_c = ' '
          write(per_c,'(f4.1)') period                
        else
          per_c = ' '
          write(per_c,'(f4.2)') period                
        end if
        n_period = index(per_c,'.')
        per_c(n_period:n_period)='p'
      end if


      call trim_c(tag,nchar_tag)
      fname = tag(1:nchar_tag)//per_c//'.tbl'
      call trim_c(fname, nc_fname)
 
      return
      end
! ----------------- BEGIN Construct_Fname ------------------


!      include 'rv_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'smsim_subs_for_rv_programs.for'
      include 'smsim_util_subs.for'
