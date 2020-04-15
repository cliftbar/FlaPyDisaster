* ----------------- BEGIN TBL_FAS ---------------------------
      Program Tbl_FAS

*  Use SMSIMFAS to compute Fourier amplitude spectrum of motion, and 
*  print to a table.  Also will
*  include the BJF94 factors given two velocities.

* Notes regarding modification of driver:
*  1. All "include" statements assume all subprograms are in the same folder
*     as this program.
*  2. If values of stress other than those in the input parameter file are
*     to be used, set "stressc" equal to the new value, rather than "stress"
*     (and also set the other parameters if the stress changes with moment).

*  The control file contains up to 60 comment lines, each line preceeded 
*  by "!", with a line of input parameters following each group of 
*  comment lines.  The processing stops after "Stop" is encountered in a line
*  (the character string "stop" can be any combination of uppercase and
*  lowercase letters).  Here is a sample of the control file:

*!Control file for program tbl_fas
*!Name of Summary File:
* tbl_fas.sum
*!Comment:
* Test case for Open-File report
*!Name of File with Input Parameters:
* ofr4.params
*!Name of Tag to Add to Output File:
* tbl_fas_
*!Enter V1 and V2 for BJF (a(V2)/A(V1) is computed; if pgv computed, no amps)
* 310.0 310.0
*!Enter mag_start, dmag, nmag
* 5.0 1.0 3
*!Enter rlow, rhigh, ndist (log spaced):
* 1.0 100.0 11
*!Enter damp, nper 
* 0.05 6
*!per (<0 => FAS of ground, not oscillator response)
*  0.1
*  0.2
*  0.5
*  1.0
*  2.0
* 11.0


 
* Dates: 07/29/00 - Written by D.M. Boore; Modified from TBLDRVRR.
*        10/09/00 - Bring in bjfv1v2f through include rvtdsubs.for
*        06/13/02 - smsimfas now included in rvtdsubs.for
*        06/14/02 - Suggest name of control file.
*        02/09/03 - Make sure that leading and trailing blanks are removed
*                   from the input file names (because LF95 allows file names
*                   with blanks).
*        11/23/04 - Compute FAS of ground, rather than oscillator, if per < 0
*        11/23/04 - Modification so that a single M or R will not cause
*                   an error
*        08/16/05 - Remove restriction of file name being no more than four
*                   characters
*        05/11/07 - Use skipcmnt to read in parameters, allowing up to 60 comment
*                   lines.
*        05/11/07 - Removed "\smsim\" from include statements
*        02/26/09 - Updated calls to get date and time.
*        12/20/09 - Use coefficients relating log10_H_eff to M to modify the distance.
!        04/15/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        10/15/14 - Allow for f_ff being defined by two lines (in rmod_calc)
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc

      character  ctl_cmmnts(60)*100

      character f_param*80, f_ctl*80, f_sum*80, f_tbl*60, 
     :  tag4fname*20, temp_c*8, buf_c*80
      character message*80
      character date*8, time_start*10, time_stop*10,
     :          time_begin*10, time_end*10

      real freq(1), fas(1,30)

      real amag4print(30)
      real per

      logical tdflag, f_ctl_exist, ground_fas

      include 'smsim.fi'

      pi = 4.0 * atan(1.0)
      twopi = 2.0 * pi

333   continue
      f_ctl = ' '
      write(*, '(a)') 
     :    ' Enter name of control file '//
     :    '(cr = tbl_fas.ctl; ctl-brk to quit): '
      read(*, '(a)') f_ctl
      if (f_ctl(1:4) .eq. '    ') f_ctl = 'tbl_fas.ctl'
      inquire(file=f_ctl, exist=f_ctl_exist)
      if (.not. f_ctl_exist) then
        write(*,'(a)') ' ******* CONTROL FILE DOES NOT EXIST ******* '
        go to 333
      end if

      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl,status='unknown')

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_sum = ' '
      read(nu_ctl, '(a)') f_sum
      call trim_c(f_sum, nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')
      call banner(nu_sum)     
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      write(nu_sum,'(1x,a)') f_sum(1:nc_f_sum)

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
      write(nu_sum,'(1x,a)') f_param(1:nc_f_param)
    
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      tag4fname = ' '
      read(nu_ctl, '(a)') tag4fname
      call trim_c(tag4fname, nc_tag4fname)
      write(nu_sum,'(a)') tag4fname(1:nc_tag4fname)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) V1, V2
      write(nu_sum, '(2x,f7.2, 1x,f7.2)' ) V1, V2

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) amaglow, damag, nmag
      write(nu_sum, '(2x,f4.2, 1x,f4.2, 1x,i2)' ) 
     :              amaglow, damag, nmag    
      do imag = 1, nmag
        amag4print(imag) = float(imag - 1)*damag + amaglow
      end do
      amaghigh = amag4print(nmag)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) rrlow, rrhigh, ndist
      write(nu_sum, '(2x,f6.2, 1x,f7.2, 1x,i3)' ) 
     :                      rrlow, rrhigh, ndist
     
      if(ndist .gt. 1) then
        dlogr = (1.0/float(ndist-1))*alog10(rrhigh/rrlow)
      else
        dlogr = 0
      end if

      write(nu_sum, '(a)') 
     :  ' *** Results computed using TBL_FAS ***'

* Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_begin )
* Date is returned as 'CCYYMMDD' (character date*8) 
* Time is returned as 'hhmmss.sss' (character time*10)
*     character datx*8, time_start*10 
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

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)
      read(nu_ctl,*) damp, nper
      write(nu_sum, '(2x,f5.3, 1x,i3)' ) damp, nper

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      write(nu_sum,'(a)') ctl_cmmnts(1:nc_ctl_cmmnts)

      do i = 1, nper
        read(nu_ctl,*) perosc
        write(nu_sum, *) 
        if (perosc .gt. 0.0) then
           ground_fas = .false.
        else
           ground_fas = .true.
           perosc = -perosc
        end if
        

        call Construct_Fname(tag4fname, perosc, f_tbl)
        call get_lun(nu_tbl)
        call trim_c(f_tbl,nc_f_tbl)
        open(unit=nu_tbl, file=f_tbl(1:nc_f_tbl), status='unknown')

        if (perosc .le. 0.0) then    ! pgv or pga
          write(*, '(a,1x,1pe10.3,a)') ' Error, quitting, perosc (',
     :       perosc,') <= 0.0' 
          stop
        end if

        if (ground_fas) then    
          write(nu_tbl, '(a,f6.2,1x,f5.2)') 
     :             ' log fas(cm/s), T = ', perosc
        else
          write(nu_tbl, '(a,f6.2,1x,f5.2)') 
     :             ' log psv(g), T, damp = ', perosc, damp
        end if

        AmpBJF = bjfv1v2f(perosc, V1, V2)

        write(nu_sum, '(a, 1x, f7.3, 1x, 2f6.1, 1x,1pe10.3)')
     :    ' Perosc, V1, V2, AmpBJF: ',
     :      perosc, V1, V2, AmpBJF

        write(nu_tbl, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
        write(nu_tbl, '(a)') 
     : ' Time Start: '//
     : time_begin(1:2)//':'//time_begin(3:4)//':'//time_begin(5:10)
        write(nu_tbl, '(3a, 2f6.1)') ' file with input parameters: ',
     :     f_param(1:20), '   V1, V2 = ', V1, V2
        write(nu_tbl, '(a)') ' maglow, maghigh, dmag, nmag:'
        write(nu_tbl, '(2x,3(1x,f5.2),1x,i3)') 
     :     amaglow, amaghigh, damag, nmag
        write(nu_tbl, '(a)') ' rlow, rhigh, dlogr, ndist:'
        write(nu_tbl, '(2x,3(1x,f6.1),1x,i3)') 
     :     rrlow, rrhigh, dlogr, ndist
        write(nu_tbl, '(a)') ' f_ff:'
        write(nu_tbl, '(2x,f6.1)') 
     :     f_ff
        write(nu_tbl, '(t3,a,2x,f4.2,19(3x,f4.2))') 
     :    'logR\M:', (amag4print(imag), imag = 1, nmag)


        if (ground_fas) then  
          iaorins = 1
          idva = 2
        else
          iaorins = 2
          idva = 0
        end if
        
        freq(1) = 1.0/perosc
        nfreq = 1

        do idist = 1, ndist
          r = rrlow*10.0**(dlogr*float(idist-1))
!Replaced h_eff with f_ff
          rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
          do imag = 1, nmag
            amag = amag4print(imag)
              write(*,'(a,1x,f6.2,1x,f6.1,1x,f5.2)') 
     :        '+ Patience! Computing rs for '//
     :             'perosc, r, f_ff, rmod, m = ', 
     :              perosc, r, f_ff, rmod, amag 
            fosc = 1.0/perosc
            call smsimfas(fas(1,imag), freq, nfreq )
            write(*,'(1x,a,1x,i3,1x,1pe10.3)') 
     :        ' imag, fas(1,imag)=', imag, fas(1,imag)
          end do                                     ! end loop over magnitude
          write(nu_tbl,'(t3,f4.2,t9,20(1x,f6.3))') 
     :      alog10(r), 
     :      (alog10(ampbjf*fas(1,imag)), 
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
* ----------------- END TBL_FAS ---------------------------

* ----------------- BEGIN Construct_Fname ------------------
      subroutine Construct_Fname(tag, period, fname)
      character tag*(*), fname*(*), per_c*4


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
      end if


      call trim_c(tag,nc_tag)
      
      fname = ' '      
      fname = tag(1:nc_tag)//per_c(1:4)//'.tbl'
      call trim_c(fname,nc_fname)
 
      return
      end
* ----------------- BEGIN Construct_Fname ------------------
      

      include 'rv_td_subs.for'
      include 'recipes.for'
      include 'smsim_util_subs.for'
