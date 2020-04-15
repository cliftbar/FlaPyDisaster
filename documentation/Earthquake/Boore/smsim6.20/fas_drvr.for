! ----------------- BEGIN FAS_DRVR ---------------------------
      Program FAS_Drvr

! Obtains input parameters, calls FAS routine, and writes output.
! Write out Fourier displacement, velocity, and acceleration spectra,
! and Fourier spectra of the oscillator response, if used (max of 10 periods).

! Dates: 12/16/95 - Written by D.M. Boore; Modified from RV_DRVR
!        01/03/96 - Added oscillator response
!        04/12/96 - Changed name psv to prv
!        11/10/96 - Added ln FAS to output
!        02/28/97 - added output of kappa (because I now allow it to be
!                   magnitude dependent)
!        02/11/99 - Minor changes (use get_lun, upstr) to be consistent
!                   with changes made in other programs of the SMSIM suite.
!                   Also changed prv back to psv
!        02/17/99 - Added stem of file name to column headings
!        03/05/99 - Added include statements at the end and write smsim version
!        03/12/99 - Change from 'a\' to 'a' in format statements.  This is
!                   less pleasing aesthetically, but it allows
!                   for redirection of input/output, which did not
!                   work before (a test program showed that redirection
!                   works as long as the total of the sum of the characters
!                   in the query strings written to the screen do not exceed 
!                   some undetermined number). Also, change "call banner(5)" 
!                   to "call banner(6)" and remove the "cr to quit" in the 
!                   "dist (cr to quit)" query
!        03/12/99 - Added check for nper = 1 in loop over computing log-spaced
!                   periods (this is "fool-proofing"; if only one period is
!                   desired the user should reply "y" to the individual
!                   periods query and the computations with nper-1 in the 
!                   denominator should never be reached).
!        04/23/99 - Took care of problem in printing psv results in the 
!                   column file.
!        02/05/00 - Used trim_c for f_sum, f_col and corrected error in 
!                   making column heads for f_sum stem less than 4 characters
!        06/13/02 - smsimfas now included in rvtdsubs.for
!        02/09/03 - Only print stress variable if numsource = 1 or 2
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - Added general 2-corner source, requiring change in
!                   writing fa, fb, stress, etc.
!        04/12/08 - Suggest the name of output files
!        08/27/08 - Changed name to fas_rv_drvr from fas_drvr to be parallel with 
!                   fas_td_drvr.  Also improve suggested output file.
!        02/26/09 - Updated calls to get date and time.
!        12/20/09 - Use coefficients relating log10_H_eff to M to modify the distance.
!        02/14/10 - As a result of feedback from John Douglas, I replaced "cr" and "psv" 
!                   in the screen prompts with "Enter" and "response spectra", respectively.
!        04/06/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        06/12/11 - Add fas_rv_drvr to front of suggested summary file name; increase length
!                   of file names.
!        07/03/11 - Write the name of the params file to the first line of the output
!        08/04/11 - Increase length of file names.
!        09/16/11 - Changed name back to fas_drvr (see 08/27/08 change) because the program 
!                   makes no use of random vibration calculations.
!        10/21/11 - Allow for R to 4 digits in suggested output file name
!                   and in output.
!                 - Trap for values less than xmin
!        10/26/11 - Remove "rv" from suggested name of output file
!        07/03/13 - Corrected bug: amag was defined after the call to rmod_calc, which uses amag
!        10/13/14 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc
!        05/01/15 - Write displacement, velocity, and acceleration oscillator response to input acceleration
!                   nper <= 4 for this version (otherwise, increase size of bigbuf).
!                   I became quite confused when doing this as a result of comparing with FAS from
!                   a_ts_drvr, followed by smc2rs_ts, followed by smc2fs2 (see a plot in 
!                   C:\dujardin_m-dependent_attenuation\work_2015-05-01).  I finally realized that 
!                   SD, PSV, and PSA only differ by a factor involving the oscillator period, so that the 
!                   shape of the FAS for any of these when plotted vs frequency is the same; they are
!                   only scaled by 2*pi/Tosc.   So I have changed the output to show the FAS is the oscillator
!                   displacement (FAS_OSCD) and the first and second derivatives of the oscillator displacement
!                   (FAS_OSCV, FAS_OSCA),
!         09/03/15 - Ask for Rrup, write Rps in output file column header

      character f_suggest*200
      character psvcalc*1, indvidper*1, bigbuf*350
      character colfds_head*12, colfvs_head*12, colfas_head*12, per_c*7
      character f_params*300, f_sum*400, buf*80, buf_c*54
      character indvidfreq*1, f_col*400, message*80 
      character date*8, time_start*10, time_stop*10
      logical fparam_exist, tdflag
      
      integer, parameter :: npermax=4

      real freq(1000), fds(1000)
      real per(10), psv(1000, 10) 
      
      real :: fas_oscd, fas_oscv, fas_osca

      integer  irecord_length

      include 'smsim.fi'

      pi = 4.0 * atan(1.0)
      twopi = 2.0 * pi

      call banner(6)

      write(*, '(a)') 
     :    ' Enter a message, if desired: '
      message = ' '
      read(*, '(a)') message
    
333   continue
      f_params = ' '
      write(*, '(a)') 
     :    ' Enter name of file with parameters'//
     :    ' ("Enter" = fas_drvr.params;'//
     :    ' ctl-brk to quit): '
      read(*, '(a)') f_params
      call trim_c(f_params, nc_f_params)
      if (f_params(1:nc_f_params) .eq. ' ') 
     :       f_params = 'fas_drvr.params'
      call trim_c(f_params, nc_f_params)
      inquire(file=f_params(1:nc_f_params), exist=fparam_exist)
      if (.not. fparam_exist) then
        write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        go to 333
      end if
 
      do i = nc_f_params, 1, -1
        if (f_params(i:i) .eq. '.') then
           indexp = i
           go to 555
        end if
      end do
      indexp = -1
555   continue      

      f_suggest = ' '
      f_suggest = 'fas_drvr.'//f_params(1:indexp)//'sum'
      call trim_c(f_suggest,nc_f_suggest)
      
      f_sum = ' '
      write(*, '(a)') 
     :    ' Enter name of summary file ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      read(*, '(a)') f_sum
      if (f_sum(1:4) .eq. '    ') then
        f_sum = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_sum,nc_f_sum)
      call get_lun(nout)
      open(unit=nout,file=f_sum(1:nc_f_sum),status='unknown')

      call banner(nout)

      write(nout, '(a)') message
      write(nout, '(2a)') ' output file: ',
     :   f_sum(1:nc_f_sum)
      
      write(nout, '(a)') 
     :  ' *** Results computed using FAS_DRVR ***'

! Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_start )
! Date is returned as 'CCYYMMDD' (character date*8) 
! Time is returned as 'hhmmss.sss' (character time*10)
!     character datx*8, time_start*10 
      write(nout, *)
      write(nout, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nout, '(a)') 
     : ' Time Start: '//
     : time_start(1:2)//':'//time_start(3:4)//':'//time_start(5:10)
      write(nout, *)

      write(*, '(a)') 
     :  ' Enter individual frequencies? (y/n): '
      indvidfreq = ' '
      read(*, '(a)') indvidfreq
      call upstr(indvidfreq)
      if (indvidfreq .eq. 'Y') then
        write(*, '(a)') 
     :     ' Enter nfreqs: '
        read(*, *) nfreq
        do i = 1, nfreq
          write(*, '(a)') '    Enter frequency for FAS: ' 
          read(*,*) freq(i)
        end do
      else
        write(*, '(a)') 
     :       ' Enter freqlow, freqhigh, nfreq (freq .ne. 0!): '
        read(*,*) freqlow, freqhigh, nfreq
        if (freqlow .eq. 0.0 .or. freqhigh .eq. 0.0) then
          write(*, '(a)') 
     :     ' ******* FREQ = 0 NOT ALLOWED; ABORTING!! *******'
          go to 999
        end if
        do i = 1, nfreq
         freq(i) = 
     :    freqlow * (freqhigh/freqlow)**(float(i-1)/float(nfreq-1))
        end do
      end if  

      write(*, '(a)') ' Compute response spectrum? (y/n): '
      psvcalc = 'n'
      read(*, '(a)') psvcalc
      call upstr(psvcalc)
      if (psvcalc .eq. 'Y') then
        write(*, '(a)') 
     :  ' Enter individual periods? (y/n): '
        indvidper = ' '
        read(*, '(a)') indvidper
        call upstr(indvidper)
        if (indvidper .eq. 'Y') then
          write(*, '(a,1x,i2,a)') 
     :       ' Enter fractional damping, nperiods '//
     :       '(max of ', npermax, '): '
          read(*, *) damp, nper
          do i = 1, nper
            write(*, '(a)') '    Enter oscillator period: ' 
            read(*,*) per(i)
          end do
        else
          write(*, '(a,1x,i2,a)') 
     :     ' Enter fractional damping, perlow, perhigh,'//
     :     ' nperiods (max of ', npermax, '): '
          read(*,*) damp, perlow, perhigh, nper
        if (nper .gt. npermax) then
          write(*, '(a,i4,a, i4,a)') ' *** ERROR ****** nper = ',
     :       nper, ' > ', npermax, '; QUITTING!!!!' ! should check this when obtain
                                         ! nper and force the user to enter
                                         ! a valid number.  Perhaps this
                                         ! will be done in a future version.   
          close(unit=nout)
          stop
        end if
          do i = 1, nper
            if (nper .eq. 1) then
              per(i) = perlow
            else
              per(i) = 
     :            perlow * (perhigh/perlow)**(float(i-1)/float(nper-1))
            end if
          end do
        end if  
      else
        nper = 0
      end if

      tdflag = .false.
      call get_params( f_params, tdflag )
      call write_params( nout, tdflag)

100   continue

      write(*, '(a)') ' Rrup: '
      read(*, *) r

      write(*, '(a)') ' amag: '
      read(*, *) amag
      
      rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
        
! Get name of output file and open the file:
      f_suggest = ' '
      f_suggest = f_params(1:indexp-1)//'.mxxxxrps000000_fs.col'
      write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
      if (rmod .lt. 10.0) then
        write(f_suggest(indexp+12:indexp+14),'(f3.1)') rmod
      else if (rmod .lt. 100.0) then
        write(f_suggest(indexp+11:indexp+14),'(f4.1)') rmod
      else if (rmod .lt. 1000.0) then
        write(f_suggest(indexp+10:indexp+14),'(f5.1)') rmod
      else
        write(f_suggest(indexp+9:indexp+14),'(f6.1)') rmod
      end if
      call trim_c(f_suggest,nc_f_suggest)
      
      write(*, '(a)') 
     :    ' Enter name of column file for FAS results ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      f_col = ' '
      read(*, '(a)') f_col
      if (f_col(1:4) .eq. '    ') then
        f_col = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_col,nchar_col)
      call get_lun(ncol)
      open(unit=ncol, file=f_col(1:nchar_col), status='unknown')

      write(ncol,'(a)') ' Parameter file: '//f_params(1:nc_f_params) 
      
! Construct and write the column headers:
      nc_f_col_stem = min0(8, index(f_col(1:nchar_col),'.') - 1)
      colfds_head  =    'fds:'//f_col(1:nc_f_col_stem)//'________'
      colfvs_head =     'fvs:'//f_col(1:nc_f_col_stem)//'________'
      colfas_head =     'fas:'//f_col(1:nc_f_col_stem)//'________'

      bigbuf = ' '
      bigbuf(5:5) = 'M'
      bigbuf(10:13) = 'Rrup'
      bigbuf(17:20) = 'F_FF'
      bigbuf(26:28) = 'Rps'
      bigbuf(31:38) = 'Freq(Hz)'
      bigbuf(43:48) = 'Per(s)'
      bigbuf(50:61) = colfds_head
      bigbuf(63:74) = colfvs_head
      bigbuf(76:87) = colfas_head
      bigbuf(91:97) = 'ln_fas'
      if (nper .gt. 0) then
        iseg = 43
        do iper = 1, nper
          iseg = iseg + 54
          per_c = ' '
          write(per_c,'(f7.3)') per(iper)
          call trim_c(per_c,nchar_per)
          buf_c = ' '
          buf_c = 
     :        ' FAS_OSCD_T0000000'//
     :        ' FAS_OSCV_T0000000'//
     :        ' FAS_OSCA_T0000000'
          buf_c(19-nchar_per:18) = per_c(1:nchar_per) 
          buf_c(37-nchar_per:36) = per_c(1:nchar_per) 
          buf_c(55-nchar_per:54) = per_c(1:nchar_per) 
          write(bigbuf(iseg+1:iseg+54),'(a)') buf_c
        end do
      end if
    
      irecord_length = 97 + 54*nper
      write(ncol, '(a)') bigbuf(1:irecord_length) 
     
      call DATE_AND_TIME( date, time_start )

      iaorins = 1
      idva = 0
      write(*,'(a)') 
     : '  Patience! Computing displacement spectrum'
      call smsimfas(fds, freq, nfreq)

      if (psvcalc .eq. 'Y') then
        iaorins = 2
        idva = 0
        write(*,*)
        do iper = 1, nper
          write(*,'(a,i3,a,i3,a)') 
     :    '+ Patience! Computing response spectra number ',
     :     iper, ' out of ', nper, ' periods.'
          perosc = per(iper)
          fosc = 1.0/perosc
          call smsimfas(psv(1,iper), freq, nfreq)
        end do
      end if

      write(nout,*)
      write(nout, '(a)') ' *********** NEW R AND M ***********'
      write(nout, '(a, 1p5(1x,e10.3))')
     :      ' Rrup, f_ff, Rps, amag, kappa = ', 
     :        r, f_ff, rmod, amag, kappa_f(amag)
      write( nout, '(2x,a,1x,1pe10.3)') ' const= ', const
      if (numsource .eq. 1 .or. numsource .eq. 2 .or. 
     :    numsource .eq. 11) then
        write( nout, '(2x,a,1p4(1x,e10.3))') 
     :    ' amag, stress, fa, fb= ', amag, stress, fa, fb
      else
        write( nout, '(2x,a,1p4(1x,e10.3))') 
     :    ' amag, fa, fb= ', amag, fa, fb
      end if
      write( nout, '(2x,a,1p2(1x,e10.3))') ' am0, am0b_m0fa= ', 
     :                                       am0, am0b_m0

      write(*,*)

      xmin = 1.0e-30
      
      do i = 1, nfreq
        per4freq = 1.0e10
        if (freq(i) /= 0.0) per4freq = 1.0/freq(i)
        if (fds(i) < xmin) then
          fds4output = 0.0
        else
          fds4output = fds(i)
        end if
        fvs = (2.0*pi*freq(i)) * fds(i)
        if (fvs < xmin) then
          fvs  = 0.0
        end if
        fas = (2.0*pi*freq(i)) * fvs
        if (fas < xmin) then
          fas  = 0.0
          alogfas = -99.9
        else 
          alogfas = alog(fas)        
        end if
        bigbuf = ' '
        write(bigbuf(1:97), '(
     :      1x,f4.2, 1x,f7.2, 1x,f6.2, 1x,f7.2,
     :      2(es10.3),3(1x,es12.5),f9.4)')
     :      amag, r, f_ff, rmod,
     :      freq(i), per4freq, fds4output, fvs, fas, alogfas

        if (nper .gt. 0) then
          iseg = 43
          do iper = 1, nper
            iseg = iseg + 54
            if (psv(i,iper) < xmin) then
              fas_oscd = 0.0
              fas_oscv = 0.0
              fas_osca = 0.0
            else
              perosc = per(iper)
              fas_oscd = psv(i,iper)/(2*pi/perosc)
              radfreq  = 2*pi*freq(i)
              fas_oscv = radfreq*fas_oscd
              fas_osca = radfreq*fas_oscv
            end if            
            write(bigbuf(iseg+1:iseg+54),'(3(7x,es11.4))')
     :          fas_oscd, fas_oscv, fas_osca
          end do
        end if

        write(ncol, '(a)') bigbuf(1:irecord_length) 
      end do
      close(unit=ncol)
     
      write(nout, *)          
      call DATE_AND_TIME( date, time_stop )
      write(nout, *)
      write(nout, '(a)') 
     : ' Time Stop: '//
     : time_stop(1:2)//':'//time_stop(3:4)//':'//time_stop(5:10)
      call time_diff(time_start, time_stop, time_elapsed)
      write(nout, '(a,1x,1pe10.3)') 
     :     ' Elapsed time (sec): ', time_elapsed

      write(*,*)
      write(*, '(a)') 
     : ' Compute results for another r and M (y/n;"Enter"=quit)? '
      read(*, '(a)') buf
      if (buf(1:4) .eq. '    ') go to 999
      if (buf(1:1) .eq. 'n' .or. buf(1:1) .eq. 'N') go to 999

      goto 100

999   continue      

      close(unit=nout)
      stop
      end
! ----------------- END FAS_DRVR ---------------------------

      include 'rv_td_subs.for'
      include 'recipes.for'
      include 'smsim_util_subs.for'
