* ----------------- BEGIN gm_rv_drvr ---------------------------
      Program gm_rv_drvr

*  Obtains input parameters and calls random vibration simulation


* Notes regarding modification of driver:
*  1. All "include" statements assume all subprograms are in the same folder
*     as this program.
*  2. If values of stress other than those in the input parameter file are
*     to be used, set "stressc" equal to the new value, rather than "stress"
*     (and also set the other parameters if the stress changes with moment).

* Dates: 06/01/95 - Written by D.M. Boore; Modified from TD_DRVR
*        06/09/95 - Bells and whistles still being added
*        08/11/95 - Add frequency column to output
*        08/11/95 - Changed places where parameter-file and column-file names
*                   are requested
*        08/18/95 - Added call to Write_Params
*        10/17/95 - Added a flag to Get_Params and Write_Params to tell if using
*                   time domain or random vibration procedure
*        11/14/95 - Combined smsim.fi and rv.fi
*        11/16/95 - Minor changes to i/o; moved calculation of fup to get_params
*        12/06/95 - Added "patience" message on screen.
*        12/08/95 - Added switch for 91 standard periods.
*        12/14/95 - Print out frequency and duration of excitation
*        12/14/95 - Repeated computation of fup in this driver.
*        12/17/95 - Changed location of asking for col file and reordered
*                   some output
*        12/31/95 - Modified some of the *.sum output
*        01/03/96 - Added pk_rms_cl_eq68 to *.sum output
*        01/05/96 - Added column file stem name to column headings
*        02/06/96 - Minor formatting improvements
*        04/12/96 - changed names psv, psa to psv, psa
*        10/21/96 - added an override option for fup calculated in the driver
*        02/28/97 - added output of kappa (because I now allow it to be
*                   magnitude dependent)
*        04/20/97 - increased dimension of per from 91 to 400
*        01/14/99 - Added calculation of Arias intensity
*        01/22/99 - Modified output, including Sd, pga, pgv in column file
*        01/22/99 - changed names psv, psa back to psv, psa
*        02/09/99 - Added computation of displacement 
*        02/10/99 - Include option to print pga, pgd, pgd and similarly
*                   pga/w, pga/w*w, etc in a column file to provide peak
*                   motion levels in plots of response spectra.  The file
*                   has so many columns that I decided to write the
*                   information to a separate file rather than include it
*                   in the response spectra file.  
*        03/05/99 - Added include statements at end to bring in all needed
*                   subroutines, rather than including them in the
*                   cl_rv.bat batch file.  Also, I removed the include calls to 
*                   rvtdsubs.for and recipes.for from smsim_rv.for.
*                   Write smsim version number
*        03/10/99 - Change from 'a\' to 'a' in format statements.  This is
*                   less pleasing aesthetically, but it allows
*                   for redirection of input/output, which did not
*                   work before (a test program showed that redirection
*                   works as long as the total of the sum of the characters
*                   in the query strings written to the screen do not exceed 
*                   some undetermined number). Also, change "call banner(5)" 
*                   to "call banner(6)" and remove the "cr to quit" in the 
*                   "dist (cr to quit)" query
*        03/12/99 - Added check for nper = 1 in loop over computing log-spaced
*                   periods (this is "fool-proofing"; if only one period is
*                   desired the user should reply "y" to the individual
*                   periods query and the computations with nper-1 in the 
*                   denominator should never be reached).
*        05/23/99 - Add an extra decimal to per output and add psa/pga (shape)
*                   to output
*        02/05/00 - Used trim_c for f_sum, f_col and corrected error in 
*                   making column heads for f_sum stem less than 4 characters
*        06/13/02 - Renamed "rv_drvr" to "gmrvdrvr" and "smsim_rv" to "gm_rv"
*                   to indicate that various ground motion measures are being
*                   computed using random vibration theory.
*        02/09/03 - Only print stress variable if numsource = 1 or 2
*        07/16/05 - Suggest the name of output files
*        07/27/05 - Improve obtaining input (among other things, define "rply"
*                   with more characters, in case the user enters a space
*                   before the requested reply).
*        08/16/05 - Renamed from gmrvdrvr to gm_rv_drvr
*        05/11/07 - Removed "\smsim\" from include statements
*        08/19/08 - Change the construction of the suggested col file,
*                   because the stem name of the params file can contain periods
*                   before the last period.
*        02/26/09 - Updated calls to get date and time.
*        12/01/09 - Redo computation of fup, depending on the values of fmax and kappa
*        12/02/09 - Because the same code for computing fup appears in a number of drivers, I've
*                   consolidated it into a subroutine.
*        12/04/09 - Write calculated values of dursource and durpath to summary file.
*        12/20/09 - Use coefficients relating log10_H_eff to M to modify the distance.
*        12/22/09 - Add m, r, h_eff, rmod, numsource, stress to output
*        02/12/10 - As a result of feedback from John Douglas, I replaced "cr" and "psv" 
*                   in the screen prompts with "Enter" and "response spectra", respectively.
!        08/25/10 - Shorten "patience" screen message.
!        09/30/10 - Use e format when writing psa/pga to avoid discretization effects in plots
!        01/15/11 - Add durex to output
!        01/17/11 - Add trms to output
!        01/21/11 - Add Der Kiureghain/Asfura estimates to summary file, as well
!                   as estimates using trms when calculation Nz and Ne.  This required
!                   changes to rv_subs and to smsim.fi
!        01/29/11 - Add amom0,1,2,4 to summary file
!        01/31/11 - Change nout to nu_sum, f_out to f_sum
!        02/06/11 - Explicitly set idva = 0 for an oscillator, even though this is also
!                   done in gm_rv in rv_subs (so this change is not needed--the output
!                   will be the same as before).  I am doing it here as well for clarity in
!                   reading the code, which has become quite convoluted.
!        04/05/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        05/23/11 - Print out avib (what Liu and Pezeshk call "k"; the value should be 1/3 for the
!                   BJ84 oscillator correction) to the summary file.
!        07/02/11 - Incorporate the Boore-Thompson modifications to the BJ84 oscillator correction
!                   for computing Trms.  The type of correction is specified in the params file, as follows:
!                   osc_crrctn(0=no correction;1=bj84;2=lp99; 3=bt12 WNA; 4=bt12 ENA; 5=bt12 avg. ena and wna)
!                 - Use generic names for pars files (ena250_bt12.pars and wna100_bt12.pars), so can 
!                   change them without revising and recompiling the program.
!                 - Read in both ena and wna oscillator corrections.   The decision of whether to
!                   use one or the other or both is now made in rv_subs (as specified 
!                   by osc_crrctn).
!        07/05/11 - Write smsim parameter file name in the first line of the output file.
!        08/02/11 - Changed "bt11" to "bt12", in anticipation of the paper being published in 2012.
!        08/08/11 - Change input order of BT12 coefficients
!        09/17/11 - The Trms4Osc coefficients are now read within get_params
!        12/02/11 - Add computation of am0 right after specification of amag (previously, it
!                   was contained in spect_scale, but it was not clear from where that routine was
!                   called). 
!                 - I also corrected an error, in which I called rmod_calc, with amag as an
!                   argument, before specifying amag.  I corrected this by changing the order
!                   in which I ask for amag and r (amag first)
!        01/07/14 - Longer file names
!        10/13/14 - Add warning about using CL68 rather than DK80 R2PF
!                 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc
!        11/14/15 - Revised to use the simulations made using the DK80 rms-to-peak factors.
!                   The previous version, which used simulations made with the CL68 rms-to-peak
!                   factors, has been renamed gm_rv_drvr_cl

      character f_params*200, f_sum*200, buf*200, rply*10, f_pavd*200
      character f_suggest*120
      character colsd_head*12, colpsv_head*12, colpsa_head*12
      character col_shape*16
      character psvcalc*10, indvidper*10, f_col*200, message*80, 
     :  standard_periods*10, write_pavd*10
      character date*8, time_start*10, time_stop*10
      logical tdflag, fparam_exist
      real t_pgd(2), t_pgv(2), t_pga(2)

      real*8 ane_d, xi_d, w

      REAL PER(400)
      DATA PER/0.040,0.042,0.044,0.046,0.048,
     * 0.050,0.055,0.060,0.065,0.070,
     * 0.075,0.080,0.085,0.090,0.095,0.10,0.11,0.12,0.13,0.14,
     * 0.15,0.16,0.17,0.18,0.19,0.20,0.22,0.24,0.26,0.28,
     * 0.30,0.32,0.34,0.36,0.38,0.40,0.42,0.44,0.46,0.48,
     * 0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,
     * 1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,
     * 2.0,2.2,2.4,2.6,2.8,3.0,3.2,3.4,3.6,3.8,
     * 4.0,4.2,4.4,4.6,4.8,5.0,5.5,6.0,6.5,7.0,
     * 7.5,8.0,8.5,9.0,9.5,10.0,11.0,12.0,13.0,14.0,
     * 15.0,309*0.001/

      include 'bt12osc.fi'

      include 'smsim.fi'

      pi = 4.0 * atan(1.0)
      twopi = 2.0 * pi

      call banner(6)

      write(*, '(a)') 
     :    ' Enter a message, if desired: '
      message = ' '
      read(*, '(a)') message
    
333   continue

      write(*,*)
      write(*,'(a)') 
     :    ' ****** NOTE ******'
      write(*,'(a)') 
     :    ' This program uses the Vanmarcke/Der Kiureghian'
      write(*,'(a)') 
     :    ' rms-to-peak factor.'
      write(*,'(a)') 
     :    ' ****** NOTE ******'
      write(*,*)
      


      f_params = ' '
      write(*, '(a)') 
     :  ' Enter name of file with parameters (press "Enter" to quit): '
      read(*, '(a)') f_params
      if (f_params(1:4) == '    ') stop
      call trim_c(f_params,nc_f_params)
      inquire(file=f_params(1:nc_f_params), exist=fparam_exist)
      if (.not. fparam_exist) then
        write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        go to 333
      end if
      do i = nc_f_params, 1, -1
        if (f_params(i:i) == '.') then
           indexp = i
           go to 991
        end if
        indexp = 1
      end do
991   continue  

      f_suggest = ' '
      f_suggest = f_params(1:indexp)//'gmrv.sum'
      call trim_c(f_suggest,nc_f_suggest)
      
      f_sum = ' '
      write(*, '(a)') 
     :    ' Enter name of summary file ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      read(*, '(a)') f_sum
      if (f_sum(1:4) == '    ') then
        f_sum = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_sum,nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')

      call banner(nu_sum)

      write(nu_sum, '(a)') message
      write(nu_sum, '(2a)') ' output file: ',
     :   f_sum(1:nc_f_sum)
      
      write(nu_sum, '(a)') 
     :  ' *** Results computed using GM_RV_DRVR ***'

* Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_start )
* Date is returned as 'CCYYMMDD' (character date*8) 
* Time is returned as 'hhmmss.sss' (character time*10)
*     character datx*8, time_start*10 
      write(nu_sum, *)
      write(nu_sum, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nu_sum, '(a)') 
     : ' Time Start: '//
     : time_start(1:2)//':'//time_start(3:4)//':'//time_start(5:10)
      write(nu_sum, *)
     
      write(nu_sum, '(2a)') ' file with parameters: ',
     :   f_params(1:20)

      write(*, '(a)') ' Compute response spectra? (y/n): '
      psvcalc = ' '
      read(*, '(a)') psvcalc
      call trim_c(psvcalc,nc_psvcalc)
      call upstr(psvcalc)
      if (psvcalc(1:1) == 'Y') then
        write(*, '(a)') 
     :  ' Enter individual periods? (y/n): '
        indvidper = ' '
        read(*, '(a)') indvidper
        call trim_c(indvidper, nc_indvidper)
        call upstr(indvidper)
        if (indvidper(1:1) == 'Y') then
          write(*, '(a)') 
     :       ' Enter fractional damping, nperiods: '
          read(*, *) damp, nper
          write(*, '(a)') '    Enter oscillator periods '//
     :        '(space or comma separated, single or multiple lines): '           
          read(*,*) (per(i), i = 1, nper)
        else
          write(*, '(a)') ' Use standard set of 91 periods? (y/n): '
          standard_periods = ' '
          read(*, '(a)') standard_periods
          call trim_c(standard_periods, nc_standard_periods)
          call upstr(standard_periods)
          nper = 91
          if (standard_periods(1:1) == 'Y') then
            nper = 91
            write(*, '(a)') ' Enter fractional damping: '
            read(*, *) damp
          else
            write(*, '(a)') 
     :       ' Enter fractional damping, perlow, perhigh, nper: '
            read(*,*) damp, perlow, perhigh, nper
            do i = 1, nper
              if (nper == 1) then
                per(i) = perlow
              else
                per(i) = 
     :            perlow * (perhigh/perlow)**(float(i-1)/float(nper-1))
              end if
            end do
          end if
        end if  
      else
        nper = 0
      end if

      tdflag = .false.  ! controls extent of input file read and written
      call get_params( f_params, tdflag )
      call write_params( nu_sum, tdflag)

! Compute fup.  Because the same code appears in a number of drivers, I've
! consolidated it into a subroutine.

      call get_fup(fm, akappa, amp_cutoff, nu_sum, fup)

100   continue

      write(*, '(a)') ' amag: '
      read(*, *) amag
      am0 = 10.**(1.5*amag + 16.05)

      write(*, '(a)') ' dist: '
      read(*, *) r

      rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
      new_mr = .true.      

      if (psvcalc(1:1) == 'Y') then
        f_suggest = ' '
        f_suggest = f_params(1:indexp-1)//'.mxxxxr00000_rs.rv.col'
        call trim_c(f_suggest,nc_f_suggest)
        write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
        if (rmod .lt. 10.0) then
          write(f_suggest(indexp+9:indexp+11),'(f3.1)') rmod
        else if (rmod .lt. 100.0) then
          write(f_suggest(indexp+8:indexp+11),'(f4.1)') rmod
        else if (rmod .lt. 1000.0) then
          write(f_suggest(indexp+7:indexp+11),'(f5.1)') rmod
        else if (rmod .lt. 10000.0) then
          write(f_suggest(indexp+8:indexp+11),'(i4)') int(rmod)
        else
          write(f_suggest(indexp+7:indexp+11),'(i5)') int(rmod)
        end if
        call trim_c(f_suggest,nc_f_suggest)
      
        f_col = ' '
        write(*, '(a)') 
     :    ' Enter name of column file to contain'//
     :    ' response spectra ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
        read(*, '(a)') f_col
        call trim_c(f_col,nc_f_col)
        if (f_col(1:4) == '    ') then
          f_col = f_suggest(1:nc_f_suggest)
        end if
        call trim_c(f_col,nc_f_col)
        call get_lun(ncol)
        open(unit=ncol, file=f_col(1:nc_f_col), status='unknown')
        write(ncol,'(a)') ' Parameter file: '//f_params(1:nc_f_params)
        nc_f_col_stem = min0(8, index(f_col(1:nc_f_col),'.') - 1)
        colsd_head  =     ' sd:'//f_col(1:nc_f_col_stem)//'________'
        colpsv_head =     'psv:'//f_col(1:nc_f_col_stem)//'________'
        colpsa_head =     'psa:'//f_col(1:nc_f_col_stem)//'________'
        col_shape   = 'psa/pga:'//f_col(1:nc_f_col_stem)//'________'
        write(ncol, '(
     :    2x,a, 6x,a, 5x,a, 
     :    6x,a, 7x,a, 4x,a, 4x,a,
     :    1x,a, 2x,a,
     :    1x,a, 1x,a, 1x,a, 1x,a,
     :    1x,a,
     :    2x,a, 4x,a, 6x,a, 7x,a
     :                            )')
     :   'damp', 'per', 'freq', 
     :   'M', 'R', 'F_FF', 'Rmod',
     :   'numsource', 'stress',
     :    colsd_head, colpsv_head, colpsa_head, col_shape,
     :   'domfrqpsv',
     :   'dursource', 'durpath', 'durex', 'trms' 
     
        write_pavd = ' '
        if (indvidper(1:1) /= 'Y') then
          write(*, '(a)') 
     :      ' Write pga, pgv, pgd to a column file? (y/n): '
          read(*, '(a)') write_pavd
          call trim_c(write_pavd,nc_write_pavd)
          call upstr(write_pavd)
          if (write_pavd(1:1) == 'Y') then
            f_suggest= ' '
            f_suggest= f_params(1:indexp-1)//'.mxxxxr00000_pavd.rv.col'
            write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
            if (rmod .lt. 10.0) then
              write(f_suggest(indexp+9:indexp+11),'(f3.1)') rmod
            else if (rmod .lt. 100.0) then
              write(f_suggest(indexp+8:indexp+11),'(f4.1)') rmod
            else if (rmod .lt. 10000.0) then
              write(f_suggest(indexp+8:indexp+11),'(i4)') int(rmod)
            else
              write(f_suggest(indexp+7:indexp+11),'(i5)') int(rmod)
            end if
            call trim_c(f_suggest,nc_f_suggest)
      
            write(*, '(a)') 
     :        ' Enter name of column file for to contain'//
     :        ' pga, pgv, pgd ("Enter" = '//
     :                       f_suggest(1:nc_f_suggest)//'):'
            f_pavd = ' '
            read( *, '(a)') f_pavd
            call trim_c(f_pavd, nc_f_pavd)
            if(f_pavd(1:4) == '    ') then
              f_pavd = f_suggest(1:nc_f_suggest)
            end if
            call trim_c(f_pavd,nc_f_pavd)
            
            call get_lun(npavd)
            open(unit=npavd, file=f_pavd(1:nc_f_pavd), status='unknown')
            tl = per(1)
            th = per(nper)
            tdiv = (th/tl)**(0.25)
            t_pga(1) = tl
            t_pga(2) = tl*tdiv
            t_pgv(1) = sqrt(th*tl/tdiv)
            t_pgv(2) = sqrt(th*tl*tdiv)
            t_pgd(1) = th/tdiv
            t_pgd(2) = th
            
            print *,' tl, th, tdiv, t_pgd(1,2)=',
     :                tl, th, tdiv, t_pgd(1),t_pgd(2)

            write(npavd, 
     :            '(t4,a,    t17,a,    t26,a,    t36,a, 
     :             t45,a,    t56,a,    t69,a,    t78,a, 
     :             t86,a,    t96,a,   t108,a,   t121,a)')
     :           't_pgd',    'pgd',  'w*pgd', 'w2*pgd',
     :           't_pgv',  'pgv/w',    'pgv',  'w*pgv',
     :           't_pga', 'pga/w2',  'pga/w',    'pga'
          end if
        end if
      end if  ! end loop to compute response spectrum

      call DATE_AND_TIME( date, time_start )

      iaorins = 1

      idva = 0
      write(*,'(a)') 
     : '  Computing peak displacement'
      call gm_rv(pgdsim)
      freq20pgd = freq20
      ane_pgd = ane
      anz_pgd = anz
      eps_pgd = eps_rv
      pk_rms_pgd = pk_rms_dk80_eq2
      amom0_pgd = amom0
      amom1_pgd = amom1
      amom2_pgd = amom2
      amom4_pgd = amom4
     
      idva = 1
      write(*,'(a)') 
     : '  Computing peak velocity'
      call gm_rv(pgvsim)
      freq20pgv = freq20
      ane_pgv = ane
      anz_pgv = anz
      eps_pgv = eps_rv
      pk_rms_pgv = pk_rms_dk80_eq2
      amom0_pgv = amom0
      amom1_pgv = amom1
      amom2_pgv = amom2
      amom4_pgv = amom4
     
      idva = 2
      write(*,'(a)') 
     : '  Computing peak acceleration'
      call gm_rv(pgasim)
      freq20pga = freq20
      ane_pga = ane
      anz_pga = anz
      eps_pga = eps_rv
      pk_rms_pga = pk_rms_dk80_eq2
      amom0_pga = amom0
      amom1_pga = amom1
      amom2_pga = amom2
      amom4_pga = amom4
      arias = arias_rv   ! must rename, because the variable arias_rv
                         ! is assigned a value for oscillator runs, but in 
                         ! those cases it is not the true Arias intensity
                         ! (it is the zeroth moment of the oscillator response)

      write(nu_sum,*)
      write(nu_sum, '(a)') ' *********** NEW R AND M ********** '
      write(nu_sum, '(a, 1p5(1x,e10.3))')
     :      ' r, f_ff, rmod, amag, kappa = ', 
     :        r, f_ff, rmod, amag, kappa_f(amag)
      if (psvcalc(1:1) == 'Y') then
        write(nu_sum, '(2x,2a)') 'File with response spectra: ', 
     :                         f_col(1:30)
      else
        write(nu_sum, '(2x,a)') 'No response spectra computed'
      end if

      write(nu_sum, '(2x,a,1x,1pe10.3)') ' const= ', const

      if (numsource == 1 .or. numsource == 2) then
        write(nu_sum, '(2x,a, f6.3, 
     :      1p, 1x,e9.2, 2(1x,e10.3), 3(1x,e9.2))') 
     :    ' amag, stress, fa, fb, dursource, durpath, durex= ', 
     :      amag, stress, fa, fb, dursource_calc, durpath_calc, durex
      else
        write(nu_sum, '(2x,a, f6.3, 
     :      1p, 2(1x,e10.3), 3(1x,e9.2))') 
     :    ' amag, fa, fb, dursource, durpath, durex= ', 
     :      amag, fa, fb, dursource_calc, durpath_calc, durex
      end if
      write(nu_sum, '(2x,a,1p2(1x,e10.3))') ' am0, am0b_m0fa= ', 
     :                                       am0, am0b_m0
      write(nu_sum,'( 30x,a, 
     :                 2x,a, 4x,a,
     :                 6x,a, 6x,a, 1x,a,
     :                 8x,a, 8x,a, 8x,a, 8x,a 
     :                                                     )')
     :             'pga(cm/s2)',
     :             'domfreq', 'eps', 
     :             'nx', 'nz', 'pk_rms',
     :             'amom0', 'amom1', 'amom2', 'amom4'
      write(nu_sum,
     :      '( 30x,es10.2, 
     :          2x,f7.2, 1x,f6.4, 
     :          1x,f7.2, 1x,f7.2, 1x,f6.2,
     :        4(1x,es12.5) )')
     :      pgasim, 
     :      freq20pga, eps_pga, 
     :      ane_pga, anz_pga, pk_rms_pga,
     :      amom0_pga, amom1_pga, amom2_pga, amom4_pga

      write(nu_sum,'( 31x,a, 
     :                 2x,a, 4x,a,
     :                 6x,a, 6x,a, 1x,a,
     :                 8x,a, 8x,a, 8x,a, 8x,a 
     :                                                     )')
     :             'pgv(cm/s)', 
     :             'domfreq', 'eps', 
     :             'nx', 'nz', 'pk_rms',
     :             'amom0', 'amom1', 'amom2', 'amom4'
      write(nu_sum,
     :      '( 30x,es10.2, 
     :          2x,f7.2, 1x,f6.4, 
     :          1x,f7.2, 1x,f7.2, 1x,f6.2,
     :        4(1x,es12.5) )')
     :      pgvsim, 
     :      freq20pgv, eps_pgv, 
     :      ane_pgv, anz_pgv, pk_rms_pgv,
     :      amom0_pgv, amom1_pgv, amom2_pgv, amom4_pgv

      write(nu_sum,'( 33x,a, 
     :                 2x,a, 4x,a,
     :                 6x,a, 6x,a, 1x,a,
     :                 8x,a, 8x,a, 8x,a, 8x,a 
     :                                                     )')
     :             'pgd(cm)', 
     :             'domfreq', 'eps', 
     :             'nx', 'nz', 'pk_rms',
     :             'amom0', 'amom1', 'amom2', 'amom4'
      write(nu_sum,
     :      '( 30x,es10.2, 
     :          2x,f7.2, 1x,f6.4, 
     :          1x,f7.2, 1x,f7.2, 1x,f6.2,
     :        4(1x,es12.5) )')
     :      pgdsim, 
     :      freq20pgd, eps_pgd, 
     :      ane_pgd, anz_pgd, pk_rms_pgd,
     :      amom0_pgd, amom1_pgd, amom2_pgd, amom4_pgd

      write(nu_sum, '(t19,a)') 'Arias intensity (cm/s)'
      write(nu_sum, '(1p, t32, e9.2)') arias


      if (psvcalc(1:1) == 'Y') then
        write(nu_sum,'(2x,a,f6.3)') 
     :        ' Fractional oscillator damping = ', damp
        write(nu_sum,'( 2x,a, 4x,a, 
     :                5x,a, 2x,a, 1x,a, 
     :               1x,a, 
     :               4x,a, 5x,a, 5x,a, 1x,a, 1x,a,
     :               5x,a, 1x,a,
     :               1x,a, 
     :               1x,a, 1x,a,
     :               1x,a, 1x,a, 1x,a,
     :               1x,a, 1x,a,
     :               1x,a, 1x,a,
     :               1x,a, 1x,a,
     :               2x,a, 1x,a,
     :               1x,a, 1x,a,
     :               4x,a, 1x,a,
     :               1x,a, 1x,a,
     :               2x,a, 1x,a,
     :               8x,a, 8x,a, 8x,a, 8x,a,
     :               7x,a,
     :               2x,a, 4x,a, 6x,a, 7x,a,
     :               4x,a,
     :               1x,a, 1x,a
     :                                              )')
     :             'per(s)', 'freq', 
     :             'sd(cm)', 'psv(cm/s)', 'psa(cm/s2)',
     :             'domfreq', 
     :             'eps', 'nx', 'nz', 'pk_rms', 'zupusd',
     :      'deltay',         'deltay_prime', 
     :      'anz_prime', 
     :      'ane_psv',        'ane_dur4n_trms',
     :      'anz_psv',        'anz_prime', 'anz_dur4n_trms',
     :      'pk_rms_cl_eq68', 'pk_rms_cl_eq68_dur4n_trms', 
     :      'pk_rms_dk_2',    'pk_rms_dk_dur4n_trms', 
     :      'pk_cl_eq68/w',   'pk_cl_eq68_dur4n_trms/w', 
     :      'pk_dk_2/w',      'pk_dk_dur4n_trms/w', 
     :      'pk_cl_eq68',     'pk_cl_eq68_dur4n_trms', 
     :      'pk_dk_2',        'pk_dk_dur4n_trms', 
     :      'pk_cl_eq68*w',   'pk_cl_eq68_dur4n_trms*w', 
     :      'pk_dk_2*w',      'pk_dk_dur4n_trms*w',
     :      'amom0', 'amom1', 'amom2', 'amom4',
     :      'avib',
     :      'dursource', 'durpath', 'durex', 'trms',
     :      'per/durex',
     :      'sqrt(durex/trms)', 'sqrt(trms/durex)'
     
        iaorins = 2
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! Assume that instrument response is relative to ground
! displacement.  In this case idva = 0, and to make sure
! that this is so, I include the following statement
! (this is also set in gm_rv in rv_subs; I am repeating here
! for clarity when reading this code later):

        idva = 0
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        do i = 1, nper
        
          write(*,'(a,i3,a,i3,a)') 
     :    '+ Computing response spectra number ',
     :     i, ' out of ', nper, ' periods.'

          perosc = per(i)
          call gm_rv(psvsim)
          freq20psv = freq20          
          ane_psv = ane
          anz_psv = anz
          eps_psv = eps_rv
          pk_rms_psv = pk_rms_dk80_eq2
          
          w = twopi/perosc          

          write(nu_sum,
     :      '( t2,f7.3, 1x,f7.3, 
     :         1x,es10.3, 1x,es10.3, 1x,es10.3, 
     :         1x,f7.2,      
     :         1x,f6.4, 1x,f6.1, 1x,f6.1, 1x,f6.2, 2x,f5.2,
     :         1x,es10.3, 3x,es10.3,
     :         4x,f6.1, 
     :         2x,f6.1, 9x,f6.1,
     :         2x,f6.1, 4x,f6.1, 9x,f6.1,
     :         9x,f6.2, 20x,f6.2,
     :         6x,f6.2, 15x,f6.2,
     :         3x,es10.3, 14x,es10.3,
     :         1x,es10.3, 9x,es10.3,
     :         1x,es10.3, 12x,es10.3,
     :         1x,es10.3, 7x,es10.3,
     :         3x,es10.3, 14x,es10.3,
     :         1x,es10.3, 9x,es10.3,
     :         1x,es12.5, 1x,es12.5, 1x,es12.5, 1x,es12.5,
     :         1x,es10.3,
     :         4(1x,es10.3),
     :         1x,es12.5,
     :         7x,es10.3, 7x,es10.3
     :                                                        )')
     :      perosc, 1.0/perosc, 
     :      psvsim/w, psvsim, w*psvsim, 
     :      freq20psv,
     :      eps_psv, ane_psv, anz_psv, pk_rms_psv, zup_used,
     :      deltay, deltay_prime, 
     :      anz_prime, 
     :      ane_psv, ane_dur4n_trms,
     :      anz_psv, anz_prime, anz_dur4n_trms,
     :      pk_rms_cl_eq68, pk_rms_cl_eq68_dur4n_trms, 
     :      pk_rms_dk_2,   pk_rms_dk_2_dur4n_trms, 
     :      pk_cl_eq68/w, pk_cl_eq68_dur4n_trms/w, 
     :      pk_dk_2/w, pk_dk_2_dur4n_trms/w, 
     :      pk_cl_eq68, pk_cl_eq68_dur4n_trms, 
     :      pk_dk_2, pk_dk_2_dur4n_trms, 
     :      pk_cl_eq68*w, pk_cl_eq68_dur4n_trms*w, 
     :      pk_dk_2*w, pk_dk_2_dur4n_trms*w,
     :      amom0, amom1, amom2, amom4,
     :      avib,
     :      dursource_calc, durpath_calc, durex, trms,
     :      perosc/durex,
     :      sqrt(durex/trms), sqrt(trms/durex)
 
          if (numsource == 1 .or. numsource == 2 .or. 
     :        numsource == 11) then
            stress_out = stress
          else
            stress_out = 0.0
          end if
           
          write(ncol,'(
     :      1x,f5.3, 1x,f8.4, 1x,f8.3,
     :      1x,f6.3, 1x,f7.2, 1x,f7.2, 1x,f7.2, 
     :      8x,i2, 1x,f7.2, 
     :      3x,es10.3, 3x,es10.3, 3x,es10.3, 
     :      7x,es10.3, 
     :      1x,f9.4,
     :      4(1x,es10.3)
     :                                 )')
     :      damp, perosc, 1.0/perosc, 
     :      amag, r, f_ff, rmod,
     :      numsource, stress_out,
     :      psvsim/(twopi/perosc), psvsim, (twopi/perosc)*psvsim, 
     :      (twopi/perosc)*psvsim/pgasim,
     :      freq20psv,
     :      dursource_calc, durpath_calc, durex, trms
     
        end do
        close(unit=ncol)
        if (write_pavd(1:1) == 'Y') then
          do i = 1, 2
            write(npavd, '(3(1x,f7.3,3(1x,es10.3)))')
     :       t_pgd(i),pgdsim,pgdsim*(twopi/t_pgd(i)),
     :                pgdsim*(twopi/t_pgd(i))**2,
     :       t_pgv(i),pgvsim/(twopi/t_pgv(i)),pgvsim,
     :                pgvsim*(twopi/t_pgv(i)),
     :       t_pga(i),pgasim/(twopi/t_pga(i))**2,
     :                pgasim/(twopi/t_pga(i)),pgasim
          end do
          close(unit=npavd)
        end if
      end if
     
      write(nu_sum, *)
      call DATE_AND_TIME( date, time_stop )
      call time_diff(time_start, time_stop, time_elapsed)
      write(nu_sum, '(a,1x,1pe10.3)') 
     :     ' Elapsed time (sec): ', time_elapsed

      write(*, '(a)') 
     : ' Compute results for another r and M? (y/n;"Enter"=quit): '
      read(*, '(a)') buf
      if (buf(1:4) == '    ') go to 999
      if (buf(1:1) == 'n' .or. buf(1:1) == 'N') go to 999

      goto 100

999   continue      


      close(unit=nu_sum)
      stop
      end
* ----------------- END gm_rv_drvr ---------------------------

!      include 'rv_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'smsim_subs_for_rv_programs.for'
      include 'smsim_util_subs.for'
      