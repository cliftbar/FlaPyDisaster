! --------------- BEGIN fas_td_drvr ---------------------------------
      Program fas_td_drvr

! Computes time series and the average Fourier amplitude spectra of all time
! series (the FAS is computed in the subroutine smsim_tdfa)
! It writes out the target spectral amplitude as well
! as the FAS of the last time series out of nsims time series and the
! average FAS obtained by averaging the FAS of each time series
! and the squared FAS of each time series.

! Note for future modifications:  "fas_td_drvr.FOR" is exactly the same as 
! "GM_TD_DRVR.FOR" except the call to gm_td is replaced with a call to
! fs_td.
! See the comments in FS_TD for more information.

! Here are the two lines which differ from those in GMTDDRVR.FOR:
!     :    ' *** Results computed using fas_td_drvr ***'
!      call fs_td(per, nper,  

!  Obtains input parameters and calls time domain simulation

! Obtains input parameters and uses time-domain stochastic model to compute 
! a suite of accelerations, and from them velocities, displacements, and 
! response spectra, from which average peak motions are obtained.

! At most one set of acceleration, velocity, and displacement time series
! will be saved in this driver; see acc_drvr for a program that will save 
! more than one times series.

! Notes regarding modification of driver:
!  1. All "include" statements assume all subprograms are in the same folder
!     as this program.
!  2. The logical variable "new_mr" must be explicitly set (= .true. for a
!     new value of magnitude and distance); its value is passed through the
!     block common /misc/ (see SMSIM.FI).
!  3. If values of stress other than those in the input parameter file are
!     to be used, set "stressc" equal to the new value, rather than "stress"
!     (and also set the other parameters if the stress changes with moment).

! Dates: 06/01/95 - Written by D. M. Boore;
!                   Renamed and slightly modified PSV_DRVR
!        06/09/95 - Pass r, amag through common rather than parameter lists
!        06/12/95 - Added optional writing of acc, vel time series to a file
!        08/11/95 - Added frequency column to psv output
!        08/11/95 - Changed places where parameter-file and column-file names
!                   are requested
!        08/18/95 - Added call to Write_Params
!        10/17/95 - Added flag to get_params and write_params that tells 
!                   whether are dealing with time domain or rv.
!        11/14/95 - Modified output slightly
!        12/08/95 - Added switch to use 91 standard periods
!        12/14/95 - Print out frequency and duration of excitation
!        12/17/95 - Changed location of asking for col file name and
!                   reorder some output
!        12/28/95 - Used new_mr to correct bug related to use of loop over
!                   amag,r
!        12/31/95 - Modified some of the *.sum output
!        01/05/96 - Added column file stem name to column headings
!        01/22/96 - Added total duration, npts to output
!        02/06/96 - Minor formatting improvements
!        04/12/96 - changed names prv, paa to psv, psa; changed
!                   format of time in accvel file to fixed format.
!        02/28/97 - added output of kappa (because I now allow it to be
!                   magnitude dependent)
!        04/20/97 - changed dimension of per from 91 to 400
!        09/01/98 - added calculation of standard deviations
!        09/11/98 - changed array dimensions from 16400 to 33000
!        01/13/99 - added calculation of Arias intensity
!        01/23/99 - modified output to include Sd, and changed
!                   "prv" and "paa" to "psv" and "psa"
!        02/09/99 - Added computation of displacement 
!        02/10/99 - Include option to print pga, pgd, pgd and similarly
!                   pga/w, pga/w*w, etc in a column file to provide peak
!                   motion levels in plots of response spectra.  The file
!                   has so many columns that I decided to write the
!                   information to a separate file rather than include it
!                   in the response spectra file.  
!        02/13/99 - Changed "accvel" to "avd"
!        03/05/99 - Added include statements at end and write smsim version
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
!        02/05/00 - Used trim_c for f_out, f_col and corrected error in 
!                   making column heads for f_out stem less than 4 characters
!        06/22/00 - Print out standard error of the mean rather than std
!        01/28/01 - Increased array size from 33000 to 66000 (should use
!                   allocatable arrays)
!        02/12/01 - Use dynamically allocatable arrays; renamed "npts" to "npw2"
!        06/13/02 - New organization of subroutines requires initialization
!                   of seed and determination of time series indices in the 
!                   calling program, calling gm_td rather than smsim_td, and
!                   use of include statements to bring in necessary 
!                   subroutines at compile time.  
!                   Also, changed name from "td_drvr" to "fas_td_drvr" to indicate
!                   that ground motion is being computed using time domain 
!                   simulations.
!        06/14/02 - Modified from gmtddrvr.  Changed "tdfa" to "fstd" in 
!                   variable names.
!        02/09/03 - Only print stress variable if numsource = 1 or 2
!                   (or 11, 10 april 08)
!        08/16/05 - Changed name from fstddrvr to fas_td_drvr
!        05/09/07 - Delete "\smsim\" from include statements
!        04/10/08 - Added general 2-corner source, requiring change in
!                   writing fa, fb, stress, etc.
!        08/19/08 - Suggest the name of sum and rs output files
!        08/26/08 - Suggest a better name for the fs output file. This
!                   required adding f_params to the input parameter list
!                   of fs_td.for.
!        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                   I caught this only when I compiled the program using the -chk
!                   switch.  This probably means that all of my time-domain runs
!                   up till now used ntaper = 0, even if a nonzero taper was 
!                   specified (the standard being 0.05).
!        02/26/09 - Updated calls to get date and time.
!        12/04/09 - Change subroutine name Get_Npts to Get_Npts_for_TD_Calcs
!        12/23/09 - Use coefficients relating log10_H_eff to M to modify the distance.
!        02/14/10 - As a result of feedback from John Douglas, I replaced "cr" and "psv" 
!                   in the screen prompts with "Enter" and "response spectra", respectively.
!        07/03/11 - Write the name of the params file to the first line of the output
!        12/02/11 - Add computation of am0 right after specification of amag (previously, it
!                   was contained in spect_scale, but it was not clear from where that routine was
!                   called--it was in get_npts_for_td_calcs, called from the driver program). 
!                 - I also corrected an error, in which I called rmod_calc, with amag as an
!                   argument, before specifying amag.  I corrected this by changing the order
!                   in which I ask for amag and r (amag first)
!        05/29/13 - Add an extra decimal to the output period
!        02/21/14 - Add name of program to output
!                 - Increase size of some file names
!        10/13/14 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc

      character colpsv_head*12, colpsv_sem_head*16
      character colpsa_head*12, colpsag_head*15
      character colsd_head*11
      character f_params*200, f_out*2200, buf*80, f_pavd*80
      character f_suggest*250
      character f_avd*80, save_avd*1
      character psvcalc*1, indvidper*1, f_col*80, message*80,
     :  standard_periods*1, write_avd*1
      character date*8, time_start*10, time_stop*10
      logical tdflag, fparam_exist

! ALLOCATABLE: comment out following lines to disable dynamic allocation
      real acc_save(:), vel_save(:), dis_save(:)
      allocatable :: acc_save, vel_save, dis_save
! Remove comment character in line below to disable dynamic allocation
!      real acc_save(66000), vel_save(66000), dis_save(66000)
! ALLOCATABLE: end of group

      real psvsim(400), avgpsv(400), psvsim_std(400)
      real t_pgd(2), t_pgv(2), t_pga(2)
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
     * 15.0, 309*0.0/

      include 'smsim.fi'

! pi, twopi now computed in smsim.fi
!      pi = 4.0 * atan(1.0)
!      twopi = 2.0 * pi

      call banner(6)

      write(*, '(a)') 
     :    ' Enter a message, if desired: '
      message = ' '
      read(*, '(a)') message
    
333   continue
      f_params = ' '
      write(*, '(a)') 
     :    ' Enter name of file with parameters ("Enter" to quit): '
      read(*, '(a)') f_params
      call trim_c(f_params,nc_f_params)
      if (f_params(1:4) .eq. '    ') stop
      inquire(file=f_params(1:nc_f_params), exist=fparam_exist)
      if (.not. fparam_exist) then
        write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        go to 333
      end if
      do i = nc_f_params, 1, -1
        if (f_params(i:i) .eq. '.') then
           indexp = i
           go to 991
        end if
        indexp = 1
      end do
991   continue  

      f_suggest = ' '
      f_suggest = f_params(1:indexp)//'fas_td_drvr.sum'
      call trim_c(f_suggest,nc_f_suggest)
      
      f_out = ' '
      write(*, '(a)') 
     :    ' Enter name of summary file ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      read(*, '(a)') f_out
      if (f_out(1:4) .eq. '    ') then
        f_out = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_out,nc_f_out)
      call get_lun(nout)
      open(unit=nout,file=f_out(1:nc_f_out),status='unknown')

      call banner(nout)

      write(nout, '(a)') message
      write(nout, '(a)') 
     :   ' summary file: '//f_out(1:nc_f_out)

      write(nout, '(a)') 
     :    ' *** Results computed using fas_td_drvr ***'

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

      write(nout, '(a)') 
     :   ' file with parameters: '//f_params(1:nc_f_params)

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
          write(*, '(a)') 
     :       ' Enter fractional damping, nperiods: '
          read(*, *) damp, nper
          do i = 1, nper
            write(*, '(a)') '    Enter oscillator period: ' 
            read(*,*) per(i)
          end do
        else
          write(*, '(a)') ' Use standard set of 91 periods? (y/n): '
          standard_periods = ' '
          read(*, '(a)') standard_periods
          call upstr(standard_periods)
          nper = 91
          if (standard_periods .eq. 'Y') then
            nper = 91
            write(*, '(a)') ' Enter fractional damping: '
            read(*, *) damp
          else
            write(*, '(a)') 
     :       ' Enter fractional damping, perlow, perhigh, nper: '
            read(*,*) damp, perlow, perhigh, nper
            do i = 1, nper
              if (nper .eq. 1) then
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

      tdflag = .true.  ! controls extent of the input file read and written
      call get_params( f_params(1:nc_f_params), tdflag )
      call write_params(nout, tdflag)
      
100   continue

      write(*, '(a)') ' amag: '
      read(*, *) amag
      am0 = 10.**(1.5*amag + 16.05)

      new_mr = .true.

      write(*, '(a)') ' dist: '
      read(*, *) r

! replace h_eff with f_ff
        rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
 
      if (psvcalc .eq. 'Y') then
      
        f_suggest = ' '
        f_suggest = f_params(1:indexp)//'fas_td_drvr_rs.col'
        call trim_c(f_suggest,nc_f_suggest)      
        f_col = ' '
        write(*, '(a)') 
     :    ' Enter name of column file to contain response spectra:'//
     :    ' ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
        read(*, '(a)') f_col
        if (f_col(1:4) .eq. '    ') then
          f_col = f_suggest(1:nc_f_suggest)
        end if
        call trim_c(f_col,nc_f_col)
        call get_lun(ncol)
        open(unit=ncol, file=f_col(1:nc_f_col), status='unknown')
        do i = nc_f_col, 1, -1
          if (f_col(i:i) .eq. '.') then
             indexpcol = i
             go to 992
          end if
          indexpcol = 1
        end do
992     continue  

        write(ncol,'(a)') ' Program file: FAS_TD_DRVR' 
        write(ncol,'(a)') ' Parameter file: '//f_params(1:nc_f_params) 
      
        nc_f_col_stem = min0(8, indexpcol - 1)
        colsd_head  =     'sd:'//f_col(1:nc_f_col_stem)//'________'
        colpsv_head =     'psv:'//f_col(1:nc_f_col_stem)//'________'
        colpsv_sem_head ='sem/psv:'//f_col(1:nc_f_col_stem)//'________'
        colpsa_head =     'psa:'//f_col(1:nc_f_col_stem)//'________'
        colpsag_head =    'psa(g):'//f_col(1:nc_f_col_stem)//'________'

        write(ncol, '(
     :                 4x,a, 6x,a, 3x,a, 3x,a, 
     :                 6x,a, 5x,a, 
     :                 1x,a, 1x,a, 
     :                 1x,a, 1x,a,
     :                 1x,a)')
     :   'M', 'r', 'f_ff', 'rmod',
     :   'per', 'freq', 
     :    colsd_head, colpsv_head, 
     :    colpsa_head, colpsag_head,
     :    colpsv_sem_head
        write_avd = 'N'
        if (indvidper .ne. 'Y') then
          write(*, '(a)') 
     :        ' Write pga, pgv, pgd to a column file? (y/n): '
          read(*, '(a)') write_avd
          call upstr(write_avd)
          if (write_avd .eq. 'Y') then
            f_suggest = ' '
            f_suggest = 
     :          f_params(1:indexp)//'fas_td_drvr_pga_pgv_pgd.col'
            call trim_c(f_suggest,nc_f_suggest)      
            f_pavd = ' '
            write(*, '(a)') 
     :       ' Enter name of column file to contain pga, pgv, pgd:'//
     :       ' (cr = '//
     :      f_suggest(1:nc_f_suggest)//'):'
            read(*, '(a)') f_pavd
            if (f_pavd(1:4) .eq. '    ') then
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

            write(npavd, 
     :            '(t4,a,    t17,a,    t26,a,    t36,a, 
     :             t45,a,    t56,a,    t69,a,    t78,a, 
     :             t86,a,    t96,a,   t108,a,   t121,a)')
     :           't_pgd',    'pgd',  'w*pgd', 'w2*pgd',
     :           't_pgv',  'pgv/w',    'pgv',  'w*pgv',
     :           't_pga', 'pga/w2',  'pga/w',    'pga'
          end if
        end if
      end if

      write(*, '(a)') ' Save acc & vel time series? (y/n): '
      save_avd = 'n'
      read(*, '(a)') save_avd
      call upstr(save_avd)
      if (save_avd .eq. 'Y') then
        write(*, '(a,i4,a)') 
     : '    Save which simulation (out of ', nsims, ')? '
        read(*, *) nacc_save
        write(*, '(a)') 
     : '    Enter file name for time series '//
     :     '(cr=fas_td_drvr_avd_time_series.col): '
        f_avd = ' '
        read(*, '(a)') f_avd
        if (f_avd(1:3) .eq. '   ') then
           f_avd = 'fas_td_drvr_avd_time_series.col'
        end if
        call trim_c(f_avd,nc_f_avd)
      else
        nacc_save = 0
      end if
     
      call DATE_AND_TIME( date, time_start )

      call Get_Npts_for_TD_Calcs(nstart, nstop, npw2, te, ntaper)  

! ALLOCATABLE: comment out following lines to disable dynamic allocation
      ndimen = npw2
      allocate (acc_save(ndimen), vel_save(ndimen), dis_save(ndimen))
! Remove comment character in lines below to disable dynamic allocation
!      ndimen = 66000
!      if (npw2 .gt. ndimen) then
!        write(*,'(2x,a,i5,a,i5,a)') 
!     :       ' *** FATAL ERROR: NPW2 (',
!     :                                    npw2, 
!     :       ') > NDIMEN (', 
!     :                                     ndimen, 
!     :       '); QUITTING!'
!        write(*, '(2x,a)') ' TRY INCREASING DT'
!        stop
!      end if
! ALLOCATABLE: end of group

      call fs_td(per, nper,  
     :   psvsim, psvsim_std,
     :   pgasim, pgasim_std,
     :   pgvsim, pgvsim_std,
     :   pgdsim, pgdsim_std,
     :   arias, arias_std,
     :   psvcalc, nacc_save, acc_save, vel_save, dis_save,
     :   nstart, nstop, npw2, te, ntaper,
     :   f_params)

      if (save_avd .eq. 'Y') then
        call get_lun(nuacc)
        open(unit=nuacc,file=f_avd(1:nc_f_avd),status='unknown')
        write(nuacc, '(t12,a,t24,a,t36,a,t48,a)') 'T', 'A', 'V', 'D'
        do i = 1, npw2
          write(nuacc, '(1x,f11.6,1p3(1x,e11.4))') 
     :      float(i-1)*dt, acc_save(i), vel_save(i), dis_save(i)
        end do
      end if 
         
! ALLOCATABLE: comment out following line to disable dynamic allocation
      deallocate (acc_save, vel_save, dis_save)
! ALLOCATABLE: end of group

      write(nout, *)
      write(nout, '(a)') ' *********** NEW R AND M ***********'
      write(nout, '(a, 1p5(1x,e10.3))')
     :      ' r, f_ff, rmod, amag, kappa = ', 
     :        r, f_ff, rmod, amag, kappa_f(amag)
      write(nout, '(2x,2a)') ' Time Start: ', time_start
      if (psvcalc .eq. 'Y') then
        write(nout, '(2x,a)') 
     : ' Column file with RS: '//f_col(1:nc_f_col)
      end if
      write(nout, '(2x,a,1x,1pe10.3)') ' const= ', const

      if (numsource .eq. 1 .or. numsource .eq. 2
     :    .or. numsource .eq. 11) then
        write(nout, '(2x,a,f6.3,1pe9.2,2e10.3,e9.2)') 
     :    ' amag, stress, fa, fb, durex= ', 
     :      amag, stress, fa, fb, durex
      else
        write(nout, '(2x,a,f6.3,1p,2e10.3,e9.2)') 
     :  ' amag, fa, fb, durex= ', 
     :    amag, fa, fb, durex
      end if
      write( nout, '(2x,a,1p2(1x,e10.3))') ' am0, am0b_m0fa= ', 
     :                                       am0, am0b_m0

      write(nout, '(2x,a,i6, f8.5, f6.1)')
     :  ' npw2, dt, total duration = ', npw2, dt, npw2*dt
      write(nout,'(t5,a, t16,a, t25,a, t38,a, t46,a, t60,a)') 
     :    'pgd(cm)',    'sem/pgd', 
     :    'pgv(cm/s)',  'sem/pgv', 
     :    'pga(cm/s2)', 'sem/pga'
      write(nout,'(1p,6(1x,e10.2))') 
     :     pgdsim, (pgdsim_std/sqrt(float(nsims)))/pgdsim, 
     :     pgvsim, (pgvsim_std/sqrt(float(nsims)))/pgvsim, 
     :     pgasim, (pgasim_std/sqrt(float(nsims)))/pgasim
      write(nout,'( t2,a, t25,a)') 
     :    'Arias intensity(cm/s)', 'sem/Arias'
      write(nout,'(1p, 11x, 2(1x,e10.2))') 
     :     arias, (arias_std/sqrt(float(nsims)))/arias

      write(*,*)

      if (psvcalc .eq. 'Y') then
        write(nout,'(2x,a,f6.3)') 
     :        ' Fractional oscillator damping = ', damp
        write(nout,'( 
     :                4x,a, 6x,a, 3x,a, 3x,a, 
     :                5x,a, 5x,a, 
     :                2x,a, 4x,a, 
     :                1x,a, 5x,a,
     :                4x,a)')
     :             'M', 'r', 'f_ff', 'rmod',
     :             'per', 'freq', 
     :             'psv(cm/s)', 'sem/psv', 
     :             'psa(cm/s2)', 'psa(g)',
     :             'psd(cm)'
        do i = 1, nper
          write(nout,
     :      '( 
     :         1x,f4.2, 1x,f6.2, 1x,f6.2, 1x,f6.2,
     :         1x,f8.4, 1x,f8.3, 
     :         1p, 
     :         5(1x,e10.3)        
     :                                )')
     :      amag, r, f_ff, rmod,
     :      per(i), 1.0/per(i), psvsim(i), 
     :      (psvsim_std(i)/sqrt(float(nsims)))/psvsim(i),
     :      (twopi/per(i))*psvsim(i), 
     :      (twopi/per(i))*psvsim(i)/981.0, 
     :      psvsim(i)/(twopi/per(i))
          write(ncol,'(
     :                 1x,f4.2, 1x,f6.2, 1x,f6.2, 1x,f6.2,
     :                 1x,f7.3, 1x,f8.3,
     :                 1p,
     :                 2x,e10.3, 3x,e10.3,
     :                 3x,e10.3, 6x,e10.3,
     :                 7x,e10.3)')
     :      amag, r, f_ff, rmod,
     :      per(i), 1.0/per(i), 
     :      psvsim(i)/(twopi/per(i)), psvsim(i), 
     :      (twopi/per(i))*psvsim(i), (twopi/per(i))*psvsim(i)/981.0, 
     :      (psvsim_std(i)/sqrt(float(nsims)))/psvsim(i)
        end do
        close(unit=ncol)
        if (write_avd .eq. 'Y') then
          do i = 1, 2
            write(npavd, '(3(0p1x,f7.3,1p3(1x,e10.3)))')
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
! --------------- END fas_td_drvr ---------------------------------

      include 'fs_td.for'
!      include 'acc_ts.for'
      include 'td_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'smsim_util_subs.for'



