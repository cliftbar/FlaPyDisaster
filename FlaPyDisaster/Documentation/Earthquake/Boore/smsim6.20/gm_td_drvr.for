* --------------- BEGIN gm_td_drvr ---------------------------------
      Program gm_td_drvr

* Obtains input parameters and uses time-domain stochastic model to compute 
* a suite of accelerations, and from them velocities, displacements, and 
* response spectra, from which average peak motions are obtained.

* At most one set of acceleration, velocity, and displacement time series
* will be saved in this driver; see a_ts_drvr for a program that will save 
* more than one times series.

* Notes regarding modification of driver:
*  1. All "include" statements assume all subprograms are in the same folder
*     as this program.
*  2. The logical variable "new_mr" must be explicitly set (= .true. for a
*     new value of magnitude and distance); its value is passed through the
*     block common /misc/ (see SMSIM.FI).
*  3. If values of stress other than those in the input parameter file are
*     to be used, set "stressc" equal to the new value, rather than "stress"
*     (and also set the other parameters if the stress changes with moment).

* Dates: 06/01/95 - Written by D. M. Boore;
*                   Renamed and slightly modified PSV_DRVR
*        06/09/95 - Pass r, amag through common rather than parameter lists
*        06/12/95 - Added optional writing of acc, vel time series to a file
*        08/11/95 - Added frequency column to psv output
*        08/11/95 - Changed places where parameter-file and column-file names
*                   are requested
*        08/18/95 - Added call to Write_Params
*        10/17/95 - Added flag to get_params and write_params that tells 
*                   whether are dealing with time domain or rv.
*        11/14/95 - Modified output slightly
*        12/08/95 - Added switch to use 91 standard periods
*        12/14/95 - Print out frequency and duration of excitation
*        12/17/95 - Changed location of asking for col file name and
*                   reorder some output
*        12/28/95 - Used new_mr to correct bug related to use of loop over
*                   amag,r
*        12/31/95 - Modified some of the *.sum output
*        01/05/96 - Added column file stem name to column headings
*        01/22/96 - Added total duration, npts to output
*        02/06/96 - Minor formatting improvements
*        04/12/96 - changed names prv, paa to psv, psa; changed
*                   format of time in accvel file to fixed format.
*        02/28/97 - added output of kappa (because I now allow it to be
*                   magnitude dependent)
*        04/20/97 - changed dimension of per from 91 to 400
*        09/01/98 - added calculation of standard deviations
*        09/11/98 - changed array dimensions from 16400 to 33000
*        01/13/99 - added calculation of Arias intensity
*        01/23/99 - modified output to include Sd, and changed
*                   "prv" and "paa" to "psv" and "psa"
*        02/09/99 - Added computation of displacement 
*        02/10/99 - Include option to print pga, pgd, pgd and similarly
*                   pga/w, pga/w*w, etc in a column file to provide peak
*                   motion levels in plots of response spectra.  The file
*                   has so many columns that I decided to write the
*                   information to a separate file rather than include it
*                   in the response spectra file.  
*        02/13/99 - Changed "accvel" to "avd"
*        03/05/99 - Added include statements at end and write smsim version
*        03/12/99 - Change from 'a\' to 'a' in format statements.  This is
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
*        02/05/00 - Used trim_c for f_out, f_col and corrected error in 
*                   making column heads for f_out stem less than 4 characters
*        06/22/00 - Print out standard error of the mean rather than std
*        01/28/01 - Increased array size from 33000 to 66000 (should use
*                   allocatable arrays)
*        02/12/01 - Use dynamically allocatable arrays; renamed "npts" to "npw2"
*        06/13/02 - New organization of subroutines requires initialization
*                   of seed and determination of time series indices in the 
*                   calling program, calling gm_td rather than smsim_td, and
*                   use of include statements to bring in necessary 
*                   subroutines at compile time.  
*                   Also, changed name from "td_drvr" to "gmtddrvr" to indicate
*                   that ground motion is being computed using time domain 
*                   simulations.
*        02/09/03 - Only print stress variable if numsource = 1 or 2
*        08/16/05 - Change name from "gmtddrvr" to "gm_td_drvr"
*        05/11/07 - Removed "\smsim\" from include statements
*        08/19/08 - Suggest name of output files
*        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
*                   I caught this only when I compiled the program using the -chk
*                   switch.  This probably means that all of my time-domain runs
*                   up till now used ntaper = 0, even if a nonzero taper was 
*                   specified (the standard being 0.05).
*        02/21/09 - Added geometric mean computation.  Let pga, pgd asymptotes
*                   be in terms of geometric mean.
*                 - Add 95%-5% duration
*        02/23/09 - Hardwire 75% rather than 95% in determining duration, as 
*                   recommended by Ou and Herrmann (this is also consistent with
*                   some Husid plots I made).
*        02/26/09 - Updated calls to get date and time.
*        07/04/09 - Correct small error in summary output if rs not computed.
*        12/04/09 - Write calculated values of dursource and durpath to summary file.
*        12/04/09 - Change subroutine name Get_Npts to Get_Npts_for_TD_Calcs
*        12/20/09 - Use coefficients relating log10_H_eff to M to modify the distance.
*        12/22/09 - Add m, r, h_eff, rmod, numsource, stress to output
*        02/12/10 - As a result of feedback from John Douglas, I replaced "cr" and "psv" 
*                   in the screen prompts with "Enter" and "response spectra", respectively.
!        06/01/10 - Suggest file name for avd output.
!        01/04/11 - Add "gm" to column headers in asymptotes file to make it clear
!                   that geometric means are being used (see 02/21/09 comment above).
!        01/17/11 - Compute 95%-5% duration
!        02/02/11 - Write SD for individual simulations to a file named 'SD_sims.out'
!                   (modification in gm_td.for)
!        04/15/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        07/05/11 - Write smsim parameter file name in the first line of the output file.
!        12/02/11 - Add computation of am0 right after specification of amag (previously, it
!                   was contained in spect_scale, but it was not clear from where that routine was
!                   called--it was in get_npts_for_td_calcs, called from the driver program. 
!                 - I also corrected an error, in which I called rmod_calc, with amag as an
!                   argument, before specifying amag.  I corrected this by changing the order
!                   in which I ask for amag and r (amag first)
!        01/07/14 - Longer file names
!        10/13/14 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc


      character colpsv_head*12, colpsv_sem_head*16, colpsa_head*12
      character colsd_head*12
      character colpsvgm_head*14, colpsvgm_sem_head*18,colpsagm_head*14
      character colsdgm_head*14
      character f_params*200, f_out*200, buf*200, f_pavd*200
      character f_suggest*200
      character f_avd*200, save_avd*10
      character psvcalc*10, indvidper*10, f_col*200, message*80,
     :  standard_periods*10, write_pavd*10
      character date*8, time_start*10, time_stop*10
      logical tdflag, fparam_exist

      real acc_save(:), vel_save(:), dis_save(:)
      allocatable :: acc_save, vel_save, dis_save

      real psvsim(400), psvsim_std(400)
      real psvsimgm(400), psvsimgm_std(400)
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

* pi, twopi now computed in smsim.fi
*      pi = 4.0 * atan(1.0)
*      twopi = 2.0 * pi

      call banner(6)

      write(*, '(a)') 
     :    ' Enter a message, if desired: '
      message = ' '
      read(*, '(a)') message
    
333   continue
      f_params = ' '
      write(*, '(a)') 
     : ' Enter name of file with parameters (press "Enter" to quit): '
      read(*, '(a)') f_params
      call trim_c(f_params, nc_f_params)
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
      f_suggest = f_params(1:indexp)//'gmtd.sum'
      call trim_c(f_suggest,nc_f_suggest)
      
      f_out = ' '
      write(*, '(a)') 
     :    ' Enter name of summary file ("Enter" = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      read(*, '(a)') f_out
      call trim_c(f_out, nc_f_out)
      if (f_out(1:4) .eq. '    ') then
        f_out = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_out,nc_f_out)
      call get_lun(nout)
      open(unit=nout,file=f_out(1:nc_f_out),status='unknown')

      call banner(nout)

      write(nout, '(a)') message
      write(nout, '(2a)') ' summary file: ',
     :   f_out(1:20)

      write(nout, '(a)') 
     :    ' *** Results computed using gm_td_drvr ***'

* Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_start )
* Date is returned as 'CCYYMMDD' (character date*8) 
* Time is returned as 'hhmmss.sss' (character time*10)
*     character datx*8, time_start*10 
      write(nout, *)
      write(nout, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nout, '(a)') 
     : ' Time Start: '//
     : time_start(1:2)//':'//time_start(3:4)//':'//time_start(5:10)
      write(nout, *)

      write(nout, '(a)') ' file with parameters: '//
     :   f_params(1:nc_f_params)

      write(*, '(a)') ' Compute response spectra? (y/n): '
      psvcalc = ' '
      read(*, '(a)') psvcalc
      call trim_c(psvcalc,nc_psvcalc)
      call upstr(psvcalc)
      if (psvcalc(1:1) .eq. 'Y') then
        write(*, '(a)') 
     :  ' Enter individual periods? (y/n): '
        indvidper = ' '
        read(*, '(a)') indvidper
        call trim_c(indvidper, nc_indvidper)
        call upstr(indvidper)
        if (indvidper(1:1) .eq. 'Y') then
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
          call trim_c(standard_periods, nc_standard_periods)
          call upstr(standard_periods)
          nper = 91
          if (standard_periods(1:1) .eq. 'Y') then
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
      call get_params( f_params, tdflag )
      call write_params(nout, tdflag)
      
100   continue

      write(*, '(a)') ' amag: '
      read(*, *) amag
      am0 = 10.**(1.5*amag + 16.05)
      
      write(*, '(a)') ' dist: '
      read(*, *) r

!Replaced h_eff with f_ff
      rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
        
      new_mr = .true.

      if (psvcalc(1:1) .eq. 'Y') then
        f_suggest = ' '
        f_suggest = f_params(1:indexp-1)//'.mxxxxr00000_rs.td.col'
        write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
        if (rmod .lt. 10.0) then
          write(f_suggest(indexp+9:indexp+11),'(f3.1)') rmod
        else if (rmod .lt. 100.0) then
          write(f_suggest(indexp+8:indexp+11),'(f4.1)') rmod
        else
          write(f_suggest(indexp+7:indexp+11),'(f5.1)') rmod
        end if
        call trim_c(f_suggest,nc_f_suggest)
        
        f_col = ' '      
        write(*, '(a)') 
     :    ' Enter name of column file to contain'//
     :    ' response spectra ("Enter" = ' //
     :     f_suggest(1:nc_f_suggest) // '):'
        read(*, '(a)') f_col
        call trim_c(f_col, nc_f_col)
        if (f_col(1:4) .eq. '    ') then
          f_col = f_suggest(1:nc_f_suggest)
        end if
        call trim_c(f_col,nc_f_col)
        call get_lun(ncol)
        open(unit=ncol, file=f_col(1:nc_f_col), status='unknown')
        write(ncol,'(a)') ' Parameter file: '//f_params(1:nc_f_params)
        nc_f_col_stem = min0(8, index(f_col(1:nc_f_col),'.') - 1)
        colsd_head  =     ' sd:'//f_col(1:nc_f_col_stem)//'________'
        colpsv_head =     'psv:'//f_col(1:nc_f_col_stem)//'________'
        colpsv_sem_head ='sem/psv:'//f_col(1:nc_f_col_stem)//'________'
        colpsa_head =     'psa:'//f_col(1:nc_f_col_stem)//'________'

        colsdgm_head  =     ' sdgm:'//f_col(1:nc_f_col_stem)//'________'
        colpsvgm_head =     'psvgm:'//f_col(1:nc_f_col_stem)//'________'
        colpsvgm_sem_head =
     :                  'sem_psvgm:'//f_col(1:nc_f_col_stem)//'________'
        colpsagm_head =     'psagm:'//f_col(1:nc_f_col_stem)//'________'

        write(ncol, '(
     :    2x,a, 6x,a, 5x,a, 
     :    6x,a, 7x,a, 4x,a, 4x,a,
     :    1x,a, 2x,a,
     :    1x,a, 1x,a, 1x,a, 1x,a,
     :    1x,a, 1x,a, 1x,a, 1x,a
     :                                           )')
     :   'damp', 'per', 'freq', 
     :   'M', 'R', 'F_FF', 'Rmod',
     :   'numsource', 'stress',
     :    colsd_head, colpsv_head, colpsa_head, colpsv_sem_head,
     :    colsdgm_head, colpsvgm_head, colpsagm_head, colpsvgm_sem_head
     
        write_pavd = ' '
        if (indvidper(1:1) .ne. 'Y') then
          write(*, '(a)') 
     :      ' Write pga, pgv, pgd to a column file? (y/n): '
          read(*, '(a)') write_pavd
          call trim_c(write_pavd,nc_write_pavd)
          call upstr(write_pavd)
          if (write_pavd(1:1) .eq. 'Y') then
            f_suggest= ' '
            f_suggest= f_params(1:indexp-1)//'.mxxxxr00000_pavd.td.col'
            write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
            if (r .lt. 10.0) then
              write(f_suggest(indexp+9:indexp+11),'(f3.1)') r
            else if (r .lt. 100.0) then
              write(f_suggest(indexp+8:indexp+11),'(f4.1)') r
            else
              write(f_suggest(indexp+7:indexp+11),'(f5.1)') r
            end if
            call trim_c(f_suggest,nc_f_suggest)
      
            write(*, '(a)') 
     :        ' Enter name of column file for to contain' //
     :        ' pga, pgv, pgd ("Enter" = '//
     :                       f_suggest(1:nc_f_suggest) // '):'
            f_pavd = ' '
            read( *, '(a)') f_pavd
            call trim_c(f_pavd, nc_f_pavd)
            if(f_pavd(1:4) .eq. '    ') then
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
     :            '(t4,a,    t15,a,    t24,a,    t34,a, 
     :             t45,a,    t54,a,    t67,a,    t76,a, 
     :             t86,a,    t94,a,   t106,a,   t119,a)')
     :           't_pgd',    'pgdgm',  'w*pgdgm', 'w2*pgdgm',
     :           't_pgv',  'pgvgm/w',    'pgvgm',  'w*pgvgm',
     :           't_pga', 'pgagm/w2',  'pgagm/w',    'pgagm'
          end if
        end if
      end if  ! end loop to compute response spectrum

      write(*, '(a)') ' Save acc & vel time series? (y/n): '
      save_avd = ' '
      read(*, '(a)') save_avd
      call upstr(save_avd)
      call trim_c(save_avd, nc_save_avd)
      if (save_avd(1:1) .eq. 'Y') then
        write(*, '(a,i4,a)') 
     : '    Save which simulation (out of ', nsims, ')? '
        read(*, *) nacc_save

        f_suggest = ' '
        f_suggest = f_params(1:indexp-1)//'.mxxxxr00000_avd.td.col'
        write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
        if (rmod .lt. 10.0) then
          write(f_suggest(indexp+9:indexp+11),'(f3.1)') rmod
        else if (rmod .lt. 100.0) then
          write(f_suggest(indexp+8:indexp+11),'(f4.1)') rmod
        else
          write(f_suggest(indexp+7:indexp+11),'(f5.1)') rmod
        end if
        call trim_c(f_suggest,nc_f_suggest)
        
        write(*, '(a)') 
     : '    Enter file name for time series ("Enter" = ' //
     :                       f_suggest(1:nc_f_suggest) // '):'
        f_avd = ' '
        read(*, '(a)') f_avd
        call trim_c(f_avd, nc_f_avd)
        if (f_avd(1:3) .eq. '   ') then
           f_avd = f_suggest(1:nc_f_suggest)
           call trim_c(f_avd, nc_f_avd)
        end if
      else
        nacc_save = 0
      end if
     
      call DATE_AND_TIME( date, time_start )

      call Get_Npts_for_TD_Calcs(nstart, nstop, npw2, te, ntaper)  

      ndimen = npw2
      allocate (acc_save(ndimen), vel_save(ndimen), dis_save(ndimen))

      call gm_td(per, nper,  
     :   psvsim, psvsim_std,
     :   pgasim, pgasim_std,
     :   pgvsim, pgvsim_std,
     :   pgdsim, pgdsim_std,
     :   arias, arias_std,
     :   psvsimgm, psvsimgm_std,
     :   pgasimgm, pgasimgm_std,
     :   pgvsimgm, pgvsimgm_std,
     :   pgdsimgm, pgdsimgm_std,
     :   ariasgm, ariasgm_std,
     :   dur_75_05, dur_75_05gm,
     :   dur_95_05, dur_95_05gm,
     :   psvcalc, nacc_save, acc_save, vel_save, dis_save,
     :   nstart, nstop, npw2, te, ntaper)

      if (save_avd(1:1) .eq. 'Y') then
        call get_lun(nuacc)
        open(unit=nuacc,file=f_avd(1:nc_f_avd),status='unknown')
        write(nuacc, '(t12,a,t24,a,t36,a,t48,a)') 'T', 'A', 'V', 'D'
        do i = 1, npw2
          write(nuacc, '(1x,f11.6,1p3(1x,e11.4))') 
     :      float(i-1)*dt, acc_save(i), vel_save(i), dis_save(i)
        end do
      end if 
         
      deallocate (acc_save, vel_save, dis_save)

      write(nout, *)
      write(nout, '(a)') ' *********** NEW R AND M ***********'
      write(nout, '(a, 1p3(1x,e10.3))')
     :      ' r, f_ff, rmod, amag, kappa = ', 
     :        r, f_ff, rmod, amag, kappa_f(amag)
      if (psvcalc(1:1) .eq. 'Y') then
        write(nout, '(2x,2a)') 'File with response spectra: ', 
     :    f_col(1:30)
      else
        write(nout, '(2x,a)') 'No response spectra computed'
      end if
      write(nout, '(2x,a,1x,1pe10.3)') ' const= ', const
      if (numsource .eq. 1 .or. numsource .eq. 2) then
        write(nout, '(2x,a, f6.3, 
     :      1p, 1x,e9.2, 2(1x,e10.3), 3(1x,e9.2))') 
     :    ' amag, stress, fa, fb, dursource, durpath, durex= ', 
     :      amag, stress, fa, fb, dursource_calc, durpath_calc, durex
      else
        write(nout, '(2x,a, f6.3, 
     :      1p, 2(1x,e10.3), 3(1x,e9.2))') 
     :    ' amag, fa, fb, dursource, durpath, durex= ', 
     :      amag, fa, fb, dursource_calc, durpath_calc, durex
      end if
      write(nout, '(2x,a,1p2(1x,e10.3))') ' am0, am0b_m0fa= ', 
     :                                       am0, am0b_m0
      write(nout, '(2x,a, i6, 1x,i6, 1x,f8.5, 2(1x,f6.1))')
     :  ' npw2, ntaper, dt, te, total duration = ', 
     :    npw2, ntaper, dt, te, float(npw2)*dt
     
      write(nout,'(2x,a, 2x,a, 
     :             1x,a, 2x,a, 
     :             1x,a, 2x,a)') 
     :    'pgdam(cm)',    'sem/pgdam', 
     :    'pgvam(cm/s)',  'sem/pgvam', 
     :    'pgaam(cm/s2)', 'sem/pgaam'
      write(nout,'(1p, 
     :         1x,e10.3, 1x,e10.3,
     :         2x,e10.3, 1x,e10.3,
     :         3x,e10.3, 1x,e10.3)')
     :     pgdsim, (pgdsim_std/sqrt(float(nsims)))/pgdsim, 
     :     pgvsim, (pgvsim_std/sqrt(float(nsims)))/pgvsim, 
     :     pgasim, (pgasim_std/sqrt(float(nsims)))/pgasim
     
      write(nout,'(2x,a, 2x,a, 
     :             1x,a, 2x,a, 
     :             1x,a, 2x,a)') 
     :    'pgdgm(cm)',    'sem_pgdgm', 
     :    'pgvgm(cm/s)',  'sem_pgvgm', 
     :    'pgagm(cm/s2)', 'sem_pgagm'
      write(nout,'(1p, 
     :         1x,e10.3, 1x,e10.3,
     :         2x,e10.3, 1x,e10.3,
     :         3x,e10.3, 1x,e10.3
     :                                      )') 
     :     pgdsimgm, 
     :     (pgdsimgm_std**(1.0/sqrt(float(nsims)))), 
     :     pgvsimgm, 
     :     (pgvsimgm_std**(1.0/sqrt(float(nsims)))), 
     :     pgasimgm, 
     :     (pgasimgm_std**(1.0/sqrt(float(nsims))))
     
      write(nout,'( 1x,a, 2x,a)') 
     :    'Arias intensity(cm/s)', 'sem/Arias'
      write(nout,'(1p, 10x,e10.3,  10x,e10.3)') 
     :     arias, (arias_std/sqrt(float(nsims)))/arias

      write(nout,'( 1x,a, 1x,a)') 
     :    'Arias intensitygm(cm/s)', 'sem_Ariasgm'
      write(nout,'(1p, 14x,e10.3,  2x,e10.3)') 
     :     ariasgm, 
     :     (ariasgm_std**(1.0/sqrt(float(nsims))))
 
      write(*,*)

      if (psvcalc(1:1) .eq. 'Y') then
      
        write(nout,'(2x,a,f6.3)') 
     :        ' Fractional oscillator damping = ', damp
        write(nout,'( 
     :                4x,a, 6x,a, 
     :                2x,a, 
     :                4x,a, 
     :                1x,a, 4x,a,
     :                1x,a, 
     :                2x,a,
     :                1x,a, 2x,a
     :                                                        )')
     :             'per(s)', 'freq', 
     :             'psv(cm/s)',  
     :             'sem/psv', 
     :             'psa(cm/s2)', 'psd(cm)',
     :             'psvgm(cm/s)', 
     :             'psvgm_sem', 
     :             'psagm(cm/s2)', 'psdgm(cm)'
     
        do i = 1, nper
          write(nout, '(
     :          1x,f9.4,  1x,f9.4, 1p,
     :          1x,e10.3, 
     :          1x,e10.3, 
     :          1x,e10.3, 1x,e10.3, 
     :          2x,e10.3, 
     :          1x,e10.3, 
     :          2x,e10.3, 1x,e10.3 
     :                                       )')
     :      per(i), 1.0/per(i), 
     :      psvsim(i),
     :      (psvsim_std(i)/sqrt(float(nsims)))/psvsim(i),
     :      (twopi/per(i))*psvsim(i), psvsim(i)/(twopi/per(i)),
     :      psvsimgm(i),
     :      (psvsimgm_std(i)**(1.0/sqrt(float(nsims)))),
     :      (twopi/per(i))*psvsimgm(i), psvsimgm(i)/(twopi/per(i))
     
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
     :      1p, 
     :              3x,e10.3, 3x,e10.3,
     :              3x,e10.3, 
     :              7x,e10.3,
     :              5x,e10.3, 5x,e10.3,
     :              5x,e10.3, 
     :              9x,e10.3
     :                                  )')
     :      damp, per(i), 1.0/per(i), 
     :      amag, r, f_ff, rmod,
     :      numsource, stress_out,
     :      psvsim(i)/(twopi/per(i)), psvsim(i), 
     :      (twopi/per(i))*psvsim(i), 
     :      (psvsim_std(i)/sqrt(float(nsims)))/psvsim(i),
     :      psvsimgm(i)/(twopi/per(i)), psvsimgm(i), 
     :      (twopi/per(i))*psvsimgm(i), 
     :      (psvsimgm_std(i)**(1.0/sqrt(float(nsims))))
        end do
        
        close(unit=ncol)
        if (write_pavd(1:1) .eq. 'Y') then
          do i = 1, 2
            write(npavd, '(3(0p1x,f7.3,1p3(1x,e10.3)))')
     :       t_pgd(i),pgdsimgm,
     :                pgdsimgm*(twopi/t_pgd(i)),
     :                pgdsimgm*(twopi/t_pgd(i))**2,
     :       t_pgv(i),pgvsimgm/(twopi/t_pgv(i)),
     :                pgvsimgm,
     :                pgvsimgm*(twopi/t_pgv(i)),
     :       t_pga(i),pgasimgm/(twopi/t_pga(i))**2,
     :                pgasimgm/(twopi/t_pga(i)),
     :                pgasimgm
          end do
          close(unit=npavd)
        end if
        
      end if

      write(nout, *)
      call DATE_AND_TIME( date, time_stop )
      call time_diff(time_start, time_stop, time_elapsed)
      write(nout, '(a,1x,1pe10.3)') 
     :     ' Elapsed time (sec): ', time_elapsed

      write(*,*)
      write(*, '(a)') 
     : ' Compute results for another r and M? (y/n;"Enter"=quit): '
      read(*, '(a)') buf
      if (buf(1:4) .eq. '    ') go to 999
      if (buf(1:1) .eq. 'n' .or. buf(1:1) .eq. 'N') go to 999

      goto 100

999   continue      


      close(unit=nout)
      stop
      end
* --------------- END gm_td_drvr ---------------------------------

!      include 'gm_td.for'
!      include 'acc_ts.for'
!      include 'td_subs.for' 
   !NOTE: run make_td_subs.bat to make the file; it now includes rv_td_subs, recipes, etc
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'td_subs.for'
      include 'smsim_util_subs.for'



