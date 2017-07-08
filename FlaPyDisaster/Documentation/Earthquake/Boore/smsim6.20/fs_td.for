
!----------------- BEGIN fs_td -----------------------------
      subroutine fs_td(per, nper,  
     :    psvsim, psvsim_std, 
     :    pgasim, pgasim_std, 
     :    pgvsim, pgvsim_std, 
     :    pgdsim, pgdsim_std, 
     :    arias, arias_std,
     :    psvcalc, nacc_save, acc_save, vel_save, dis_save,
     :    nstart, nstop, npw2, te, ntaper,
     :    f_params)

! Uses time-domain stochastic model to compute ground motion
!  (SMSIM = Stochastic Model Simulation) 
! and average FAS.  It writes out the target spectral amplitude as well
! as the average FAS obtained by averaging the FAS of each time series
! and the squared FAS of each time series.

! The indices controlling the extent and duration of motion are
! obtained by calling Get_Npts_for_TD_Calcs fron within the calling program, using this
! statement:
!
!       call Get_Npts_for_TD_Calcs(nstart, nstop, npw2, te)

!         09/10/97 - This is a renamed and slightly revised 
!                    version of smsim_td.for, with
!                    portions surrounded by "* TDFA" to compute
!                    Fourier spectra and average
!                    Fourier spectra for comparison with the target spectra.
!                    See PCO notes in early January, 1996.
!                    (NOTE: the first runs were done on 12/29/95, I am
!                    redoing them again, and this time I will make an
!                    executable tdfadrvr.  Before, I simply saved the
!                    proper version of smsim_td.for to a backup file and
!                    compiled td_drvr with this new version.  This is
!                    a dangerous operation and did not leave a trail of
!                    file that I could track down almost two years later.)
!         08/01/98 - Calculate standard deviation peak values
!         09/11/98 - Increased array dimensions from 16400 to 66000
!         01/13/99 - Added computation of Arias intensity
!         01/14/99 - Added code to compute and print out FAS, and increased 
!                    array dimensions.
!         02/10/99 - Modified to account for computation of displacement
!         02/10/99 - Transfer choice of npts from rvtdsubs.for to this
!                    subroutine.  This also includes pad extension if a low-cut
!                    filter is used.  
!                    In all cases 
!                    npts*dt >= max(tshift, tfpad) + dur_fctr * nmotion * dt 
!                               + tfpad 
!         01/07/00 - Upgraded to most recent version of acc2vd
!         01/27/01 - Modified time series window computation (new parameters)
!         02/10/01 - Added v0, d0 to acc2vd
!         02/12/01 - Combine smsim_td and get_acc, and use dynamically 
!                    allocatable arrays.  Put calculation of ndimen into
!                    a subroutine.  Rename "npts" to "npw2".
!         06/14/02 - Written by modifying gm_td according to the old routine
!                    smsim_tdfa, contained in the old tdfadrvr.for
!         06/14/02 - Bowing to convention, I changed "prv" to "psv", although 
!                    I prefer "prv" (and "paa" rather than "psa").
!                    Changed "tdfa" to "fas_td" in variable names.
!         02/05/03 - Changed units of g from 980 cm/s/s to 981 cm/s/s.
!         05/11/07 - Removed "\smsim\" from include statements
!         08/19/08 - Use trim_c for output file
!         08/26/08 - Suggest a better name for the output file. This
!                    required adding f_params to the input parameter list.
!        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                   I caught this only when I compiled the program using the -chk
!                   switch.  This probably means that all of my time-domain runs
!                   up till now used ntaper = 0, even if a nonzero taper was 
!                   specified (the standard being 0.05).
!        12/04/09 - Call rd_calc rather than rdcalcdp
!                 - The construction of the frequency and fas column headings to
!                   include the fas filename ran into problems when the
!                   params file had more than one period in the name.  For this
!                   reason I have used non-fas-filename-specific column labels
!        12/23/09 - Add m, r, h_eff, rmod to output
!                 - Change "fstd" to "fas_td"
!        04/08/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        07/03/11 - Write the name of the params file to the first line of the output
!        02/21/14 - Add name of program to output
!        03/17/14 - Add geometric mean (trap for zero FAS; assign phony value for f=0)


! ALLOCATABLE: comment out following lines to disable dynamic allocation
      real acc(:), vel(:), dis(:), a_sq_int(:)
      allocatable :: acc, vel, dis, a_sq_int
      real fas_td_freq(:), fas_td_cabs(:), 
     :   fas_td_cabs_avg(:), fas_td_cabs_sqavg(:),
     :   fas_td_log_fas_avg(:)
      complex fas_td_work(:)
      allocatable :: fas_td_freq, fas_td_cabs, 
     :   fas_td_cabs_avg, fas_td_cabs_sqavg, fas_td_log_fas_avg, 
     :   fas_td_work
! Remove comment character in lines below to disable dynamic allocation
!      real acc(66000), vel(66000), dis(66000), a_sq_int(66000)
!      real fas_td_freq(18000), fas_td_cabs(18000), 
!     :   fas_td_cabs_avg(18000), fas_td_cabs_sqavg(18000)
!      complex fas_td_work(66000)
! ALLOCATABLE: end of group

! fas_td *****************
      character f_suggest*120, f_params*(*)
      character colfrq_head*12, colfas_head*12, f_fas_td*220
! fas_td *****************

      real avgpsv(400), per(*), psvsim(*), psvsim_std(*), 
     :     acc_save(*), vel_save(*), dis_save(*)
      double precision psvcumsq(400), pgacumsq, pgvcumsq, pgdcumsq, 
     :                 asqintcumsq
      logical rmv_trend

      character psvcalc*1

      include 'smsim.fi'
      
! pi, twopi now computed in smsim.fi
!      pi = 4.0 * atan(1.0)
!      twopi = 2.0 * pi

      do i = 1, nper
        avgpsv(i) = 0.
        psvcumsq(i) = 0.
        psvsim_std(i) = 0.
      end do
      avgpga = 0.
      avgpgv = 0.
      avgpgd = 0.
      avgasqint = 0.
      pgacumsq = 0.
      pgvcumsq = 0.
      pgdcumsq = 0.
      asqintcumsq = 0.
      pgasim_std = 0.
      pgvsim_std = 0.
      pgdsim_std = 0.
      arias_std = 0.

      iseed = -abs(seed)

! ALLOCATABLE: comment out following lines to disable dynamic allocation
      ndimen = npw2
      allocate ( acc(ndimen), vel(ndimen), dis(ndimen), 
     :          a_sq_int(ndimen) )

! fas_td *****************
      nnyq = npw2/2 + 1
      allocate ( fas_td_freq(nnyq), fas_td_cabs(nnyq), 
     :   fas_td_cabs_avg(nnyq), fas_td_cabs_sqavg(nnyq), 
     :   fas_td_log_fas_avg(nnyq),  
     :   fas_td_work(npw2) )
! fas_td *****************

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

! fas_td *****************
      do ifas_td = 1, nnyq
        fas_td_cabs_avg(ifas_td) = 0.0
        fas_td_cabs_sqavg(ifas_td) = 0.0
        fas_td_log_fas_avg(ifas_td) = 0.0
      end do
! fas_td *****************

      write(*,*)
      do isim = 1, nsims
         write(*,'(3(a,i5),a)') 
     :    '+ Patience! Computing accelerogram # ', isim,
     :    ' of ', nsims,
     :    ' and rs at ', nper,' periods.'

         call acc_ts(acc, nstart, nstop, npw2, te, ntaper)

! fas_td *****************
        do ifas_td = 1, npw2
          fas_td_work(ifas_td) = 0.0
        end do

        do ifas_td = 1, npw2
          fas_td_work(ifas_td) = cmplx(acc(ifas_td),0.0)
        end do

        call fork(npw2,fas_td_work,-1.)

        df = 1.0/(float(npw2)*dt)
        do i = 1,nnyq
          f = (i-1) * df
          fas_td_freq(i) = f
          fas_td_fact = dt * sqrt(float(npw2))
          fas_td_cabs(i) = fas_td_fact * cabs(fas_td_work(i))
        end do
        nfreqdb = nnyq

        do ifas_td = 1, nfreqdb
          fas_td_cabs_avg(ifas_td) = 
     :           fas_td_cabs_avg(ifas_td) + fas_td_cabs(ifas_td)
          fas_td_cabs_sqavg(ifas_td) = 
     :           fas_td_cabs_sqavg(ifas_td) + fas_td_cabs(ifas_td)**2
     
          if (ifas_td == 1) then
            fas_td_log_fas_avg(1) = -999.0
          else 
            if (fas_td_cabs(ifas_td) <= 0.0) then
              print *,
     :          ' ERROR IN FS_TD: fas_td_cabs <= 0 for ifas_td = ', 
     :             ifas_td, '; QUITTING!!!'
              STOP
            else
              fas_td_log_fas_avg(ifas_td) = 
     :           fas_td_log_fas_avg(ifas_td) + 
     :                     alog10(fas_td_cabs(ifas_td))
            end if
          end if
        end do          
! fas_td *****************

!         rmv_trend = .true.
         rmv_trend = .false.
         v0 = 0.0
         d0 = 0.0
         call acc2vd(acc, npw2, dt, rmv_trend, v0, d0, vel, dis) 

         call accsqint(acc, npw2, dt, .false., a_sq_int)

         if (isim .eq. nacc_save) then
           do i = 1, npw2
             acc_save(i) = acc(i)
             vel_save(i) = vel(i)
             dis_save(i) = dis(i)
           end do
         end if
         
! Compute pga, pgv, pgd, asqint:

         call mnmax(acc, 1, npw2, 1, amin, amax)
         pga = amax1(abs(amin), abs(amax))

         call mnmax(vel, 1, npw2, 1, vmin, vmax)
         pgv = amax1(abs(vmin), abs(vmax))

         call mnmax(dis, 1, npw2, 1, dmin, dmax)
         pgd = amax1(abs(dmin), abs(dmax))

         asqint = a_sq_int(npw2)

         avgpga = avgpga + pga
         avgpgv = avgpgv + pgv
         avgpgd = avgpgd + pgd
         avgasqint = avgasqint + asqint
         pgacumsq = pgacumsq + pga*pga
         pgvcumsq = pgvcumsq + pgv*pgv
         pgdcumsq = pgdcumsq + pgd*pgd
         asqintcumsq = asqintcumsq + asqint*asqint
 
         if (psvcalc .eq. 'Y') then
! Compute psv:
           do i = 1, nper
              omega = twopi/per(i)
              d0 = 0.0
              v0 = 0.0
              call rd_calc(acc, npw2, omega, damp, dt, rd, d0, v0)
              psv = omega * rd
              avgpsv(i) = avgpsv(i) + psv
              psvcumsq(i) = psvcumsq(i) + psv*psv
           end do                                     ! iper
         end if

      end do                           ! isim

! fas_td *****************
      call trim_c(f_params, nc_f_params)
      do i = nc_f_params, 1, -1
        if (f_params(i:i) .eq. '.') then
           indexp = i
           go to 991
        end if
        indexp = 1
      end do
991   continue 

      f_suggest = ' '
      f_suggest = f_params(1:indexp-1)//'.mxxxxr00000_fs.td.col'
      write(f_suggest(indexp+2:indexp+5),'(f4.2)') amag
      if (rmod .lt. 10.0) then
        write(f_suggest(indexp+9:indexp+11),'(f3.1)') rmod
      else if (rmod .lt. 100.0) then
        write(f_suggest(indexp+8:indexp+11),'(f4.1)') rmod
      else
        write(f_suggest(indexp+7:indexp+11),'(f5.1)') rmod
      end if
      call trim_c(f_suggest,nc_f_suggest)
        
      f_fas_td = ' '      
      write(*, '(a)') 
     :    ' Enter name of column file to contain'//
     :    ' Fourier spectra (cr = '//
     :     f_suggest(1:nc_f_suggest)//'):'
      read(*, '(a)') f_fas_td
      call trim_c(f_fas_td, nc_f_fas_td)
      if (f_fas_td(1:4) .eq. '    ') then
        f_fas_td = f_suggest(1:nc_f_suggest)
      end if
      call trim_c(f_fas_td,nc_f_fas_td)
      call get_lun(nu_fas_td)
      open(unit=nu_fas_td,file=f_fas_td(1:nc_f_fas_td),
     :     status='unknown')

!      colfrq_head = 'frq:________'
!      colfrq_head = '___frequency'
!      colfrq_head(5:index(f_fas_td,'.')+3) = 
!     :     f_fas_td(1:index(f_fas_td,'.')-1)
!      colfas_head = 'fas:________'
!      colfas_head = '__________fas'
!      colfas_head(5:index(f_fas_td,'.')+3) = 
!     :     f_fas_td(1:index(f_fas_td,'.')-1)

      write(nu_fas_td,'(a)') ' Program file: FAS_TD_DRVR' 
      write(nu_fas_td,'(a)') 
     :    ' Parameter file: '//f_params(1:nc_f_params) 
      
      write(nu_fas_td, '( 
     :       4x,a, 6x,a, 3x,a, 3x,a,
     :       4x,a, 
     :       3x,a, 
     :       3x,a, 
     :       5x,a, 1x,a, 7x,a,
     :       5x,a,
     :       1x,a)') 
     :      'M', 'r', 'f_ff', 'rmod',
     :       'freq(hz)', 
     :       'period(s)',
     :       'model_fas', 
     :      'lastfas', 
     :      'E(fa^2)^1/2', 'E(fa)', 'E(^2)/E',
     :      '10^E(log fa)'
      do ifas_td = 1, nfreqdb
        fas_td_cabs_avg(ifas_td) = fas_td_cabs_avg(ifas_td)/nsims
        fas_td_cabs_sqavg(ifas_td) = 
     :                     sqrt(fas_td_cabs_sqavg(ifas_td)/nsims)
        ratio = -1
        if (fas_td_cabs_avg(ifas_td) .ne. 0.0) then
          ratio = 
     :      fas_td_cabs_sqavg(ifas_td)/fas_td_cabs_avg(ifas_td)
        end if
        if (ifas_td == 1) then
            fas_td_log_fas_avg(1) = -999.0
        else 
          fas_td_log_fas_avg(ifas_td) = 
     :        10.0**(fas_td_log_fas_avg(ifas_td)/nsims)
        end if

        if (fas_td_freq(ifas_td) == 0.0) then
          per4output = 9999.9
        else
          per4output = 1.0/fas_td_freq(ifas_td)
        end if
        
        write(nu_fas_td, '(
     :                     1x,f4.2, 1x,f6.2, 1x,f6.2, 1x,f6.2,
     :                     1x,es11.4,
     :                     1x,es11.4,
     :                     1x,es11.4,
     :                     1x,es11.4,
     :                     3(1x,es11.4),
     :                     2x,es11.4     )')
     :      amag, r, f_ff, rmod,
     :      fas_td_freq(ifas_td),
     :      per4output,
     :      spect_amp(fas_td_freq(ifas_td)), 
     :      fas_td_cabs(ifas_td), 
     :      fas_td_cabs_sqavg(ifas_td), fas_td_cabs_avg(ifas_td), ratio,
     :      fas_td_log_fas_avg(ifas_td)
      end do
      close(unit=nu_fas_td)

      deallocate ( fas_td_freq, fas_td_cabs, 
     :   fas_td_cabs_avg, fas_td_cabs_sqavg, fas_td_work )

! fas_td *****************
! Compute averages
      pgasim = avgpga/float(nsims)
      pgvsim = avgpgv/float(nsims)
      pgdsim = avgpgd/float(nsims)
      asqint = avgasqint/float(nsims)

      if (psvcalc .eq. 'Y') then
        do i = 1, nper
           psvsim(i) = avgpsv(i)/float(nsims)
        end do
      end if

! Compute standard deviations.  Note that this is a poor way to do so
! (because of subtracting two possibly large numbers), but
! it avoids having to save the values in an array.  This is particularly
! important for the psv array, which could be quite large.  Hopefully
! using double precision will minimize the roundoff problem.  Also note that
! the standard deviation is not too meaningful; it does not capture
! variability associated with variability of the input parameters.

      if (nsims .gt. 1) then
        factr = float(nsims)/float(nsims-1)
        pgasim_std = dsqrt(factr * (pgacumsq/nsims - pgasim*pgasim))
        pgvsim_std = dsqrt(factr * (pgvcumsq/nsims - pgvsim*pgvsim))
        pgdsim_std = dsqrt(factr * (pgdcumsq/nsims - pgdsim*pgdsim))
        asqint_std = dsqrt(factr * (asqintcumsq/nsims-asqint*asqint))
        if (psvcalc .eq. 'Y') then
          do i = 1, nper
             psvsim_std(i) = 
     :        dsqrt(factr * (psvcumsq(i)/nsims - 
     :                      psvsim(i)*psvsim(i)))
          end do
        end if
      end if        

      g = 981.0    ! acceleration of gravity, assuming acc units of cm/s^2

      arias_fctr = pi/(2.0*g)
      arias = arias_fctr * asqint
      arias_std = arias_fctr * asqint_std 
      
! ALLOCATABLE: comment out following line to disable dynamic allocation
      deallocate (acc, vel, dis, a_sq_int)
! ALLOCATABLE: end of group

      return
      end
!----------------- END fs_td -----------------------------

