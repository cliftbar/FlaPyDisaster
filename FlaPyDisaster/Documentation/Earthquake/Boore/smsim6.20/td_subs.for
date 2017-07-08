
!----------------- BEGIN gm_td -----------------------------
      subroutine gm_td(per, nper,  
     :    psvsim, psvsim_std, 
     :    pgasim, pgasim_std, 
     :    pgvsim, pgvsim_std, 
     :    pgdsim, pgdsim_std, 
     :    arias,  arias_std,
     :    psvsimgm, psvsimgm_fctr, 
     :    pgasimgm, pgasimgm_fctr, 
     :    pgvsimgm, pgvsimgm_fctr, 
     :    pgdsimgm, pgdsimgm_fctr, 
     :    ariasgm,  ariasgm_fctr,
     :    dur_75_05, dur_75_05gm,
     :    dur_95_05, dur_95_05gm,
     :    psvcalc, nacc_save, acc_save, vel_save, dis_save,
     :    nstart, nstop, npw2, te, ntaper)

! Compute various measures of ground motion using time-domain stochastic 
! model simulations.
 
! The indices controlling the extent and duration of motion are
! obtained by calling Get_Npts_For_TD_Calcs fron within the calling program, using this
! statement:
!
!       call Get_Npts_For_TD_Calcs(nstart, nstop, npw2, te, ntaper)


! Dates:  05/10/95 - Written by D. M. Boore
!         05/30/95 - Modified by Basil Margaris to allow choice of window
!         06/02/95 - Further renamed (from psvsimnu) and modified.
!         06/08/95 - Normalize by sqrt(avg square amp)
!         06/09/95 - Get r, amag from common, not parameter list
!         06/12/95 - Removed writing of acc, vel to a file; this should
!                    be done in the front-end driver program.
!         08/08/95 - Removed dc from noise segment before going to
!                    the frequency domain.
!         08/08/95 - Changed things so that remove dc from noise sample before
!                    applying the window.
!         08/18/95 - Added subroutine Write_Params
!         10/17/95 - Added flag to Get_Params and Write_Params
!         10/17/95 - Remove dc or not from random series, depending on a flag
!                    (irmvdc) that is passed through the include statement
!         11/14/95 - Added call to const_am0_gsprd
!         11/14/95 - Broke out numerical recipes and subroutines used
!                    by both rv and td into separate collections
!                    of Fortran subroutines
!         11/16/95 - Replaced 'shape = ...' statement with call to spect_amp
!         12/06/95 - Major simplification in exponential window, removing
!                    tapers (the window itself provides tapers) and correcting
!                    a bug so that the window can be used for a duration
!                    other than the duration used to determine the window 
!                    parameters.
!         12/14/95 - Assigned durex the source plus path duration
!         12/22/95 - Removed rmean real part of work only
!         01/22/96 - Use REALFT rather than FORK
!         04/12/96 - Changed names psv, sd to psv, rd
!         04/15/96 - Changed "rd_spect" to "rd_calc"
!         08/01/98 - Calculate standard deviation peak values
!         09/11/98 - Increased array dimensions from 16400 to 33000
!         01/13/99 - Added computation of Arias intensity
!         02/09/99 - Added computation of displacement; removed subtracting
!                    linear trend from acc in acc2vd routine
!         03/05/99 - Removed include rvtdsubs.for, recipes.for from the
!                    end; these must now be included in the driver programs.
!         07/02/99 - Added arguments to durpath to incorporate Atkinson and 
!                    Silva (1999) magnitude-dependent modification to distance.
!         08/03/99 - Added uniform deviate if desired
!         02/10/99 - Transfer choice of npts from rvtdsubs.for to this
!                    subroutine.  This also includes pad extension if a low-cut
!                    filter is used.  
!                    In all cases 
!                    npts*dt >= max(tshift, tfpad) + dur_fctr * nmotion * dt 
!                               + tfpad 
!         03/12/99 - Suggest increasing dt if the time series is too long.
!         01/07/00 - Upgraded to most recent version of acc2vd
!         01/27/01 - Modified time series window computation (new parameters)
!         01/28/01 - Increased dimensions to 66000 (should use allocatable 
!                    arrays)
!         02/12/01 - Combine gm_td and get_acc, and use dynamically 
!                    allocatable arrays.  Put calculation of ndimen into
!                    a subroutine.  Rename "npts" to "npw2".
!         03/12/01 - Use double precision version of rd_calc
!         05/28/02 - Separate out computation of acceleration time series
!                    as a separate subroutine ("smsim_acc", contained in
!                    file "smsimacc.for").
!         06/13/02 - Renamed "smsim_td" to "gm_td", and deleted routines
!                    now contained in smsimacc, td_subs, and rvtdsubs, which
!                    are brought into the calling program via include
!                    statements.  In addition, pass variables 
!                    nstart, nstop, npw2, and te through the argument list.
!         06/14/02 - Bowing to convention, I changed "prv" to "psv", although 
!                    I prefer "prv" (and "paa" rather than "psa")
!         02/05/03 - Changed units of g from 980 cm/s/s to 981 cm/s/s.
!        05/11/07 - Removed "\smsim\" from include statements
!        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                   I caught this only when I compiled the program using the -chk
!                   switch.  This probably means that all of my time-domain runs
!                   up till now used ntaper = 0, even if a nonzero taper was 
!                   specified (the standard being 0.05).
!        02/21/09 - Use a call to avg_std to obtain mean and standard deviation.
!                   This makes the coding less cumbersome and is useful when
!                   computing both geometric and arithmetic means
!                   (as in this revision).
!        02/21/09 - Compute 95%-5% duration
!        02/23/09 - Hardwire 75% rather than 95% in determining duration, as 
!                   recommended by Ou and Herrmann (this is also consistent with
!                   some Husid plots I made).
!        12/04/09 - Call rd_calc rather than rdcalcdp
!        03/25/10 - Replaced call to rd_calc with call to rscalc_interp_acc.
!        08/25/10 - Shorten "patience" screen message.
!        08/25/10 - Shorten "patience" screen message.
!        01/17/11 - Compute 95%-5% duration
!        02/02/11 - Write SD for individual simulations to a file named 'SD_sims.out'
!        03/04/11 - Modify formatting of sd_out to allow for 410 SD
!        04/17/11 - Change "std" in geometric mean variables to "fctr", because
!                   the quantities are factors (10**std_logy), not standard deviations.
!        04/19/11 - Increase numbers of digits for screen message ("Compute accelerogram...")
!        02/18/14 - Increase dimension of colhead

      real acc(:), vel(:), dis(:), a_sq_int(:) 
      allocatable :: acc, vel, dis, a_sq_int

      real pga4avg(:), pgv4avg(:), pgd4avg(:), asqint4avg(:), 
     :     psv4avg(:,:), dur75_5array(:), dur95_5array(:)
      allocatable :: pga4avg, pgv4avg, pgd4avg, asqint4avg, 
     :     psv4avg, dur75_5array, dur95_5array

      real omega

      real per(*), 
     :     psvsim(*), psvsim_std(*), 
     :     psvsimgm(*), psvsimgm_fctr(*), 
     :     acc_save(*), vel_save(*), dis_save(*)

      logical rmv_trend

      character psvcalc*1, psvcalc_uc*1

! 02/18/14
      character colhead(610)*9
! 02/18/14

      include 'smsim.fi'
      
      psvcalc_uc = psvcalc
      call upstr(psvcalc_uc)
      
! pi, twopi now computed in smsim.fi
!      pi = 4.0 * atan(1.0)
!      twopi = 2.0 * pi

! 02/02/11
      if (psvcalc_uc == 'Y') then          
        call get_lun(nu_sims)
        open(nu_sims,file='SD_sims.out',status='unknown')
        colhead = 'SD_______'
        do i = 1, nper
          if (per(i) < 10.0) then
            write(colhead(i)(5:9),'(f5.3)') per(i)
          else if (per(i) < 100.0) then
            write(colhead(i)(4:9),'(f6.3)') per(i)
          else
            write(colhead(i)(3:9),'(f7.3)') per(i)
          end if 
        end do
        write(nu_sims,'(1x,a4, 410(4x,a9))') 
     :       'isim', (colhead(i), i=1, nper)        
      end if
! 02/02/11
      

      allocate(pga4avg(nsims), pgv4avg(nsims), pgd4avg(nsims), 
     :     asqint4avg(nsims), 
     :     psv4avg(nsims,nper), 
     :     dur75_5array(nsims), dur95_5array(nsims))
     
      pga4avg = 0.0
      pgv4avg = 0.0
      pgd4avg = 0.0
      aintsq4avg = 0.0
      psv4avg = 0.0
 
      iseed = -abs(seed)

      ndimen = npw2
      allocate ( acc(ndimen), vel(ndimen), dis(ndimen), 
     :          a_sq_int(ndimen) )

      write(*,*)
      loop over simulations: DO isim = 1, nsims
         write(*,'(3(a,i4),a)') 
     :    '+ Compute accelerogram # ', isim,
     :    ' of ', nsims,
     :    ' and rs at ', nper,' periods.'

         call acc_ts(acc, nstart, nstop, npw2, te, ntaper)

!         rmv_trend = .true.
         rmv_trend = .false.
         v0 = 0.0
         d0 = 0.0
         call acc2vd(acc, npw2, dt, rmv_trend, v0, d0, vel, dis) 

         call accsqint(acc, npw2, dt, .false., a_sq_int)
         
         a_sq_int_max = a_sq_int(npw2)
         call locate(a_sq_int,npw2,0.05*a_sq_int_max,j05) 
         call locate(a_sq_int,npw2,0.75*a_sq_int_max,j75) 
         call locate(a_sq_int,npw2,0.95*a_sq_int_max,j95) 
        
         dur75_5array(isim) = float(j75-j05)*dt
         dur95_5array(isim) = float(j95-j05)*dt
         
         if (isim == nacc_save) then
           do i = 1, npw2
             acc_save(i) = acc(i)
             vel_save(i) = vel(i)
             dis_save(i) = dis(i)
           end do
         end if
         
! Compute pga, pgv, pgd, asqint:

         call mnmax(acc, 1, npw2, 1, amin, amax)
         pga = amax1(abs(amin), abs(amax))
         pga4avg(isim) = pga
         
         call mnmax(vel, 1, npw2, 1, vmin, vmax)
         pgv = amax1(abs(vmin), abs(vmax))
         pgv4avg(isim) = pgv

         call mnmax(dis, 1, npw2, 1, dmin, dmax)
         pgd = amax1(abs(dmin), abs(dmax))
         pgd4avg(isim) = pgd

         asqint = a_sq_int(npw2)
         asqint4avg(isim) = asqint
         
         psvcalc_uc = psvcalc
         call upstr(psvcalc_uc)

         if (psvcalc_uc == 'Y') then  
         
! Compute psv:
           d0 = 0.0
           v0 = 0.0
           do i = 1, nper
              omega = twopi/per(i)
              call rscalc_interp_acc(acc, npw2, omega, damp, dt,
     :                             rd, rv, aa)
!              call rd_calc(acc, npw2, omega, damp, dt, rd, d0, v0)
              psv = omega * rd
              psv4avg(isim,i) = psv
           end do                                     ! iper
! 02/02/11
           write(nu_sims,'(1x,i4, 410(1x,es12.5))') 
     :           isim, (psv4avg(isim,i)/omega, i= 1, nper)           
! 02/02/11
         end if

      END DO  loop over simulations                         ! isim

! 02/02/11
      if (psvcalc_uc == 'Y') then          
        close(nu_sims)
      end if
! 02/02/11

! Compute averages

      itype_avg = 1 ! arithmetic
      call avg_std(pga4avg,nsims,pgasim,pgasim_std,itype_avg)
      call avg_std(pgv4avg,nsims,pgvsim,pgvsim_std,itype_avg)
      call avg_std(pgd4avg,nsims,pgdsim,pgdsim_std,itype_avg)
      call avg_std(asqint4avg,nsims,asqintsim,asqintsim_std,
     :             itype_avg)
      call avg_std(dur75_5array,nsims,dur_75_05,dur_75_05_std,
     :             itype_avg)
      call avg_std(dur95_5array,nsims,dur_95_05,dur_95_05_std,
     :             itype_avg)

      itype_avg = 2 ! geometric
      call avg_std(pga4avg,nsims,pgasimgm,pgasimgm_fctr,itype_avg)
      call avg_std(pgv4avg,nsims,pgvsimgm,pgvsimgm_fctr,itype_avg)
      call avg_std(pgd4avg,nsims,pgdsimgm,pgdsimgm_fctr,itype_avg)
      call avg_std(asqint4avg,nsims,asqintsimgm,asqintsimgm_fctr,
     :             itype_avg)
      call avg_std(dur75_5array,nsims,dur_75_05gm,dur_75_05gm_fctr,
     :             itype_avg)
      call avg_std(dur95_5array,nsims,dur_95_05gm,dur_95_05gm_fctr,
     :             itype_avg)

      if (psvcalc_uc == 'Y') then 
      
        do i = 1, nper
          itype_avg = 1 ! arithmetic
          call avg_std(psv4avg(1,i),nsims,psvsim(i),psvsim_std(i),
     :               itype_avg)
          itype_avg = 2 ! geometric
          call avg_std(psv4avg(1,i),nsims,psvsimgm(i),psvsimgm_fctr(i),
     :               itype_avg)
        end do

      end if

      g = 981.0    ! acceleration of gravity, assuming acc units of cm/s^2

      arias_fctr = pi/(2.0*g)
      arias = arias_fctr * asqintsim
      arias_std = arias_fctr * asqintsim_std 
      ariasgm = arias_fctr * asqintsimgm
      ariasgm_fctr = arias_fctr * asqintsimgm_fctr 
      
      deallocate (acc, vel, dis, a_sq_int)
      deallocate (pga4avg, pgv4avg, pgd4avg, 
     :     asqint4avg, 
     :     psv4avg, dur75_5array, dur95_5array)

      return
      end
!----------------- END gm_td -----------------------------


!----------------- BEGIN acc_ts -----------------------------
      subroutine acc_ts(acc, nstart, nstop, npw2, te, ntaper)

! Uses time-domain stochastic model to compute an acceleration time series
!  (SMSIM = Stochastic Model Simulation) 

! Note: Because the intended use is to generate a series of accelerograms
!       for the same M, R, but different seeds, I have kept the
!       determination of nstart, nstop, npw2, and te (obtained via a call to 
!       Get_Npts_for_TD_Calcs) in the calling program (e.g., gm_td_drvr or a_ts_drvr).  It
!       might be more logical to include the determination of these variables
!       in this subroutine, but then the determination would have to be made 
!       for each simulation.

!       The calls in the calling program used to initialize the seed and to
!       determine the indices controlling the extent and duration of motion are:
!
!       iseed = -abs(seed)  ! initialize the seed (in gm_td), call only once if doing a
!                           ! suite of simulations; the value is passed through
!                           ! a common block contained in smsim.fi
!
!       call Get_Npts_for_TD_Calcs(nstart, nstop, npw2, te, ntaper)

! Note: Various input parameters are accessed through the common statements in
!       smsim.fi.   These parameters are obtained from calls such as this
!       (from td_drvr):
!
!       tdflag = .true.  ! controls extent of the input file read and written
!       call get_params( f_params, tdflag )
!       call write_params(nout, tdflag)
!       write(*, '(a)') ' dist: '
!       read(*, *) r
!       write(*, '(a)') ' amag: '
!       read(*, *) amag

! The various subroutines needed here are contained in td_subs.for and
! rvtdsubs.for

! Dates:  05/28/02 - Written by D. M. Boore (a much shortened version of
!                    smsim_td, only computing a single acceleration
!                    time series).  
!         08/08/02 - Disable removal of mean of noise
!         05/11/07 - Removed "\smsim\" from include statements
!         02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                    I caught this only when I compiled the program using the -chk
!                    switch.  This probably means that all of my time-domain runs
!                    up till now used ntaper = 0, even if a nonzero taper was 
!                    specified (the standard being 0.05).
!         12/04/09 - Change subroutine name Get_Npts to Get_Npts_for_TD_Calcs

      real acc(*)   ! allocate in calling program

! ALLOCATABLE: comment out following lines to disable dynamic allocation
      real work(:)
      allocatable :: work
! Remove comment character in lines below to disable dynamic allocation
!      real work(66000)
! ALLOCATABLE: end of group

      include 'smsim.fi'
      
! pi, twopi now computed in smsim.fi
!      pi = 4.0 * atan(1.0)
!      twopi = 2.0 * pi

! ALLOCATABLE: comment out following lines to disable dynamic allocation
      ndimen = npw2
      allocate (work(ndimen) )
! ALLOCATABLE: end of group

! Initialize arrays:
      do i = 1, ndimen
        acc(i)=0.
        work(i) = 0.
      end do

! Generate white noise with zero mean, unit variance, and proper distribution:

      if (iran_type .eq. 0) then
        do i = nstart, nstop
          work(i) = gasdev(iseed)
        end do
      else
        do i = nstart, nstop
          work(i) = ran1(iseed) - 0.5
        end do
      end if

      rmean = 0.0  ! no longer allow for removal of mean of noise--- see
                   ! readme.txt for the reason (it can cause long-period
                   ! distortions).

!      if (irmvdc .eq. 0) then
!        rmean = 0.0
!      else
!        call mean(work, nstart, nstop, rmean)
!      end if

! Window the white noise:

! NOTE: nstart, nstop comes from get_npts.for

      if(indxwind .eq. 0) then        ! BOX WINDOW
  
        do i = nstart, nstop
          work(i) = wind_box(i, nstart, nstop, ntaper) *
     :                  (work(i) - rmean)
        end do

      else                           ! EXPONENTIAL WINDOW

        do i = nstart, nstop
          t = float(i-nstart) * dt
          work(i) = wind_exp( t, te, eps_w, eta_w, new_mr) *
     :                 (work(i) - rmean)
        end do

      end if

! Transform to frequency domain:
      call realft(work,npw2,+1)

! Find sqrt of squared spectral amplitude for normalization:
      call avgsq_realft(work,npw2,avgsqamp) 
      sqrt_avgsqamp = sqrt(avgsqamp)

! Get frequency independent parameter:
      call const_am0_gsprd()     ! returns freq_indep_factor through common

      iaorins = 1             ! ground motion
      idva = 2                ! acceleration

      df = 1.0/(float(npw2)*dt)
      do i = 2,npw2/2
        f = (i-1) * df
        sfact = spect_amp(f)/ sqrt_avgsqamp
        work(2*i-1) = sfact * work(2*i-1)
        work(2*i)   = sfact * work(2*i)
      end do
      work(1) = 0.0              ! OK for acceleration spectrum, but not for
                                 ! displacement spectrum
      fnyq = 1.0/(2.0*dt)
      work(2) = spect_amp(fnyq) * work(2)/ sqrt_avgsqamp

! Transform back to time domain:
      call realft(work,npw2,-1)

! Apply FFT normalization:
      afact = 2 * df
      do i = 1, npw2
        acc(i) = afact * work(i)
      end do

! ALLOCATABLE: comment out following line to disable dynamic allocation
      deallocate (work)  
! ALLOCATABLE: end of group

      return
      end
!----------------- END acc_ts -----------------------------

!----------------- BEGIN BANNER -----------------------------
      subroutine banner(nu)
      
! Dates: 05/09/07 - Use a batch program (make_util_subs_file.bat)
!                   to make a file of forprogs utility files,
!                   and use include to bring the subroutines into this
!                   set of subroutines.  I do this to make the routines
!                   self contained.  It would be better to 
!                   use separate include statements, pointing to the
!                   forprogs folder, but people using the program
!                   won't have this folder available. But in order
!                   make sure that the util programs are up to date, I need
!                   to run the batch file above periodically.   The best
!                   way to do that would probably be to include a call to 
!                   that file in the batch files I use for compiling
!                   the smsim programs.

! Write version number to unit nu

      version = 6.0

      write(nu,*)
      write(nu,'(a,f6.3,a)') 
     :   '   ***************** SMSIM, Version ', 
     :    version,
     :   ' ************************'
      write(nu,*)

      return
      end
!----------------- BEGIN BANNER -----------------------------

!----------------- BEGIN GET_PARAMS -----------------------------
      subroutine get_params(f_params, tdflag)

! Dates: 06/07/95 - Written by D.M. Boore
!        10/17/95 - Added tdflag to control if more data need be read for
!                   the time series processing; also added irmvdc as an input
!                   parameter
!        11/14/95 - Read eps_int from data file
!        11/15/95 - changed input of spectral parameters
!        11/16/95 - Read amp_cutoff from data file and determine fup
!        11/27/95 - changed input of spectral parameters again
!        12/06/95 - Get low-cut filter parameters before rv and td params
!        12/07/95 - Get taper with window parameters
!        12/19/95 - Added zup to input parameters
!        02/28/97 - allow kappa to be magnitude dependent
!                   ( kappa = akappa + dkappadmag*(M-amagkref) )
!                   Use amagkref so that akappa is a reasonable value.  
!                   One reason to do so is that in
!                   this version of the program I determine fcut using
!                   a magnitude-independent value for kappa (akappa), and
!                   if the equation for kappa is given with amagkref = 0.0
!                   the value for akappa could well be less than 0.0.
!        01/18/99 - Added osc_crrctn to allow specification of which
!                   correction for oscillator response to make (Boore and Joyner
!                   or Liu and Pezeshk).
!        01/21/99 - Skip over reading rv params if tdflag = .true.
!        02/10/99 - Remove computation of npts for td sims; change name 
!                   "tsimdur" to "dur_fctr"
!        02/13/99 - Use get_lun, changed "fname" to "f", etc.
!        08/03/99 - Added parameter (iran_type) to control type of random 
!                   number (iran_type = 0 for normal, uniform otherwise)
!        06/05/00 - Added velocity parameter c_q to Q parameters
!        06/08/00 - Added r_ref to gsprd parameters and move definition of 
!                   const to const_am0_gsprd
!        01/27/01 - Added new time series window parameters
!        01/27/02 - Allow geom. spreading to be magnitude dependent
!        08/08/02 - Disabled use of remove-mean-from-noise parameter (irmvdc)
!        05/09/07 - Use skipcmnt to read in parameters, allowing up to 60 comment 
!                   lines (in particular, I can list all built-in sources).
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - New parameters and renamed parameters as a result of source 11
!        12/01/09 - Include version date at beginning of params file.  This
!                   is included as a way of checking if the proper params file is being
!                   used, in case the input parameters were changed. 
!                 - Trap for fmax = 0.0, in which case do not use fmax in computing 
!                   the diminution function  
!                 - Redo computation of fup, depending on the values of fmax and kappa
!                 - Read nslope rather than norder for the low-cut filter
!        12/02/09 - Remove computation of fup here, as it is now done in a subroutine
!                   in rv_subs.
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/16/09 - Read coefficients relating
!                   log10(f_ff) to M (this is motivated by Atkinson and Silva,
!                   2000, but may be a way of capturing the effect of a
!                   finite-fault without computing R_eff).  Set iflag_f_ff=1
!                   if want to compute f_ff (it does not work to set
!                   the coefficients = 0, because f_ff = 1 will then be
!                   computed.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/05/11 - Params file changed a bit, allowing the finite flag to have a value of 2
!                   to indicate rmod = r + f_ff.  
!        04/05/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!        04/08/11 - Trap for a source that does not use stress as a parameter, and 
!                   set stressc = 0.0 in that case.
!        05/16/11 - Write name of params file if it has the wrong date
!        09/16/11 - Change the error message produced if the wrong
!                   date is found.
!        09/17/11 - Read Boore and Thompson Trms coefficients from the params file
!        09/17/11 - Add read Boore and Thompson Trms coefficients for PGA and PGV
!                   from the params file
!        11/30/11 - Allow for a frequency-dependent geometrical spreading option
!                   (currently the Atkinson November 2011 spreading if option = 1 is
!                   chosen).
!        01/19/12 - Change params file so that allow for two frequency-dependent gsprds.
!        01/23/12 - Get params for the DMB gsprd(f) function
!        03/24/13 - Get params for the generalized additive two-corner source model
!        01/05/14 - Some of the text related to the geometrical spreading was changed, as a 
!                   result of the AB14 ENA atten paper.  The changes were first incorporated into the params
!                   files a few months ago, without changing the revision date of the params files, but I
!                   decide that it was better to change the date so that I am forced to change the text
!                   related to geometrical spreading.
!        07/23/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        07/28/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        10/08/14 - Add possibility of two lines for f_ff
!        12/18/14 - Add possibility of two lines plus a transition for f_ff.  
!                   and compute the coefficients of the transition function if used
!        12/11/15 - Add a high-cut filter

      character  ctl_cmmnts(200)*100
      character date_params_file_in*20, 
     :          date_params_file_correct*20
      character f_params*(*)
      logical tdflag, file_exist

      character wna_trms4osc_pars_file_name*200, 
     :          ena_trms4osc_pars_file_name*200
      common /trms4osc_files/wna_trms4osc_pars_file_name, 
     :                       ena_trms4osc_pars_file_name
     
      include 'bt12osc.fi'
      include 'smsim.fi'

                

      pi = 4.0 * atan(1.0)

      call trim_c(f_params, nc_f_params)
      call get_lun(nin)
      open(unit=nin, file=f_params(1:nc_f_params), status='unknown')
      
      date_params_file_correct = ' '
      date_params_file_correct = '12/11/15'
      call trim_c(date_params_file_correct,
     :   nc_date_params_file_correct)

! check date of parameters file:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      date_params_file_in = ' '
      read(nin,'(a)') date_params_file_in
      call trim_c(date_params_file_in,
     :   nc_date_params_file_in)
      
      if (date_params_file_correct(1:nc_date_params_file_correct)
     :    /= 
     :    date_params_file_in(1:nc_date_params_file_in)) then
        write(*,'(a)') 
     :    ' The program is stopping because you are using an '//
     :    'outdated *.params file.'
        write(*,'(a)') 
     :     ' The parameters file you specified ('//
     :       f_params(1:nc_f_params)//') has the date '//
     :        date_params_file_in(1:nc_date_params_file_in) 
        write(*,'(a)') '  The proper date is '//
     :   date_params_file_correct(1:nc_date_params_file_correct) 
        close(nin)
        stop
      end if      
      
      
! title:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, '(a)') title
 
! rho, beta, prtitn, fs:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) rho, beta, prtitn, rtp, fs

! source shape:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) numsource, pf_a, pd_a, pf_b, pd_b
      
! source scaling: (note: stress is now read in here; to write a driver
! that loops over stress, just assign the desired values of stress
! after calling get_params)
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) stressc, dlsdm, fbdfa, amagc,
     :             c1_fa, c2_fa, amagc4fa,
     :             c1_eps, c2_eps, amagc4eps
     
      if (numsource == 12) then
        if (c2_eps == 0.0) then
          mag4eps1 = -9.9
        else
          mag4eps1 = -c1_eps/c2_eps
        end if
      end if
 
      set stressc: SELECT CASE(numsource)
      CASE(3:10)
        stressc = 0.0
        dlsdm = 0.0
      END SELECT set stressc
      
        
!!
!!finite_fault factor specification:
!!  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0 if a finite-fault factor is not to be used)
!!
!!  Distance for point-source calculation
!!    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))
!!    If iflag_f_ff = 2: rps =  r + f_ff
!!   Use rps in the calculations (this variable is called rmod in the code; it should be changed to rps to
!!   reflect my current preferred terminology.  I do not have time to do this now).
!!  Specification of the finite-fault factor h:
!!    If nlines = 1
!!      log10(f_ff) = c1 + c2*amag  
!!    If nlines = 2
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh
!!      where Mh is determined by the intersection of the two lines
!!      (this is computed in the program)  
!!    If nlines = 3
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2
!!      log10(f_ff) given by a cubic in amag between the two lines (this
!!        produces a smooth transition over the magnitude range DeltaM
!!  *** NOTE: placeholders are needed for c3, c4, and DeltaM, even if not used.
!!
!!  Published finite-fault factors
!!    Author                      applicable_region meaning_of_r  iflag_f_ff nlines         c1      c2   c3    c4  
!!    Atkinson and Silva (2000)                 ACR        r_rup           1    1      -0.0500  0.1500  0.0   0.0
!!    Toro (2002)                               SCR        r_rup           2    1      -1.0506  0.2606  0.0   0.0
!!    Atkinson and Boore (2003)          subduction        r_rup           1    1      -2.1403  0.5070  0.0   0.0
!!    Yenier and Atkinson (2014)                ACR        r_rup           1    1      -1.7200  0.4300  0.0   0.0
!!    Yenier and Atkinson (2015)                ACR        r_rup           1    1      -0.4050  0.2350  0.0   0.0
!!    Yenier and Atkinson (2015),               SCR        r_rup           1    1      -0.5690  0.2350  0.0   0.0
!!    Boore               (2014)                ACR        r_rup           1    2      -1.7200  0.4300 -0.405 0.2350
!!  
!!  Suggested modification for stable continental regions
!!    Assuming that all of the above the above relations except Toro (2002) and Atkinson and Boore (2003)
!!    are for active crustal regions, and that h is proportional to fault radius, then -0.164 should be
!!    added to c1 (and c3 for Boore (2014) to adjust for the smaller fault size expected for stable continental region
!!    earthquakes (this adjustment factor uses radius ~ stress^-1/3, and a stress of 88 bars for ACR (from 
!!    my determination of what stress matches the Atkinson and Silva (2000) high-frequency spectral level--
!!    see What_SCF_stress_param_is_consistent_with_the_AS00_source_model.pdf in the daves notes page of
!!    www.daveboore.com) and 274 bars for SCR, from my inversion of 0.1 s and 0.2 s PSA values for 8 ENA
!!    earthquakes, using the Boatwright and Seekins (2011) attenuation model.  This determination is 
!!    part of ongoing work for the NGA-East project, and will appear in a PEER report in 2015.
!!   1    1      -0.0500  0.1500  0.0   0.0 0.0
!!   1    2      -1.7200  0.4300 -0.405 0.2350 0.0
!!   1    3      -1.7200  0.4300 -0.405 0.2350 1.0
!!   1    1      -1.7200  0.4300  0.0   0.0 0.0
!!   1    1      -0.4050  0.2350  0.0   0.0 0.0
!   0 0 0.0 0.0 0.0 0.0 0.0
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) iflag_f_ff, nlines_f_ff, 
     :                c1_log10_f_ff, c2_log10_f_ff,
     :                c3_log10_f_ff, c4_log10_f_ff,
     :                dm_f_ff
      if (iflag_f_ff == 0) then
        mh_f_ff = 0.0
      else
        if (nlines_f_ff == 1) then
          mh_f_ff = 99.9
        else
          mh_f_ff = (c3_log10_f_ff - c1_log10_f_ff)/
     :              (c2_log10_f_ff - c4_log10_f_ff)
        end if
      end if

      if (nlines_f_ff == 3) then
        m1t_f_ff = mh_f_ff - 0.5*dm_f_ff
        m2t_f_ff = mh_f_ff + 0.5*dm_f_ff
        c1mod = c1_log10_f_ff + c2_log10_f_ff*m1t_f_ff
        c3mod = c3_log10_f_ff + c4_log10_f_ff*m2t_f_ff
        delta_a = c3mod - c1mod
        delta_b = c4_log10_f_ff - c2_log10_f_ff
        
        c0t_f_ff = c1mod
        c1t_f_ff = c2_log10_f_ff
        c2t_f_ff = 2.0*delta_a/dm_f_ff**2.0 - 
     :                  0.5*(3.0*c2_log10_f_ff + c4_log10_f_ff)/dm_f_ff
        c3t_f_ff = 
     :         0.5*(delta_b - 2.0 *(delta_a/dm_f_ff - c2_log10_f_ff))/
     :                          dm_f_ff**2.0
!        c2t_f_ff = (delta_a - c2_log10_f_ff*dm_f_ff)/dm_f_ff**2.0 - 
!     :                        c3t_f_ff*dm_f_ff
!         c2t_f_ff = -0.0975
!         c3t_f_ff = -2.498E-16

!DEBUG
!        print *,' Inside get_params:'
!        print *,' delta_b, c1mod, c3mod, delta_a, '//
!     :          'c2_log10_f_ff, dm_f_ff = ',      
!     :       delta_b, c1mod, c3mod, delta_a, c2_log10_f_ff, dm_f_ff      
!        print *,'c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff=', 
!     :         c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff 
!DEBUG

      else
        m1t_f_ff = 0.0
        m2t_f_ff = 0.0
        c0t_f_ff = 0.0
        c1t_f_ff = 0.0
        c2t_f_ff = 0.0
        c3t_f_ff = 0.0
      end if        
        

! gsprd: 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) i_gsprd_option

      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) r1_dmb_gsprd, 
     :        pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :        ft1_dmb_gsprd, ft2_dmb_gsprd

      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) r_ref
      read(nin, *) nsprd_segs
      do i = 1, nsprd_segs
        read(nin, *) rlow(i), a_s(i), b_s(i), m_s(i)
      end do
 
! Q:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q

! source duration:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) w_fa, w_fb

!        06/01/11 - For source 1, set w_fb = 0.0 so that do not mistakenly compute a longer
!                   source duration than is desired (note that dursource = w_fa/fa + w_fb/fb, 
!                   so before this modification, using duration weights 1.0 1.0 would have
!                   given the wrong duration, since fb = fa in spect_scale).
!        03/25/13 - It is more logical to use weights of 0.5 and 0.5, even for source 1,
!                   since dursource is always = w_fa/fa + w_fb/fb (see function dursource), 
!                   and fb is set equal to 
!                   fa in spect_scale for source 1, and in addition, I now recommend that 
!                   weights of 0.5 and 0.5 be used for the other sources.

!      if (numsource == 1) then
!        w_fb = 0.0
!      end if
      

! path duration:              
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) nknots
      do i = 1, nknots
        read(nin, *) rdur(i), dur(i)
      end do
      read(nin, *) slast

! site amps:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) namps
      do i = 1, namps
        read(nin, *) famp(i), amp(i)
      end do

! site diminution:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fm, akappa, dkappadmag, amagkref

! low-cut filter parameters:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fcut, nslope_in
! Force nslope = 4, 8, 12, etc for acausal filters:
      if (nslope_in .lt. 4) then
        nslope = 4
        write(*,'(a)') ' !!! nslope < 4; resetting it to 4'
      else
        nslope = 4 * int(float(nslope_in)/4.0)
        if (nslope .ne. nslope_in) then
          write(*,'(a, 1x,i2)') 
     :    ' !!! nslope not 4, 8, 12, etc; resetting to ', nslope
        end if
      end if
      norder = nslope/2 ! for an acausal (2 passes of an norder Butterworth,
                        ! see Dave's notes titled: "dependence_of_fourier_amplitude_spectra_at_low_frequencies_on_zero_pads.pdf"
                        ! and "properties_of_butterworth_filters.pdf"

! high-cut filter parameters: itype_hcfilt (0=no high-cut filter [but the other parameters are 
!  needed as placeholders], 1=raised half cycle of cosine; 2= quarter cycle of a cosine), 
!  fhc1, fhc2 (low and high frequency limits of filter), eta_hcfilt (power of cosine)
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) itype_hcfilt, fhc1, fhc2, eta_hcfilt
      if (itype_hcfilt /= 0 .and. 
     :    itype_hcfilt /= 1 .and. 
     :    itype_hcfilt /= 2) then 
        write(*,'(a)') ' ERROR: itype_hcfilt /= 0, 1, or 2; QUITTING!!'
        stop
      end if

! parameters for rv integration (read even if tdflag == .T., but values are not used):
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) zup, eps_int, amp_cutoff, osc_crrctn
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      wna_trms4osc_pars_file_name = ' '
      read(nin, '(a)') wna_trms4osc_pars_file_name
      call trim_c(wna_trms4osc_pars_file_name, 
     :         nc_wna_trms4osc_pars_file_name)
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      ena_trms4osc_pars_file_name = ' '
      read(nin, '(a)') ena_trms4osc_pars_file_name
      call trim_c(ena_trms4osc_pars_file_name, 
     :         nc_ena_trms4osc_pars_file_name)
 
! Open files and store coefficients if not TDFLAG:

      if ( .not. tdflag .and. 
     :    (osc_crrctn >=3 .and. osc_crrctn<=5) ) then
        file_exist = .false.
        inquire(file=
     :  wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name), 
     :        exist=file_exist)
        IF (.not. file_exist) then
          write(*,'(a)') 
     :    ' ******* WNA Trms4Osc Coefficient file '//
     : wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name)//
     :    ' DOES NOT EXIST, QUIT!!!!!!!'
          stop
        END IF

        call get_lun(nu_pars)
        open(nu_pars,file=
     :   wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name), 
     :   status = 'unknown')
                         
        read(nu_pars,*)
        read(nu_pars,*)
        read(nu_pars,*) nm4bt12wna, nr4bt12wna
        read(nu_pars,*)
        do i = 1, nr4bt12wna
          do j = 1, nm4bt12wna
          
            read (nu_pars,*) m4bt12wna(j), dum_r, 
     :         c1bt12wna(j,i), c2bt12wna(j,i), c3bt12wna(j,i), 
     :         c4bt12wna(j,i), c5bt12wna(j,i), c6bt12wna(j,i),
     :         c7bt12wna(j,i),
     :         td_rv_pga_wna(j,i), td_rv_pgv_wna(j,i)
     
            logr4bt12wna(i) = alog10(dum_r)
            
          end do
        end do
      
        close(nu_pars)        
        
!DEBUG
!        call get_lun(nu_pars)
!        open(nu_pars,file='debug_wna_drms.out',
!     :   status = 'unknown')
!                         
!        write(nu_pars,*)
!        write(nu_pars,*)
!        write(nu_pars,'(i2, 2x,i2)') nm4bt12wna, nr4bt12wna
!        write(nu_pars,*)
!        do i = 1, nr4bt12wna
!          do j = 1, nm4bt12wna
!          
!            write (nu_pars,'(1x,f3.1, 1x,f7.2, 9(1x,es11.4))') 
!     :         m4bt12wna(j), 10.0**logr4bt12wna(i), 
!     :         c1bt12wna(j,i), c2bt12wna(j,i), c3bt12wna(j,i), 
!     :         c4bt12wna(j,i), c5bt12wna(j,i), c6bt12wna(j,i),
!     :         c7bt12wna(j,i),
!     :         td_rv_pga_wna(j,i), td_rv_pgv_wna(j,i)
!            
!          end do
!        end do
!      
!        close(nu_pars)         
!DEBUG  

        file_exist = .false.
        inquire(file=
     :  ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name), 
     :        exist=file_exist)
        IF (.not. file_exist) then
          write(*,'(a)') 
     :    ' ******* ENA Trms4Osc Coefficient file '//
     : ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name)//
     :    ' DOES NOT EXIST, QUIT!!!!!!!'
          stop
        END IF
        
        call get_lun(nu_pars)
        open(nu_pars,file=
     :   ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name), 
     :   status = 'unknown')
                         
        read(nu_pars,*)
        read(nu_pars,*)
        read(nu_pars,*) nm4bt12ena, nr4bt12ena
        read(nu_pars,*)
        do i = 1, nr4bt12ena
          do j = 1, nm4bt12ena
          
            read (nu_pars,*) m4bt12ena(j), dum_r, 
     :         c1bt12ena(j,i), c2bt12ena(j,i), c3bt12ena(j,i), 
     :         c4bt12ena(j,i), c5bt12ena(j,i), c6bt12ena(j,i),
     :         c7bt12ena(j,i),
     :         td_rv_pga_ena(j,i), td_rv_pgv_ena(j,i)
     
            logr4bt12ena(i) = alog10(dum_r)

!DEBUG
!       print *,' Inside rv_td_subs, '//
!     :       'm4bt12ena(j), dum_r, logr4bt12ena(i) =' 
!       print *, 
!     :       m4bt12ena(j), dum_r, logr4bt12ena(i) 
!DEBUG
 
          end do
        end do
      
        close(nu_pars)
        
!DEBUG
!        call get_lun(nu_pars)
!        open(nu_pars,file='debug_ena_drms.out',
!     :   status = 'unknown')
!                         
!        write(nu_pars,*)
!        write(nu_pars,*)
!        write(nu_pars,'(i2, 2x,i2)') nm4bt12ena, nr4bt12ena
!        write(nu_pars,*)
!        do i = 1, nr4bt12ena
!          do j = 1, nm4bt12ena
!          
!            write (nu_pars,'(1x,f3.1, 1x,f7.2, 9(1x,es11.4))') 
!     :         m4bt12ena(j), 10.0**logr4bt12ena(i), 
!     :         c1bt12ena(j,i), c2bt12ena(j,i), c3bt12ena(j,i), 
!     :         c4bt12ena(j,i), c5bt12ena(j,i), c6bt12ena(j,i),
!     :         c7bt12ena(j,i),
!     :         td_rv_pga_ena(j,i), td_rv_pgv_ena(j,i)
!     
!          end do
!        end do
!      
!        close(nu_pars)         
!DEBUG  
        
      end if
 
 
      if (tdflag) then

! Read more if time domain method:

! window parameters:
        call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
        read(nin, *) indxwind, taper, eps_w, eta_w, f_tb2te, f_te_xtnd

! timing stuff:
        call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
        read(nin, *) dur_fctr, dt, tshift, seed, nsims, iran_type

! Flag controlling the removal of the dc from the random series:
!      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
!      read(nin, *) irmvdc
!
!      irmvdc = 0            ! force this value, so mean will not be removed
!

      end if 
      
      close(unit=nin)

      return
      end
!----------------- END GET_PARAMS -----------------------------

!----------------- BEGIN WRITE_PARAMS -----------------------------
      subroutine write_params(nout, tdflag)

! Dates: 08/18/95 - Written by D. Boore
!        10/17/95 - Added tdflag to control if more data need be read for
!                   the time series processing; also added irmvdc as an input
!                   parameter
!        11/14/95 - Write eps_int
!        11/16/95 - Write amp_cutoff, fup
!        12/06/95 - Write low-cut filter params before rv and td params
!        12/07/95 - Write taper with window parameters
!        12/19/95 - Added zup to input parameters
!        01/18/99 - Added osc_crrctn to allow specification of which
!                   correction for oscillator response to make (Boore and Joyner
!                   or Liu and Pezeshk).
!        08/03/99 - Added parameter (iran_type) to control type of random 
!                   number (iran_type = 0 for normal, uniform otherwise)
!        06/05/00 - Added velocity parameter c_q to Q parameters
!        01/27/01 - Added new time series window parameters
!        01/27/02 - Allow geom. spreading to be magnitude dependent
!        02/06/03 _ Use formatted write statements
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - New parameters and renamed parameters as a result of source 11
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/16/09 - Write coefficients relating log10(f_ff) to M.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/05/11 - Params file changed a bit, allowing the finite flag to have a value of 2
!                   to indicate rmod = r + f_ff.  I also replace "h_eff" with "f_ff" (because
!                   the finite-fault factor is not really a pseudo-depth).
!        09/11/11 - Add Trms4osc coefficient file names to argument list, and write them
!        01/23/12 - Write params for the gsprd(f) options
!        03/24/13 - Add source 12 (generalized additive 2-corner source model)
!        03/24/13 - Write params for the generalized additive two-corner source model
!        07/23/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        12/18/14 - Write parameters for possible transition equation for f_ff
!        12/11/15 - Add a high-cut filter

                
      character wna_trms4osc_pars_file_name*200, 
     :          ena_trms4osc_pars_file_name*200
      common /trms4osc_files/wna_trms4osc_pars_file_name, 
     :                       ena_trms4osc_pars_file_name
     
      logical tdflag

      include 'smsim.fi'

      write( nout, '(a)') ' Title:'
      write( nout, '(4x,a)') title

      write( nout, '(a)') ' rho, beta, prtitn, rtp, fs:'
      write( nout, '(2x,f5.2, 1x,f5.2, 1x,f5.3, 1x,f4.2, 1x,f4.2)')   
     :       rho, beta, prtitn, rtp, fs

      write( nout, '(a/a/a/a/a/a)') 
     : ' spectral shape: source number, pf_a, pd_a, pf_b, pd_b',
     :     '(1=Single Corner;2=Joyner;'//
     :      '3=A93;...;8=Jena;9=AS00;10=A05;'//
     :     '11=Generalized multiplicative 2-corner; '//
     :     '12=Generalized additive 2-corner)',
     : ' pf, pd (1-corner spectrum = '//
     :      '1/(1+(f/fc)**pf)**pd; 0.0 otherwise)',
     : ' (usual model: pf=2.0,pd=1.0; Butterworth: pf=4.0,pd=0.5)',
     : ' (Note: power of high freq decay --> pf*pd)',
     : ' NOTE: see a params file for more complete description '//
     :      'of parameters'
      write( nout, '(2x,i2, 4(1x,f4.2))' ) 
     :   numsource, pf_a, pd_a,  pf_b, pd_b

      write( nout, '(a/a/a/a/a)') 
     : ' spectral scaling: stressc, dlsdm, fbdfa, amagc'// 
     :   ' c1_fa, c2_fa, amagc4fa,'//
     :   ' c1_eps, c2_eps, amagc4eps, mag4eps1',
     : ' (stress=stressc*10.0**(dlsdm*(amag-amagc))',
     : ' (fbdfa, amagc for Joyner model, usually 4.0, 7.0)',
     : ' (not used for srce 3, but placeholders still needed)',
     : ' NOTE: see ofr.params for more complete description '//
     :      'of parameters'
      write( nout, '(2x,f7.2, 1x,es10.3, 1x,f5.2, 1x,f4.2,
     :     2(1x,es10.3), 1x,f5.2,
     :     2(1x,es10.3), 1x,f5.2, f5.2
     :                                       )') 
     :     stressc, dlsdm, fbdfa, amagc,
     :     c1_fa, c2_fa, amagc4fa,
     :     c1_eps, c2_eps, amagc4eps, mag4eps1

      write( nout, '(
     :       a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a
     :                                    )') 
     : ' !',
     : ' !finite_fault factor specification:',
     : ' !  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0'//
     :                 ' if a finite-fault factor is not to be used)',
     : ' !',
     : ' !  Distance for point-source calculation',
     : ' !    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))',
     : ' !    If iflag_f_ff = 2: rps =  r + f_ff',
     : ' !   Use rps in the calculations (this variable is called '//
     :           'rmod in the code; it should be changed to rps to',
     : ' !   reflect my current preferred terminology.  '//
     :           'I do not have time to do this now).',
     : ' !  Specification of the finite-fault factor h:',
     : ' !    If nlines = 1',
     : ' !      log10(f_ff) = c1 + c2*amag',  
     : ' !    If nlines = 2',
     : ' !      log10(f_ff) = c1 + c2*amag  for amag<Mh',
     : ' !      log10(f_ff) = c3 + c4*amag  for amag>=Mh',
     : ' !      where Mh is determined by the intersection of '//
     :                         'the two lines',
     : ' !      (this is computed in the program)',  
     : ' !    If nlines = 3',
     : ' !      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2',
     : ' !      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2',
     : ' !      log10(f_ff) given by a cubic in amag between '//
     :                'the two lines (this',
     : ' !        produces a smooth transition over the '//
     :                'magnitude range DeltaM'


!      write( nout, '(a/a/a/a/a/a/a/a/a/a)') 
!     : ' !!iflag_f_ff, nlines, c1, c2, c3, c4 (0 0 0 0 0 0 if '//
!     :                                               'not used)',
!     : ' !  If iflag_f_ff = 1:',
!     : ' !    modified distance: rmod = sqrt(r^2 + f_ff^2))',
!     : ' !  If iflag_f_ff = 2:',
!     : ' !    modified distance: rmod =  r + f_ff',
!     : ' !  where log10(f_ff) = c1 + c2*amag for nlines=1, '//
!     :                                         'and for nlines=2',
!     : ' !  the two lines c1+c2*amag for amag<Mh and c3+c4*amag '//
!     :                                     'for amag>=Mh, where Mh is', 
!     : ' !  determined by the intersection of the two lines '//
!     :                           '(this is computed in the program)',  
!     : ' !  *** NOTE: placeholders are needed for c3 and c4, '//
!     :                           'even if not used.',
!     : ' !  Use rmod in the calculations'
!      
      write(nout,*) iflag_f_ff, nlines_f_ff,
     :                c1_log10_f_ff, c2_log10_f_ff, 
     :                c3_log10_f_ff, c4_log10_f_ff, dm_f_ff
      write(nout, '(4x,a, 3(1x,f5.2))') 
     :              ' mh_f_ff, m1t_f_ff, m2t_f_ff = ', 
     :                mh_f_ff, m1t_f_ff, m2t_f_ff
     
      write(nout, '(4x,a, 4(1x,es10.3))') 
     :      ' c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff = ', 
     :        c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff

      write( nout, '(1x,a, 1x,i1)') 'i_gsprd_option = ', i_gsprd_option
      write( nout, '(1x,a)') ' r1_dmb_gsprd,'//
     :        ' pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1,'// 
     :        ' ft1_dmb_gsprd, ft2_dmb_gsprd = '
      write( nout, '(1x,f6.2, 
     :               1x,f5.2, 1x,f5.2, 1x,f5.2, 
     :               1x,f5.2, 1x,f5.2)')  
     :          r1_dmb_gsprd, 
     :          pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1,
     :          ft1_dmb_gsprd, ft2_dmb_gsprd

      write( nout, '(2a)') 
     :  ' gsprd: r_ref, nsegs, (rlow(i), a_s, b_s, m_s(i))',
     :                     '  (Usually set r_ref = 1.0 km)'
      write( nout, '(2x,f6.2)' ) r_ref
      write( nout, '(2x,i3)' ) nsprd_segs
      do i = 1, nsprd_segs
        write( nout, '(2x,f7.2, 1x,es10.3, 1x,es10.3, 1x,f4.2)') 
     :    rlow(i), a_s(i), b_s(i), m_s(i)
      end do

      write( nout, '(a)') 
     :   ' q: fr1, Qr1, s1, ft1, ft2, fr2, qr2, s2, c_q'
      write( nout, '(2x,f7.3, 1x,f7.2, 1x,f6.3, 3(1x,f7.3),
     :     1x, f7.2, 1x, f6.3, 1x,f4.2)' ) 
     :       fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q

      write( nout, '(a)') ' source duration: weights of 1/fa, 1/fb'
      write( nout, '(2x,f4.2, 1x, f4.2)' ) w_fa, w_fb

      write( nout, '(2a)') ' path duration: nknots, (rdur(i), dur(i),',
     :                     ' slope of last segment'
      write( nout, '(2x,i3)' ) nknots
      do i = 1, nknots
        write( nout, '(2x,f6.1, 1x,f6.2)' ) rdur(i), dur(i)
      end do
      write( nout, '(2x,es10.3)' ) slast

      write( nout, '(2a)') ' site amplification: namps, (famp(i), ',
     :                     'amp(i))'
      write( nout, '(2x,i3)' ) namps
      do i = 1, namps
        write( nout, '(2x,f7.3, 1x,f6.3)' ) famp(i), amp(i)
      end do

      write( nout, '(a)') 
     :  ' site diminution parameters: fm, akappa, dkappadmag, amagkref'
      write( nout, '(2x,f7.3, 2(1x,es10.3), 1x,f4.2)' ) 
     :    fm, akappa, dkappadmag, amagkref

      write( nout, '(a)') ' low-cut filter parameters: fcut, nslope'
      write( nout, '(2x,f7.3, 1x,i2)' ) fcut, 2*norder

! high-cut filter parameters: itype_hcfilt (0=no high-cut filter (but the other parameters are 
!  needed as placeholders), 1=raised half cycle of cosine; 2= quarter cycle of a cosine), 
!  fhc1, fhc2 (low and high frequency limits of filter), eta_hcfilt (power of cosine)
      write( nout, '(a)') ' high-cut filter parameters: '
      write( nout, '(a)') '   itype_hcfilt'
      write( nout, '(a)') '     0=no high-cut filter (but the other'//
     :                         ' parameters are needed as placeholders)'
      write( nout, '(a)') '     1=raised half cycle of cosine'
      write( nout, '(a)') '     2= quarter cycle of a cosine' 
      write( nout, '(a)') '     fhc1, fhc2 (low and high frequency'
      write( nout, '(a)') '     eta_hcfilt (power of cosine)' 
      write( nout, *) itype_hcfilt, fhc1, fhc2, eta_hcfilt

      if (.not. tdflag) then
        write( nout, '(a)') 
     :  ' parameters for rv calcs: zup, eps_int, amp_cutoff, osc_crrctn'
        write( nout, '(2x,f6.2, 2(1x,es10.3), 1x, i2)' ) 
     :        zup, eps_int, amp_cutoff, osc_crrctn
        if (osc_crrctn >=3 .and. osc_crrctn<=5) then        
          call trim_c(wna_trms4osc_pars_file_name, 
     :        nc_wna_trms4osc_pars_file_name)
          write( nout, '(a)') ' WNA Trms4osc coefficients from file '//
     :    wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name)
          call trim_c(ena_trms4osc_pars_file_name, 
     :        nc_ena_trms4osc_pars_file_name)
          write( nout, '(a)') ' ENA Trms4osc coefficients from file '//
     :    ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name)
        end if
        return
      end if

! Write more if time domain method:

      write( nout, '(2a)') 
     :  ' window params: indxwind(0=box,1=exp), ',
     :  ' taper, eps_w, eta_w, f_tb2te, f_te_xtnd'
      write( nout, '(2x,i1, 1x,f4.2, 1x,f5.2, 1x,f6.3, 1x,f4.1, 
     :               1x, f4.1)' ) 
     :    indxwind, taper, eps_w, eta_w,f_tb2te,f_te_xtnd

      write( nout, '(a)') 
     :  ' timing stuff: dur_fctr, dt, tshift, seed, nsims, iran_type'
      write( nout, '(2x,f4.2, 1x,f6.4, 1x,f6.2, 1x,f6.1, 
     :               1x,i4, 1x,i1)' ) 
     :    dur_fctr, dt, tshift, seed, nsims, iran_type

!      write( nout, '(2a/2a)') 
!     :         ' parameter to control whether dc is',
!     :         ' removed from random series before',
!     :         ' transformation to frequency domain',
!     :         ' is no longer used'
!      write( nout, *) irmvdc

      return
      end
!----------------- END WRITE_PARAMS -----------------------------
  
!----------------- BEGIN SPECT_AMP -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/16/05 - Replace computed go to with if then else
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        03/20/09 - Add instrument type 3 (wwssn short-period)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        01/22/10 - Add more instrument types and changed code to make it 
!                   clearer where the displacement, velocity, or
!                   acceleration response is being computed.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        11/29/11 - Add possibility that gspread is frequency dependent, 
!                   as controlled by i_gsprd_option.
!        02/11/15 - Changed name of "site_amp_factor" to "crustal_amp_factor"
!                 - Added interp_type to argument list, and specified
!                   inter_type = 2 (lin f, log A), based on work done on 02/10/15 in
!                   folder C:\smsim\interpolation_log_or_linear, as described in 
!                   "How should tabulated values of crustal amps be interpolated.v01.docx" in
!                   that folder.
!        12/11/15 - Add a high-cut filter
 
      function spect_amp(f)
      
      real h, pgd_pgv_pga_factor, spect_amp_displacement, spect_amp
      complex sp(20)

      include 'smsim.fi'
      
! NOTE: freq_indep_factor comes from calls to subroutine const_am0_gsprd within programs such as
! gm_rv in rv_subs.for, acc_ts, and SMSIMFAS.  The value of freq_indep_factor is passed
! through the common block /const_params/ in smsim.fi.  

      interp_type = 2 ! lin f, log A
      spect_amp_displacement =   freq_indep_factor * 
     :      buttrlcf(f, fcut, norder) * 
     :      high_cut_filter(f) * 
     :      spect_shape(f, fa, fb, pf_a, pd_a,  pf_b, pd_b, 
     :      am0b_m0, numsource) * 
     :      crustal_amp_factor(f, namps, famp, amp, interp_type) *
     :      dimin(f)
                                 ! Could save some multiplications
                                 ! by removing freq_indep_factor outside the
                                 ! function, but at the cost of possible
                                 ! confusion.
                                 ! freq_indep_factor from subroutine const_am0_gsprd
                                 
! If i_gsprd_option /= 0, then a frequency-dependent geometrical spreading is implied, and
! this must be called from within this subroutine (spect_amp).
      if (i_gsprd_option /= 0) then
        spect_amp_displacement = 
     :    gsprd_freq(f, rmod, r_ref, i_gsprd_option,
     :      r1_dmb_gsprd, 
     :      pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :      ft1_dmb_gsprd, ft2_dmb_gsprd)
     :                                    * spect_amp_displacement
      end if
      
! NOTE: When I was first revising the program to include the possibility of a frequency-dependent
! gsprd, I modified function gsprd to return either the frequency independent or dependent functions,
! and I removed the call to gsprd from const_am0_gsprd.  The problem with this, however, is that if gsprd 
! is independent of frequency, as it usually is, it would be called unnecessarily nfreq times.

!DEBUG
!      write(*,*) ' In spect_amp: freq_indep_factor = ', 
!     :                           freq_indep_factor
!      write(*,*) ' In spect_amp: buttrlcf = ', 
!     :                           buttrlcf(f, fcut, norder)
!      write(*,*) ' In spect_amp: spect_shape = ', 
!     :     spect_shape(f, fa, fb, pf_a, pd_a,  pf_b, pd_b, 
!     :      am0b_m0, numsource)
!      write(*,*) ' In spect_amp: crustal_amp_factor  = ', 
!     :     crustal_amp_factor(f, namps, famp, amp)
!      write(*,*) ' In spect_amp: dimin(f)  = ', 
!     :     dimin(f)
!DEBUG
                                 
      IF (iaorins == 1) THEN
      
        h=1                                      ! no instrument response
        
      ELSE IF (iaorins == 2) THEN              ! psv
                                                 !   converts from displacement to velocity response
        v = twopi * fosc                         
        h = v * harmoscf( f, fosc, damp, idva )
        
      ELSE IF (iaorins == 3) THEN   ! Wood-Anderson from poles and zeros
! gain from Uhrhammer, R.A. and E.R. Collins (1990). Synthesis of Wood-
! Anderson seismograms from broadband digital records, \bssa {\bf 80},
! 702--716. (V = 2080$, T_0 = 0.8 (f_0 = 1/0.8 = 1.25), and \eta = 0.69).
!
! poles and zeros from above values of f_o and eta, using these relations:
!       2*pi*f_0 = sqrt(sp(1)**2 + sp(2)**2)
!       eta = abs(sp(1))/(2*pi*f_0)

        idva = 0
      
        gain = 2080.0
        
        if (idva == 2) then
          fgain = 0.01 
        else if (idva == 0) then
          fgain = 100.0
        else
          write(*,*) ' Invalid value of idva in spect_amp, QUITTING!'
          stop
        end if
        
        nz = 2
        np = 2
        sp(1) = cmplx( -5.41925, -5.68479)  
        sp(2) = cmplx( -5.41925, +5.68479)  
!        sp(1) = cmplx( -5.49779, -5.60886) ! standard_tab.doc (provided by
!        sp(2) = cmplx( -5.49779, +5.60886) ! Jim Dewey) containing notes from 
                                            ! Charles R. Hutt, 1 December 2006
! Determine factor to give proper gain 
        dum = 1.0
        call poles_zeros_response(fgain, dum, nz, np, sp, idva, 
     :                              ampnorm, phase)
        gainfactor = gain/ampnorm

        call poles_zeros_response(f, gainfactor, nz, np, sp, idva, 
     :                              amp_response, phase_response)
        h = amp_response

!DEBUG
!        if (abs(f/1.25-1) < 0.01) then
!          write(*,*) ' For WA: f, h = ', f, h
!        end if
!DEBUG
        
      ELSE IF (iaorins == 4) THEN   ! WWSSN-SP from poles and zeros
! values from standard_tab.doc (provided by
! Jim Dewey) containing notes from 
! Charles R. Hutt, 1 December 2006

        idva = 0
      
        gain = 1.0
        fgain = 1.0
        nz = 3
        np = 5
        sp(1) = cmplx( -3.72500, -6.22000)
        sp(2) = cmplx( -3.72500, +6.22000)
        sp(3) = cmplx( -5.61200, +0.00000)
        sp(4) = cmplx(-13.24000, +0.00000)
        sp(5) = cmplx(-21.08000, +0.00000)
                                             
! Determine factor to give proper gain 
        dum = 1.0
        call poles_zeros_response(fgain, dum, nz, np, sp, idva, 
     :                              ampnorm, phase)
        gainfactor = gain/ampnorm

        call poles_zeros_response(f, gainfactor, nz, np, sp, idva, 
     :                              amp_response, phase_response)
        h = amp_response
        

!DEBUG
!        if (abs(f/1.0-1) < 0.01) then
!          write(*,*) ' For WWSSN-SP, P&Z: f, h = ', f, h
!        end if
!DEBUG
        
!      ELSE IF (iaorins == 5) THEN   ! WWSSN short period
!        pp = 1.0
!        pg = 0.75
!        s = 0.0
!        hp = 0.49
!        hg = 1.00
!        g_spinsf = 17.1
!        h= g_spinsf * spinsf (f,pp,pg,s,hp,hg)
!
!*DEBUG
!        if (abs(f/1.0-1) < 0.01) then
!          write(*,*) ' For WWSSN-SP, Spinsf: f, h = ', f, h
!        end if
!*DEBUG
        
      ELSE
! for customized response (network instruments, etc)
        h=1
      END IF

      pgd_pgv_pga_factor = (twopi*f)**float(idva)
       
      spect_amp = h * pgd_pgv_pga_factor * spect_amp_displacement   ! spect_amp contains the spectral amplitude.

      return
      end
!----------------- END SPECT_AMP -----------------------------


!----------------- BEGIN CONST_AM0_GSPRD -----------------------------
! Dates: 11/14/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments in gsprd
!        06/08/00 - Moved computation of const into this subroutine
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/17/09 - Bring in rmod as a calling argument
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/08/11 - I removed numsource from the argument list of gsprd because the
!                   modification for the AS00 source referred to in the 
!                   07/02/99 modification is no longer used, as rmod is 
!                   calculated elsewhere and passed to gsprd through a common
!                   block in smsim.fi.
!        11/30/11 - Allow for the option of a frequency dependent geometrical spreading
!                   (i_gsprd_option /= 0).
!        10/28/14 - Changed call to the function gsprd to the subroutine gsprd_sub in order to
!                   obtain slope, gsprd in smsim.fi, so can print the values if desired (as in
!                   tmrs_loop_rv_drvr) in order to check the values (in particular, the M-dependent
!                   gsprd of Silva used in the NGA-East simulations for the 29-30 Oct. 2014 workshop).

      subroutine const_am0_gsprd()
      
! Note: This subroutine is called from SMSIMFAS      

      include 'smsim.fi'

! Define constant, for r=r_ref(km).Usually set r_ref = 1.0 km.  Be careful
! if a different value or different units are used.  In particular, using 
! different units will require a change in the scaling factor of 1.e-20 below

      const=prtitn*rtp*fs*(1.e-20)/(4.*pi*rho*beta**3*r_ref)

      if (i_gsprd_option == 0) then
        call gsprd_sub(rmod, r_ref, nsprd_segs, rlow, a_s, b_s, m_s, 
     :        amag, slope, gsprd)
      
        freq_indep_factor = const*am0*gsprd
      else
        gsprd = 1.0
        slope = 0.0
        freq_indep_factor = const*am0  ! Note: gsprd_freq is called in spect_amp
                                       ! if i_gsprd_option /= 0.  Separating 
                                       ! freq_indep_factor from gspread in this case
                                       ! avoids multiple calls (for each frequency) to the 
                                       ! frequency-independent computations in this
                                       ! subroutine.
      end if
      
!                         (am0 from Spect_Scale) 

!DEBUG
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: rmod, r_ref, nsprd_segs'
!      write(*,*)  rmod, r_ref, nsprd_segs
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: numsource, amag = ' 
!      write(*,*)  numsource, amag 
!      write(*,*)
!      do i = 1, nsprd_segs
!        write(*,'(4(1x,es10.3))') rlow(i), a_s(i), b_s(i), m_s(i)
!      end do        
!
!       temp_gsprd = gsprd(rmod, r_ref, nsprd_segs, rlow, a_s, b_s, m_s, 
!     :        numsource, amag)
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: const, am0, gsprd = ', 
!     :             const, am0, temp_gsprd 
!      write(*,*)
!DEBUG

      return
      end
!----------------- END CONST_AM0_GSPRD -----------------------------

!----------------- BEGIN GSPRD_FUNC -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments
!        06/05/00 - Added some explanation of r
!        06/08/00 - Make gsprd nondimensional through the use of r_ref, which 
!                   now appears in the definition of variable const
!                   in const_am0_gsprd
!        01/27/02 - Following Silva, parameters added to allow magnitude
!                   dependent slope (to capture finite fault effects)
!        11/27/05 - Remove deff for Atkinson (2005) source
!        04/24/07 - Put "rmod = r" in the if statement
!        12/17/09 - Bring in rmod as a calling argument.  numsource is no longer
!                   needed as a calling argument.
!        04/08/11 - I removed numsource from the argument list of gsprd because the
!                   modification for the AS00 source referred to in the 
!                   07/02/99 modification is no longer used, as rmod is 
!                   calculated elsewhere and passed to gsprd through a common
!                   block in smsim.fi.
!        10/28/14 - Changed name to gsprd_func (becauase I will also create a new routine
!                   gsprd_sub based on this one that returns slope and gsprd--see change log
!                   in CONST_AM0_GSPRD

      function gsprd_func(rmod, r_ref, nsprd_segs,  rlow, a_s, b_s, m_s, 
     :               amag)
      real r_ref, rlow(*), a_s(*), b_s(*), m_s(*), geff(10)
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      
!      if (numsource .eq. 9) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if
      
      geff(1) = r_ref/rlow(1)  ! usually set r_ref = 1.0 km.  Be careful
                               ! if a different value or different units are
                               ! used.  In particular, using different units
                               ! will require a change in the scaling factor
                               ! of 1.e-20 used in the definition of const in
                               ! const_am0_gsprd

      do i = 2, nsprd_segs
        slope = a_s(i-1) + b_s(i-1)*(amag - m_s(i-1))
        geff(i) = geff(i-1)*(rlow(i)/rlow(i-1))**slope
      end do
      if (rmod <= rlow(1)) then
        j = 1
      else if (rmod >= rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd_func = (geff(j)) * (rmod/rlow(j))**slope

      return
      end
!----------------- END GSPRD_FUNC -----------------------------

!----------------- BEGIN GSPRD_SUB -----------------------------
! Dates: 10/28/14 - Written by D.M. Boore, based on gsprd_func
      subroutine gsprd_sub(rmod, r_ref, nsprd_segs,  rlow, 
     :               a_s, b_s, m_s, 
     :               amag, slope, gsprd)
      real r_ref, rlow(*), a_s(*), b_s(*), m_s(*), geff(10)
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      
!      if (numsource .eq. 9) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if
      
      geff(1) = r_ref/rlow(1)  ! usually set r_ref = 1.0 km.  Be careful
                               ! if a different value or different units are
                               ! used.  In particular, using different units
                               ! will require a change in the scaling factor
                               ! of 1.e-20 used in the definition of const in
                               ! const_am0_gsprd

      do i = 2, nsprd_segs
        slope = a_s(i-1) + b_s(i-1)*(amag - m_s(i-1))
        geff(i) = geff(i-1)*(rlow(i)/rlow(i-1))**slope
      end do
      if (rmod <= rlow(1)) then
        j = 1
      else if (rmod >= rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd = (geff(j)) * (rmod/rlow(j))**slope

      return
      end
!----------------- END GSPRD_SUB -----------------------------

!----------------- BEGIN GSPRD_FREQ -----------------------------
      FUNCTION gsprd_freq(f, r, r_ref, i_gsprd_option,
     :    r1, 
     :    pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :    ft1, ft2)
      
! NOTE: r_ref not used in this version, but it is included as an argument
! in case it is used in a later version.
      
! Dates: 11/30/11 - Written by D.M. Boore, based on gsprd, and implementing
!                   Gail Atkinson's November 2011 frequency-dependent
!                   geometrical spreading for ENA.  This subroutine is
!                   called from spect_amp (see comments in CONST_AM0_GSPRD).
!        01/16/12 - Try a different gspread (r^-0.5 beyond R=50 km for all frequencies;
!                   r^eta for R<=50 km, where eta = -1.0 for f<1 Hz, -1.5 for f > 4 Hz,
!                   with a linear transition (in log f) between).
!        01/19/12 - Allow for either GMA or DMB gsprd
!        01/23/12 - Replace the many options for DMB (with different spreading rates, etc)
!                   with a single option, with variables passed through the
!                   argument list
!        04/05/13 - Add CASE 04 (in GMA & DMB April 2013 atten paper).
!        10/02/13 - Revise CASE 04

      real f, r, r_ref, gsprd_antilog10_gma_nov2011, r1, r2

      real gsprd_pwr_dmb

      integer i_gsprd_option
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      SELECT CASE (i_gsprd_option)
       
        CASE(0)
         
          print *,' i_gsprd_option = 0 in call to gsprd_freq, '//
     :            'which is not supposed to occur; QUITTING!' 
          stop
           
        CASE(1)  ! GMA, November, 2011

          r1 = amin1(r, 60.0)
          r2 = amax1(r/60.0, 1.0)

          gsprd_antilog10_gma_nov2011 = -1.6*alog10(r1) 
     :        -0.5*alog10(r2) + delta_gma_nov2011(f, r)  
     
          gsprd_freq = 10.00**gsprd_antilog10_gma_nov2011
        
        CASE(2)  ! DMB, January, 2012
          
          r_ref = 1.0  ! hardwire this
          
          if (r <= r_ref) then
            gsprd_freq = 1/r
          else if (r_ref < r .and. r <= r1) then
            gsprd_freq =  (r/r_ref)**gsprd_pwr_dmb(f, 
     :       r, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2) 
          else
            gsprd_freq = (r1/r_ref)**gsprd_pwr_dmb(f,
     :       r1, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2) 
     :                                   *
     :                       (r/r1)**gsprd_pwr_dmb(f, 
     :       r, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2)
          end if
          
        CASE(3)  ! GMA, September, 2012

          rt = amin1(70.0, (70.0-amin1( 57.2*alog10(f/2),40.0) ))

          if ( r < rt ) then
             gsprd_freq = (r_ref/r)**1.3
          else
             gsprd_freq = (r_ref/rt)**1.3*(rt/r)**0.5
          end if
 
        CASE(4)  ! Atkinson, G.M. and D.M. Boore (2014). The attenuation of Fourier amplitudes 
                 ! for rock sites in eastern North America, Bull. Seismol. Soc. Am. 104, (in press). 

        
          pidiv2 = 1.570796  ! I defined this in SMSIM.FI, but I do not include that file in this subroutine
          

          h4gspread = r1
          r_hinge = 50.0
          
          tc = amax1( 1.0-1.429*alog10( amax1( f, 1.0)), 0.0 )
            
          if ( r <= r_hinge) then
          
            if ( r <= h4gspread) then

              clf = 0.2*cos( pidiv2*(r-h4gspread)/(1.0-h4gspread) )
            
            else  
          
              clf = 0.2*cos( pidiv2*(amin1(r,r_hinge)-h4gspread)/
     :                      (r_hinge-h4gspread) )
          
            end if 
            
            gsprd_freq = 10.0**(tc*clf)*(r_ref/r)**1.3

          else            

            gsprd_freq = (r_ref/r_hinge)**1.3*(r_hinge/r)**0.5

          end if
            
        CASE default
        
          print *,' i_gsprd_option = ', i_gsprd_option,' in call to '// 
     :     'gsprd_freq, for which there is no CASE; QUITTING!' 
          stop
          
      END SELECT
           
      return
      END

      FUNCTION gsprd_pwr_dmb(f, r, r1, 
     :  eta_r_le_r1_lf, eta_r_le_r1_hf, eta_r_gt_r1, ft1, ft2)

        real f, r, r1, 
     :    eta_r_le_r1_lf, eta_r_le_r1_hf, eta_r_gt_r1,
     :    ft1, ft2, gsprd_pwr_dmb
             
        if (r > r1) then
          gsprd_pwr_dmb = eta_r_gt_r1
        else
          if (f <= ft1)                             then
            gsprd_pwr_dmb = eta_r_le_r1_lf
          else if (f > ft1 .and. f <= ft2)          then
            gsprd_pwr_dmb = eta_r_le_r1_lf 
     :        - ((eta_r_le_r1_lf - eta_r_le_r1_hf)/alog(ft2/ft1))*
     :                       alog(f/ft1)
          else           
            gsprd_pwr_dmb = eta_r_le_r1_hf
          end if
        end if           

      return
      END FUNCTION gsprd_pwr_dmb
      
      
      FUNCTION delta_gma_nov2011(f, r)
      real f, r, b
      
      if (f <= 1.0) then
        b = -0.6
      else if (f >= 3.162) then
        b = -0.1
      else
        b = -0.6 + alog10(f)
      end if
      
      delta_gma_nov2011 = 0.7 - amin1(0.7,0.7*exp(b*(r-1.0)))
      
      return
      END FUNCTION delta_gma_nov2011     
!----------------- END GSPRD_FREQ -----------------------------

 
!----------------- BEGIN SPECT_SHAPE -----------------------------
! Source Displacement Spectrum
! Dates: 06/07/95 - Written by D.M. Boore
!        11/15/95 - changed source types
!        12/02/96 - added Atkinson and Silva (model 4) (I did this earlier)
!                   and Haddon (model 5)
!        02/28/97 - Added Atkinson's new version of the source spectra
!                   derived from Atkinson and Silva (this will appear
!                   in Atkinson and Boore... Evaluation of source models...).
!                   (model 6)
!        06/24/97 - added Boatwright and Choy scaling (model 7).
!        07/21/97 - Added Joyner ENA model (model 8; the spectral shape is
!                   the same as his model 2, but I because the corner frequency
!                   relations are new I have to repeat the shape with the new 
!                   model number).
!        09/02/98 - Renamed model 6 to AB98-Ca to be consistent with usage
!                   in Tables 3 and 4 in Atkinson and Boore, BSSA, v. 88, 
!                   p. 917--934.
!        02/16/99 - Added Atkinson and Silva, 1999, as model 9
!        06/05/00 - Changed "AS99" to "AS2000" because the paper was published
!                   in 2000 (BSSA 90, 255--274)
!        07/15/05 - Added new ENA model (from Gail Atkinson)
!        07/16/05 - Replace computed go to with if then else
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        03/24/13 - Add source 12 (added c1_eps, c2_eps as input parameters)

      function spect_shape(f, fa, fb, pf_a, pd_a, pf_b, pd_b, 
     :                     am0b_m0, numsource)
      
      real spect_shape
      
      if (numsource .eq. 1) then  

! Single corner frequency:
        sb = 1.0
        sa = 1.0/( 1.0 + (f/fa)**pf_a )**pd_a 

      else if (numsource .eq. 2) then

! Joyner model
        sb = 1.0/ ( 1.0 + (f/fb)**2 )**0.25
        sa = 1.0/ ( 1.0 + (f/fa)**2 )**0.75

      else if (numsource .eq. 3) then

! Atkinson 1993 model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 4) then

! Atkinson & Silva 1996 (same format as Atkinson 1993) model
       sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 5) then

! Haddon (see 12/02/96 notes; approximate spectra in Fig. 10 of
! Haddon's paper in BSSA, v. 86, p. 1312)
        pda = 1.0/8.0
        pdb = 1.0/8.0
        pfa = 1/pda
        pfb = 1/pdb
        sa = 1.0/( 1.0 + (f/fa)**pfa )**pda
        sb = 1.0/( 1.0 + (f/fb)**pfb )**pdb 

      else if (numsource .eq. 6) then

! AB98-Ca (Atkinson & Boore 1998) (same format as Atkinson 1993) model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 7) then

! Boatwright and Choy (this is the functional form used by 
!  Boore and Atkinson, BSSA 79, p. 1761)
        sa = 1.0
        if (f .ge. fa) sa = fa/f
        sb = 1.0/sqrt( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 8) then

! Joyner model (to be used with his ENA two-corner model)
        sb = 1.0/ ( 1.0 + (f/fb)**2 )**0.25
        sa = 1.0/ ( 1.0 + (f/fa)**2 )**0.75 

      else if (numsource .eq. 9) then

! AS2000 (Atkinson and Silva, 2000, BSSA 90, 255--274) 
! (same format as Atkinson 1993) model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 10) then

! Atkinson 2005 model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 11) then
      
! General multiplicative two-corner model      
        sa = 1.0/( 1.0 + (f/fa)**pf_a )**pd_a 
        sb = 1.0/( 1.0 + (f/fb)**pf_b )**pd_b 

      else if (numsource .eq. 12) then
      
! General additive two-corner model      
       sb = 1.0
       sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**pf_a )**pd_a
     :      +       am0b_m0/( 1.0 + (f/fb)**pf_b )**pd_b 

      else

! Not a legal source number:
        write(*, '(a, i5, a)') ' !!!!!! numsource = ',
     :  numsource, ', .ne. legal value; quitting !!!!!!'
        stop
 
      end if
      
      spect_shape = sa*sb

      return
      end
!----------------- END SPECT_SHAPE -----------------------------

!----------------- BEGIN SPECT_SCALE -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        06/05/96 - Added modified Atkinson & Silva scaling
!        12/02/96 - added Haddon scaling (see spect_shape.for comments)
!        02/28/97 - added Atkinson and Boore scaling 
!                   (see spect_shape.for comments)
!        06/24/97 - added Boatwright and Choy scaling
!        07/10/97 - changed A93, AS96, AS97 scaling to be constant
!                   stress for M < Mc, where Mc is the magnitude for which
!                   am0b_m0 = 1.0.  The single corner model for smaller
!                   magnitudes is determined so that the high frequency
!                   level is matched at M = Mc.
!        07/21/97 - Added Joyner 2-corner model for ENA, as specified 
!                   in his notes prepared for the SSHAC workshop (published 
!                   in SSHAC report, NUREG/CR-6372, p. B-303--B-305, 1997).
!        08/06/97 - I discovered that Joyner erroneously fit vertical spectral
!                   amplitudes.  He provided a revised model, fitting the
!                   horizontal amplitudes.  I changed model 8 accordingly.
!        09/02/98 - Renamed model 6 to AB98-Ca to be consistent with usage
!                   in Tables 3 and 4 in Atkinson and Boore, BSSA, v. 88, 
!                   p. 917--934.
!        02/16/99 - Added Atkinson and Silva (1999)
!        06/05/00 - Changed "AS99" to "AS2000" because the paper was published
!                   in 2000 (BSSA 90, 255--274)
!        07/15/05 - Added new ENA model (from Gail Atkinson)
!        07/16/05 - Replace computed go to with if then else
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!                   Explicitly set the stress variables to 0.0 for those source
!                   models that do not use stress as a parameter.
!        03/24/13 - Added source 12

      subroutine spect_scale()


      include 'smsim.fi'

      am0 = 10.**(1.5*amag + 16.05)  ! Now put this into the programs in which amag is specified, to 
                                     ! decrease confusion about where am0 is computed.
      am0b_m0 = 0.0
  
      if (numsource .eq. 1) then  

! Single corner frequency:
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fa = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        fb = fa
 
      else if (numsource .eq. 2) then

! Joyner scaling:
        am0c = 10.0 ** ( 1.5*amagc + 16.05 )
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        rat = stress/am0
        dum = 4.906e+06
        if ( am0 .gt. am0c ) rat = stress/am0c
        fb = ( dum*beta ) * ( fbdfa )**(3./4.) * ( rat )**(1./3.)
        fa = ( dum*beta ) * (stress)**(1./3.) * (am0c)**(1./6.)
     :     * ( fbdfa )**(-0.25) * ( am0 )**(-0.5)
        if ( am0 .lt. am0c ) fa = fb / fbdfa
 
      else if (numsource .eq. 3) then

! Atkinson 93 scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 4.0) then
          fa = 10.0**(2.41 - 0.533 * amag)
          fb = 10.0**(1.43 - 0.188 * amag)      ! fa = fb for M = 2.84
          am0b_m0 = 10.0**(2.52 - 0.637 * amag)
        else
          fb = 10.0**(2.678 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 4) then

! Atkinson and Silva 96 scaling, with am0b_m0 modified by D. Boore on 6/04/96

        stressc = 0.0        
        dlsdm = 0.0
        
        if (amag .gt. 4.6) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(1.778 - 0.302 * amag)   ! fa = fb for M = 2.08
          am0b_m0 = 10.0**(3.440 - 0.746 * amag)  ! DMB's fitting of spctrl ratios
!        am0b_m0 = 10.0**(2.764 - 0.623 * amag) ! in Atkinson & Silva preprint
        else
          fb = 10.0**(2.689 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 5) then

! Haddon scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(0.3 - (1.5/3)*(amag-4.0))
        fb = 10.0**(1.4 - (1.5/3)*(amag-4.0))  ! fa < fb for all M
 
      else if (numsource .eq. 6) then

! AB98-Ca (Atkinson and Boore 98 scaling, based on fitting a model to the 
! Atkinson and Silva 1997 Fourier amplitude spectra for California; see
! Atkinson and Boore, BSSA, v. 88, p. 917--934).

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 4.8) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(1.308 - 0.227 * amag)      ! fa=fb for M = 3.25
          am0b_m0 = 10.0**(3.223 - 0.670 * amag)
        else
          fb = 10.0**(2.617 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 7) then

! Boatwright and Choy (this is not from Boore and Atkinson, BSSA 79, p. 1761;
!  it is based on new fits by DMB on 6/24/97 to data in Boat.&Choy, 1992 BSSA.
!  See BC_BSSA.WQ1 in \haddon subdirectory and handwritten notes on 
!  yellow sheets.
!  except set fa=fb=constant stress scaling for M<=5.3)

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(3.409 - 0.681 * amag)
        fb = 10.0**(1.495 - 0.319 * amag)
        if (amag .le. 5.3) then
          fa = 0.634*10.0**(0.5*(5.3 - amag)) ! 0.634= 10^(logfa+logfb)/2 at M5.3
          fb = fa
        end if
 
      else if (numsource .eq. 8) then

! Joyner ENA scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(2.312 - 0.5 * amag)
        fb = 10.0**(3.609 - 0.5 * amag)
 
      else if (numsource .eq. 9) then

! Atkinson and Silva (2000) (AS00) scaling, based on fitting a point source
! model to finite source calculations, with constraints on various modeling
! studies, with modification for very small magnitude (when eps = 1).
! Note that in AS00 the distance is altered by using a magnitude-dependent
! depth (equation 4 in AS00).  Until the 12/17/09 revision, the distance
! was recomputed in gsprd for numsource = 9, but now I let this be
! done independently of the source number (but this means that for consistency,
! the coefficients defining f_ff in the parameter file should be those used 
! in AS00).  If the distance is modified for the AS00 source, note that 
! comparisons of spectra at close
! distances for input r = 1, 10 (for example) will not differ by about 10 (for
! a geometrical spreading of 1/r).  The application of the effective depth is
! not implemented as precisely as it should be, because this would require
! specifying the depth as an input parameter (or specifying the distance as
! the closest horizontal distance to the surface projection of the fault (rjb)),
! and then using the effective depth for the as00 scaling, or a period
! dependent pseudodepth, such as from bjf97.    This all has to do with
! the modifications needed to approximate finite-fault ground motions.  This
! is material for a future upgrade to the program.

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 2.4) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(2.41  - 0.408 * amag)      ! fa=fb for M = -2.6
          am0b_m0 = 10.0**(0.605 - 0.255 * amag)
        else
          fb = 10.0**(1.431 - 0.5 * (amag - 2.4))
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 10) then

! Atkinson 2005 scaling:  

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(2.41 - 0.533 * amag)
        fb = 10.0**(2.06 - 0.345 * amag)  
        am0b_m0 = 10.0**(1.2 - 0.3 * amag)
 
      else if (numsource .eq. 11) then

! General two-corner source (see smsim_generalization_of_2-corner_frequency_source_models_v04.pdf):
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fc = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        fa = 10.0**(c1_fa + c2_fa * (amag-amagc4fa))
        fb = (fc**2/fa**(pf_a*pd_a))**(1.0/(pf_b*pd_b))



      else if (numsource .eq. 12) then

! General two-corner source (see smsim_generalization_of_2-corner_frequency_source_models_v04.pdf):
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fc = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        if (amag < mag4eps1) then
          fa = fc
          fb = fc
          am0b_m0 = 1.0
        else
          fa = 10.0**(c1_fa + c2_fa * (amag-amagc4fa))
          eps = 10.0**(c1_eps + c2_eps * (amag-amagc4eps))
          am0b_m0 = eps 
          if (eps < 1.0) then
! Trap for fb undefined
            ds_min = ((sqrt(1.0-eps)/(4.906e+06 * beta))*fa)**3*am0
            if (stress < ds_min) then
              print *,' WARNING: for source 12, stress = ',stress
              print *,' is less than the minimum stress of ',ds_min
              print *,' for fa and eps = ', fa, eps
              print *,' QUITTING because fb is undefined!!!'
              stop
            end if
          end if
          fb = sqrt( (fc**2 - (1-eps)*fa**2)/eps )
        end if

      else

! Not a legal source number:
        write(*, '(a, i5, a)') ' !!!!!! numsource = ',
     :  numsource, ', .ne. legal value; quitting !!!!!!'
        stop

      end if

      return

      end
!----------------- END SPECT_SCALE -----------------------------

!----------------- BEGIN CRUSTAL_AMP_FACTOR -----------------------------
      function crustal_amp_factor(f, namps, famp, amp, interp_type)

! Dates: 06/07/95 - Written by D.M. Boore
!        02/10/15 - Add option to select type of interpolation
!                   Type of interpolation:
!                     inter_type = 1: linear f, linear a
!                     inter_type = 2: linear f, log    a
!                     inter_type = 3: log    f, linear a
!                     inter_type = 4: log    f, log    a


      real famp(*), amp(*), crustal_amp_factor, slope
      
      if ( f .le. famp(1) ) then
        crustal_amp_factor = amp(1)
      else if ( f .ge. famp(namps) ) then
        crustal_amp_factor = amp(namps)
      else
        call locate( famp, namps, f, j)
        SELECT CASE (interp_type)
          CASE(1) ! linear f, linear A
            slope = (amp(j+1)-amp(j))/(famp(j+1)-famp(j))
            crustal_amp_factor = amp(j) + slope * (f - famp(j))
          CASE(2) ! linear f, log A
            slope = alog10(amp(j+1)/amp(j))/(famp(j+1)-famp(j))
            crustal_amp_factor = amp(j)*10.0**(slope*(f - famp(j)))
          CASE(3) ! log f, linear A
            slope = (amp(j+1)-amp(j))/alog10(famp(j+1)/famp(j))
            crustal_amp_factor = amp(j) + slope * alog10(f/famp(j))
          CASE(4) ! log f, log A
            slope = alog10(amp(j+1)/amp(j))/alog10(famp(j+1)/famp(j))
            crustal_amp_factor = amp(j)*(f/famp(j))**slope
        END SELECT
      end if

      return
      end
!----------------- END CRUSTAL_AMP_FACTOR -----------------------------

!----------------- BEGIN DIMIN -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added modification to r required by Atkinson
!                   and Silva (1999)
!        06/05/00 - Substitute c_q for beta in akappaq and add comments
!                   about r
!        05/11/07 - Removed "\smsim\" from include statements
!        12/01/09 - Check for fm = 0.0 or kappa = 0.0; if either is
!                   true, do not include that parameter in the
!                   calculations (only have to check for fm = 0.0,
!                   because kappa = 0.0 is the same as not using
!                   kappa).
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/17/09 - Bring in rmod as a calling argument, not 
!                   calculated only if numsource = 9 (note on 04/08/11:  sometime between
!                   12/17/09 and 04/08/11 I bring rmod in through smsim.fi).
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.

      function dimin(f)
      real dimin, mag
      include 'smsim.fi'

      akappaq = rmod/(c_q*q(f))

      mag = amag    
      
      if (fm .eq. 0.0) then
        dimin = exp( -pi*(kappa_f(mag) + akappaq) * f) 
      else
        dimin = exp( -pi*(kappa_f(mag) + akappaq) * f)/
     :   sqrt( 1. + (f/fm)**8.0 )
      end if
      

      return
      end
!----------------- END DIMIN -----------------------------

!----------------- BEGIN KAPPA_F -----------------------------
! Dates: 02/28/97 - Written by D.M. Boore
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.

      function kappa_f(mag)
      real mag
      include 'smsim.fi'
  
      kappa_f = akappa + dkappadmag*(mag-amagkref)

      return
      end
!----------------- END KAPPA_F -----------------------------
        
!----------------- BEGIN Q -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        12/14/95 - Added check for equal transition frequencies
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        05/16/15 - Some modernization of code

      function q(f) 

      logical, save ::  firstcall_q = .true.
      real(4), save ::  qt1, qt2, st
      
      real, intent(in) :: f
      real(4) :: q      
      
      include 'smsim.fi'

      q = 9999.0
      if (f == 0.0) return
        
      if (firstcall_q) then
        qt1 = qr1*(ft1/fr1)**s1
        qt2 = qr2*(ft2/fr2)**s2
        st = 0.0
        if (ft1 /= ft2) then
          st = alog10(qt2/qt1)/alog10(ft2/ft1)
        end if
        firstcall_q = .false.
      end if
      
      if ( f <= ft1) then
        q = qr1*(f/fr1)**s1
      else if ( f >= ft2) then
        q = qr2*(f/fr2)**s2
      else
        q = qt1*(f/ft1)**st
      end if

      return
      end
!----------------- END Q -----------------------------


!----------------- BEGIN DURSOURCE -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
      function dursource(w_fa, fa, w_fb, fb)
      real dursource
      dursource = w_fa/fa + w_fb/fb
      return
      end
!----------------- END DURSOURCE -----------------------------
      
!----------------- BEGIN DURPATH -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments of durpath
!        06/05/00 - Changed reference for Atkinson and Silva (1999) to
!                   2000, the year in which their paper was published
!                   and added comments regarding r
!        12/04/09 - Add test for rmod <= rdur(1)
!        12/05/09 - Rearrange right hand side to be dur(j)+dur
!        12/17/09 - Bring in rmod as a calling argument

      function durpath(rmod, nknots, rdur, dur, slast, numsource, amag)
      real rdur(*), dur(*), durpath

      if ( rmod .le. rdur(1) ) then
        durpath = dur(1)
      else if ( rmod .ge. rdur(nknots) ) then
        durpath = dur(nknots) + (rmod -rdur(nknots))*slast 
      else
        call locate(rdur, nknots, rmod, j)
        durpath = dur(j) + (rmod - rdur(j))*( dur(j+1)-dur(j))
     :            /(rdur(j+1)-rdur(j))
      end if

      return
      end
!----------------- END DURPATH -----------------------------

!----------------- BEGIN SPINS -----------------------------
      function  spinsf (f,pp,pg,s,hp,hg)
!   this subroutine computes the frequency response of the WWSSN
!   short period instrument, which is heavily influenced by the
!   transducer inductance.  see reference by Burdick and Mellmann,
!   'The response of the WWSSN short period seismometer';
!   contribution no. 2628, Cal. Tech. Division of Geological and
!   Planetary Sciences.
!   latest version: 11/24/83
!
!   input variables
!
!       f:  frequency
!       pp:  transducer free period
!       pg:  galvanometer free period
!       s:  coupling factor squared
!       hp:  transducer damping
!       hg:  galvanometer damping
!       r1:  resistance of  the transducer branch
!       r2:  resistance of the galvanometer branch
!       r3:  shunt resistance
!       l:  inductance of the transducer
!
!   the values for the WWSSN short period instruments are;
!
!   fmax:  1.7 Hz        pp: 1.0 s         pg: 0.75 s
!      s:  0.0           hp: 0.490         hg: 1.00
!
!   a circuit equivalent to the WWSSN short period instrument
!   in operating conditions is:
!
!         damp trim
!         resistor
!          rb=22. ohms           rc=68. ohms            re=90.ohms
!     ________/\/\/\/\___________/\/\/\/\/\_____________/\/\/\/\/\__
!    1                                           1                  1
!    1                                           1                  1
!    1                                           1                  1
!    ( transducer coil                   shunt   /                  1
!    )  l= 6.8 henries               resistance  \      galvanometer*
!    (  ra=62.5 ohms                rd= 18. ohms /       rf= 80 ohms*
!    )  pp= 1.0 sec                              \       pg= .75 sec*
!    (                                           /                  *
!    1                                           1                  1
!    1                                           1                  1
!    ________________________________________________________________
!
!
!    r1= ra +rb + rc = 152.5 ohms
!    r2 = re + rf = 170 ohms
!    r3 = rd = 18 ohms
!    l = 6.8 henries
!
!
      real l
      r1=152.5
      r2=170.
      r3=18.0
      l=6.8
      twopi = 6.28318531
      fn1 = twopi/pp
      fn2 = twopi/pg
      fk1 = hp*fn1
      fk2 = hg*fn2
      w = twopi*f
      w2 = w**2
!
!
      q2 = r1*r2 + r2*r3 + r3*r1
      u = l*(r2+r3)*w/q2
      fa = 1./sqrt(1.+u**2)
      cplr2 = s*(fa**4)*(1.-u**2)
      cpli2 = s*(fa**4)*2.*u
!
!
      es = fk1*(fa**2)
      eg = fk2*(fa**2)*(1.+(l*w*u)/(r1+r3))
!
!
      ws2 = (fn1**2)+2.*fk1*w*u*(fa**2)
      wg2 = (fn2**2) +2.*fk2*w*(fa**2)*w*l*(r3**2)
     */(q2*(r1+r3))
!
!
      aim = (ws2 - w2)*(wg2-w2)+(cplr2-4.*es*eg)*w2
      are = 2.*es*w*(wg2-w2)+2.*eg*w*(ws2-w2)-cpli2*w2
!
!
      spinsf = (w**3)*fa/sqrt((are**2)+(aim**2))
!      arg = atan2(aim,are)-atan(u)
!  arg: phase response (-3*pi/2 to pi)   
      return
      end
!----------------- END SPINS -----------------------------

!  ------------------- BEGIN BUTTRLCF -------------------------------
! Dates: 06/07/95 - Written by D.M. Boore
      function buttrlcf( f, fcut, norder)
!
! Computes the response of an norder, bidirectional
! high-pass Butterworth filter.  This is the filter
! used by the AGRAM processing (the equation was
! provided by April Converse).

! Modification: 3/27/89 - created by modifying HiPassF

      real buttrlcf
      buttrlcf = 1.0
      if ( fcut.eq.0.0 ) return

      buttrlcf = 0.0

      if ( f .eq. 0.0) return

      buttrlcf = 1.0/ (1.0+(fcut/f)**(2.0*norder))

! Note: Because this filter is intended to simulate the
! two-pass, zero-phase (acausal) Butterworth filter used in
! accelerogram processing, buttrlcf = 1/2 when f = fcut, not 1/sqrt(2) as in
! a single-pass, causal Butterworth filter.

      return
      end
!  ------------------- END BUTTRLCF -------------------------------

!  ------------------- BEGIN HIGH_CUT_FILTER ------------------------ 
!      function high_cut_filter(f, itype_hcfilt, fhc1, fhc2, eta_hcfilt)
      function high_cut_filter(f)

! Dates: 12/11/15 - Written by D. Boore

      real high_cut_filter
      
      include 'smsim.fi'

      high_cut_filter = 1.0
      if ( itype_hcfilt == 0 .or. f <= fhc1) return
      
      if ( f >= fhc2 ) then
        high_cut_filter = 0.0
        return
      end if
      
      if ( itype_hcfilt == 1 ) then  ! raised half-cycle of cosine
        high_cut_filter = 
     :     (0.5*(1.0+cos(pi*(f - fhc1)/(fhc2 - fhc1))))**eta_hcfilt
      else if ( itype_hcfilt == 2 ) then  ! quarter-cycle of cosine
        high_cut_filter = 
     :     (cos(0.5*pi*(f - fhc1)/(fhc2 - fhc1)))**eta_hcfilt
      else
        write(*,'(a)') ' ERROR: itype_hcfilt /= 0, 1, or 2; QUITTING!!'
! A valid value for itype_hcfilt was checked in get_params, so this statement is redundant.
        stop
      end if
      
      return
      end
!  ------------------- END HIGH_CUT_FILTER ------------------------ 




!----------------- BEGIN SMSIMFAS -----------------------------
      subroutine SMSIMFAS(fas, freq, nfreq)
! Dates: 12/16/95 - Written by D.M. Boore, based on SMSIM_RV
!        03/05/99 - Removed include statements from the end (put them into the
!                   driver programs)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        11/29/11 - Add possibility that gspread is frequency dependent; see remarks in  
!                   spect_amp, and const_am0_gsprd.

      real fas(*), freq(*)
      include 'smsim.fi'

      pi = 4.0*atan(1.0)
      twopi = 2.0 * pi

! Set spectral parameters:
      call spect_scale()   ! Be sure to call Get_Params in driver

! Get frequency-independent factor:
      call const_am0_gsprd()

! Be sure to call const_am0 before spect_amp, because spect_amp 
! uses freq_indep_factor, computed in const_am0 and passed into spect_amp
! through the common block /const_params/.

      do i = 1, nfreq
        fas(i) = spect_amp(freq(i))
      end do

      return
      end
!----------------- END SMSIMFAS -----------------------------

! --------------------------- BEGIN BJFV1V2F -------------------          
      function BJFV1V2F(per, V1, V2)

! Returns the correction factor to apply to response spectra with V30 = V1
! to obtain the response spectra for V30 = V2 (i.e., computes PRV(V2)/PRV(V1)).
! The value for per = 0.1 is used for all smaller periods and the per=2.0 value 
! is used for periods longer than 2.0 sec (in an earlier version I used
! per = 1.0).  Note that the latter is
! conservative; we expect the amplifications to reach unity for long enough
! periods (at least for Fourier spectra, what about for response spectra?)

! Dates: 07/17/96 - Written by D. Boore, based on BJFR2S_F.FOR
!        07/18/96 - Changed endpoint period from 1.0 to 2.0
! Dates: 07/24/96 - Added check of V1, V2 equal zero, in which case unit
!                   amplification is returned.
!                   Also, reversed the meaning of V1 and V2.
!                   Added pga amps when per = 0.01
!        10/08/00 - pga when per = 0.0, rather than 0.01

      if (v1 .eq. 0.0 .or. v2 .eq. 0.0) then
         bjfv1v2f = 1.0
         return
      end if

      velfact = alog10( V2/V1 )
      if(per .lt. 0.1) then
         if(per .eq. 0.0) then
           bjfv1v2f = 10.0**(velfact*(-0.371))
         else           
           bjfv1v2f = 10.0**(velfact*cubic(0.1))
         end if
      else if (per .gt. 2.0) then
         bjfv1v2f = 10.0**(velfact*cubic(2.0))
      else
         bjfv1v2f = 10.0**(velfact*cubic(per))
      end if

      return
      end

      function cubic(per)
        c0 = -0.21172
        c1 =  0.06619
        c2 = -1.35085
        c3 =  0.79809
        x = alog10(per/0.1)
        cubic = c0 + c1*x + c2*x**2 + c3*x**3
!        a0 = 0.2102
!        a1 = 0.0726
!        a2 = -0.3142
!        a3 = -0.2403
!        x = alog10(per)
!        cubic = a0 + a1*x + a2*x**2 + a3*x**3
      return
      end
! --------------------------- END BJFV1V2F ------------------- 

! ---------------------------------------------------------- BEGIN RMOD_CAlC
      function rmod_calc(r, m, iflag, nlines, c1, c2, mh, c3, c4,
     :                   m1t, m2t, c0t, c1t, c2t, c3t, f_ff) 
      
!!
!!finite_fault factor specification:
!!  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0 if a finite-fault factor is not to be used)
!!
!!  Distance for point-source calculation
!!    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))
!!    If iflag_f_ff = 2: rps =  r + f_ff
!!   Use rps in the calculations (this variable is called rmod in the code; it should be changed to rps to
!!   reflect my current preferred terminology.  I do not have time to do this now).
!!  Specification of the finite-fault factor h:
!!    If nlines = 1
!!      log10(f_ff) = c1 + c2*amag  
!!    If nlines = 2
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh
!!      where Mh is determined by the intersection of the two lines
!!      (this is computed in the program)  
!!    If nlines = 3
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2
!!      log10(f_ff) given by a cubic in amag between the two lines (this
!!        produces a smooth transition over the magnitude range DeltaM
!!  *** NOTE: placeholders are needed for c3, c4, and DeltaM, even if not used.
!!
!!  Published finite-fault factors
!!    Author                      applicable_region meaning_of_r  iflag_f_ff nlines         c1      c2   c3    c4  
!!    Atkinson and Silva (2000)                 ACR        r_rup           1    1      -0.0500  0.1500  0.0   0.0
!!    Toro (2002)                               SCR        r_rup           2    1      -1.0506  0.2606  0.0   0.0
!!    Atkinson and Boore (2003)          subduction        r_rup           1    1      -2.1403  0.5070  0.0   0.0
!!    Yenier and Atkinson (2014)                ACR        r_rup           1    1      -1.7200  0.4300  0.0   0.0
!!    Yenier and Atkinson (2015)                ACR        r_rup           1    1      -0.4050  0.2350  0.0   0.0
!!    Yenier and Atkinson (2015),               SCR        r_rup           1    1      -0.5690  0.2350  0.0   0.0
!!    Boore and Thompson  (2015) (BT15)        see below
!!  
!!  Input for some of the models, as well as suggested modifications for stable continental regions
!!    Assuming that all of the above the above relations except Toro (2002) and Atkinson and Boore (2003)
!!    are for active crustal regions, and that h is proportional to fault radius, then -0.1644 should be
!!    added to c1 (and c3 for Boore (2014) to adjust for the smaller fault size expected for stable continental region
!!    earthquakes (this adjustment factor uses radius ~ stress^-1/3, and a stress of 88 bars for ACR (from 
!!    my determination of what stress matches the Atkinson and Silva (2000) high-frequency spectral level--
!!    see What_SCF_stress_param_is_consistent_with_the_AS00_source_model.pdf in the daves notes page of
!!    www.daveboore.com) and 274 bars for SCR, from my inversion of 0.1 s and 0.2 s PSA values for 8 ENA
!!    earthquakes, using the Boatwright and Seekins (2011) attenuation model.  This determination is 
!!    part of ongoing work for the NGA-East project, and will appear in a PEER report in 2015.
!!   1    1      -0.0500  0.1500  0.0   0.0 0.0       ! ACR: AS00
!!   1    2      -1.7200  0.4300 -0.405 0.2350 0.0    ! ACR: YA14&YA15, no smoothing
!!   1    3      -1.7200  0.4300 -0.405 0.2350 2.0    ! ACR: BT15 (=YA14&YA15, smooth over 2 magnitude units
!!   1    1      -1.7200  0.4300  0.0   0.0 0.0       ! ACR: YA14
!!   1    1      -0.4050  0.2350  0.0   0.0 0.0       ! ACR: YA15
!!
!!   1    1      -0.2144  0.1500  0.0   0.0 0.0       ! SCR: AS00
!!   1    2      -1.8844  0.4300 -0.5694 0.2350 0.0   ! SCR: YA14&YA15, no smoothing
!!   1    3      -1.8844  0.4300 -0.5694 0.2350 2.0   ! SCR: BT15 (=YA14&YA15, smooth over 2 magnitude units)
!!   1    1      -1.8844  0.4300  0.0   0.0 0.0       ! SCR: YA14
!!   1    1      -0.5694  0.2350  0.0   0.0 0.0       ! SCR: YA15
!   0 0 0.0 0.0 0.0 0.0 0.0                           ! No f_ff

      
! Dates: 04/05/11 - Written by D. Boore
!        10/08/14 - Allow for two lines
!        12/18/14 - Allow for a transition curve between the two lines

      real rmod_calc, r, m, mh, m1t, m2t, m4calc
      
      if (iflag == 0) then
        f_ff = 0.0
        rmod_calc = r
        return
      else 
        if (nlines == 1) then
          f_ff = 10.0**(c1 + c2 * m)
        else if (nlines == 2) then
          if (m <= mh) then
            f_ff = 10.0**(c1 + c2 * m)
          else
            f_ff = 10.0**(c3 + c4 * m)
          end if
        else if (nlines == 3) then
          if (m <= m1t) then
            f_ff = 10.0**(c1 + c2 * m)
          else if (m >= m2t) then
            f_ff = 10.0**(c3 + c4 * m)
          else
            m4calc = m - m1t
            f_ff = 10.0**
     :        (c0t + c1t*m4calc + c2t*m4calc**2.0 + c3t*m4calc**3.0)
          end if        
        else
          print *, ' within rmod_calc, nlines has an invalid value; '//
     :         'nlines = ', nlines
          print *, ' STOP the program!'
          stop
        end if
      end if
      
      if (iflag == 1) then
        rmod_calc = sqrt(r**2 + f_ff**2)
        return
      else if (iflag == 2) then
        rmod_calc = r + f_ff 
        return
      else
        write(*,*) ' In rmod_calc, iflag = ', iflag, 
     :             ' is not valid; QUITTING'
        stop
      end if
      
      end
! ---------------------------------------------------------- END RMOD_CAlC
 
 ! ---------------------- BEGIN QROMB SUITE ------------------------
      SUBROUTINE qromb(func,a,b,ss)
      INTEGER JMAX,JMAXP,K,KM
      REAL a,b,func,ss,EPS
      EXTERNAL func
      PARAMETER (EPS=1.e-6, JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1)
!U    USES polint,trapzd
      INTEGER j
      REAL dss,h(JMAXP),s(JMAXP)
      h(1)=1.
      do 11 j=1,JMAX
        call trapzd(func,a,b,s(j),j)
        if (j.ge.K) then
          call polint(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
      call error(' Too many steps in qromb') ! mod. by RBH
      END

      SUBROUTINE trapzd(func,a,b,s,n)
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END

      SUBROUTINE polint(xa,ya,n,x,y,dy)
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.)call error(' denominator = 0 in polint') ! mod. by RBH
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END

      subroutine error(str)
!-----
!	RBH code (3/99)
!-----
      character str*(*)
      write(6,*)str
      stop
      end
! ----------------------- END QROMB SUITE ---------------

!----------------- BEGIN QMIDPNT -----------------------------
      SUBROUTINE qmidpnt(func,a,b,s)
!Dates: 02/09/00 - latest version of qtrap, with midpnt substituted for trapzd
      INTEGER JMAX
      REAL a,b,func,s,EPS
      EXTERNAL func
      PARAMETER (EPS=1.e-6, JMAX=12)
!U    USES midpnt
      INTEGER j
      REAL olds
      olds=-1.e30
      do j=1,JMAX
        call midpnt(func,a,b,s,j)
        if (j.gt.5) then
          if (abs(s-olds).lt.EPS*abs(olds).or.(s.eq.0..and.olds.eq.0.)) 
     *          return
        endif
        olds=s
      end do
      call error(' too many steps in qmidpnt')
      END
!----------------- END QMIDPNT -----------------------------

!----------------- BEGIN MIDPNT -----------------------------
      SUBROUTINE midpnt(func,a,b,s,n)
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL ddel,del,sum,tnm,x
      if (n.eq.1) then
        s=(b-a)*func(0.5*(a+b))
      else
        it=3**(n-2)
        tnm=it
        del=(b-a)/(3.*tnm)
        ddel=del+del
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+ddel
          sum=sum+func(x)
          x=x+del
11      continue
        s=(s+(b-a)*sum/tnm)/3.
      endif
      return
      END
!----------------- END MIDPNT -----------------------------

! --------------------------- BEGIN LOCATE ------------------
      SUBROUTINE locate(xx,n,x,j)
      INTEGER j,n
      REAL x,xx(n)
      INTEGER jl,jm,ju
      jl=0
      ju=n+1
10    if(ju-jl.gt.1)then
        jm=(ju+jl)/2
        if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      if(x.eq.xx(1))then
        j=1
      else if(x.eq.xx(n))then
        j=n-1
      else
        j=jl
      endif
      return
      END
! --------------------------- END LOCATE ------------------

      SUBROUTINE odeint(ystart,nvar,x1,x2,eps,h1,hmin,nok,nbad,derivs)

!  6/01/95 - D. Boore removed rkqs from the list of calling arguments 
!            and from the external statement.
!  2/09/03 - Set kmax = 0

      INTEGER nbad,nok,nvar,KMAXX,MAXSTP,NMAX
      REAL eps,h1,hmin,x1,x2,ystart(nvar),TINY
      EXTERNAL derivs
      PARAMETER (MAXSTP=10000,NMAX=50,KMAXX=200,TINY=1.e-30)
      INTEGER i,kmax,kount,nstp
      REAL dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp
      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      kmax = 0
      do 11 i=1,nvar
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.*dxsav
      do 16 nstp=1,MAXSTP
        call derivs(x,y,dydx)
        do 12 i=1,nvar
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvar
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
        call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvar
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvar
              yp(i,kount)=y(i)
15          continue
          endif
          return
        endif
        if(abs(hnext).lt.hmin) 
     :    call error(' stepsize smaller than minimum in odeint')
        h=hnext
16    continue
      call error(' too many steps in odeint')
      return
      END
! --------------- END ODEINT ---------------------------------

! --------------- BEGIN RKQS ---------------------------------
      SUBROUTINE rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
      INTEGER n,NMAX
      REAL eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
!U    USES derivs,rkck
      INTEGER i
      REAL errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4)
      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr,derivs)
      errmax=0.
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x) call error(' stepsize underflow in rkqs')
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END
! --------------- END RKQS ---------------------------------

! --------------- BEGIN RKCK ---------------------------------
      SUBROUTINE rkck(y,dydx,n,x,h,yout,yerr,derivs)
      INTEGER n,NMAX
      REAL h,x,dydx(n),y(n),yerr(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
!U    USES derivs
      INTEGER i
      REAL ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,
     *B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5,
     *B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512.,
     *B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,
     *C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,
     *DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336.,
     *DC6=C6-.25)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivs(x+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivs(x+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivs(x+A4*h,ytemp,ak4)
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivs(x+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivs(x+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END
! --------------- END RKCK ---------------------------------

!  ------------------- BEGIN GASDEV --------------------------
      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
!U    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (idum.lt.0) iset=0
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
!  ------------------- END GASDEV --------------------------

!  ------------------- BEGIN RAN1 --------------------------
      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
!  ------------------- END RAN1 --------------------------

!  ------------------- BEGIN REALFT --------------------------
      SUBROUTINE realft(data,n,isign)
      INTEGER isign,n
      REAL data(n)
!U    USES four1
      INTEGER i,i1,i2,i3,i4,n2p3
      REAL c1,c2,h1i,h1r,h2i,h2r,wis,wrs
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/dble(n/2)
      c1=0.5
      if (isign.eq.1) then
        c2=-0.5
        call four1(data,n/2,+1)
      else
        c2=0.5
        theta=-theta
      endif
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      wr=1.0d0+wpr
      wi=wpi
      n2p3=n+3
      do 11 i=2,n/4
        i1=2*i-1
        i2=i1+1
        i3=n2p3-i2
        i4=i3+1
        wrs=sngl(wr)
        wis=sngl(wi)
        h1r=c1*(data(i1)+data(i3))
        h1i=c1*(data(i2)-data(i4))
        h2r=-c2*(data(i2)+data(i4))
        h2i=c2*(data(i1)-data(i3))
        data(i1)=h1r+wrs*h2r-wis*h2i
        data(i2)=h1i+wrs*h2i+wis*h2r
        data(i3)=h1r-wrs*h2r+wis*h2i
        data(i4)=-h1i+wrs*h2i+wis*h2r
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
11    continue
      if (isign.eq.1) then
        h1r=data(1)
        data(1)=h1r+data(2)
        data(2)=h1r-data(2)
      else
        h1r=data(1)
        data(1)=c1*(h1r+data(2))
        data(2)=c1*(h1r-data(2))
        call four1(data,n/2,-1)
      endif
      return
      END
!  ------------------- END REALFT --------------------------

!  ------------------- BEGIN FOUR1 --------------------------
      SUBROUTINE four1(data,nn,isign)
      INTEGER isign,nn
      REAL data(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
            tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
!  ------------------- END FOUR1 --------------------------

! ------------------ BEGIN SORT --------------------------
      SUBROUTINE sort(n,arr)
      INTEGER n,M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          do 11 i=j-1,l,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
11        continue
          i=l-1
2         arr(i+1)=a
12      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
        endif
        i=l+1
        j=ir
        a=arr(l+1)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l+1)=arr(j)
        arr(j)=a
        jstack=jstack+2
        
!        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort'  ! replaced with code below on 11/02/12
                                                               ! because "pause" produced a compile error
                                                               ! with the Unix Fortran compiler
        if(jstack.gt.NSTACK) then
          print *,' NSTACK too small in sort; QUIT!!!'
          stop
        end if
        
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
! ------------------ END SORT --------------------------
!
!***************  WARNING!!!!!  IMPORTANT NOTE **********************
!
! Do not make any edits to TD_SUBS.FOR, because these will be overwritten
! the next time programs involving TD are compiled (TD_SUBS is created
! at compile time by calling make_td_subs_file.bat).
!
! Make any revisions in the individual *.for files for the subroutines.
!
!***************  WARNING!!!!!  IMPORTANT NOTE **********************
!
! ---------------------------------------------------- Get_Npts_For_TD_Calcs
      subroutine get_npts_for_td_calcs(nstart, nstop, npw2, te, ntaper)

! Calculate various indices and te (=t_eta)

! Note: all parameters passed to the routine are output parameters; the
! parameters needed to calculate the output parameters are obtained through
! common blocks soecified in smsim.fi

! Dates: 02/12/01 - Extracted from smsim_td.
!        05/11/07 - Removed "\smsim\" from include statements
!        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                   I caught this only when I compiled the program using the -chk
!                   switch.  This probably means that all of my time-domain runs
!                   up till now used ntaper = 0, even if a nonzero taper was 
!                   specified (the standard being 0.05).
!        12/04/09 - Save calculation of durpath and dursource so can pass
!                   them through a revised smsim.fi and write them if desired.
!                 - Place calculation of durex before if statement.
!                 - Renamed it from get_npts.for, because I might have a routine in
!                   \forprogs\ with this name that does something else.
!        12/21/09 - Bring in rmod as a calling argument and use in call to durpath.
!        12/22/09 - Set tb, te, tw to 0.0 if box window
!        02/12/10 - Initialize ntaper to 0.
!        11/02/12 - Trap for durex < dt, as can happen if M is very small.  Quit, rather
!                   than reset nmotion to 1.

      include 'smsim.fi'
      
      ntaper = 0

! Calculate the number of points in the noise sample:

      call spect_scale()  ! call this now because need corner frequencies
                          ! for window durations

! determine duration of excitation.
      dursource_calc = dursource(w_fa, fa, w_fb, fb)
      durpath_calc = durpath(rmod, nknots, rdur, dur, slast, 
     :                       numsource, amag)
      durex = dursource_calc + durpath_calc 

      if(indxwind .eq. 0) then        ! BOX WINDOW

!        durex = dursource(w_fa, fa, w_fb, fb)+
!     :          durpath(rmod, nknots, rdur, dur, slast, numsource, amag)

        if (durex < dt) then
          print *,' durex (= ', durex,
     :      ') < dt (= ', dt,
     :      '); QUIT!!!'
          stop
        else
          nmotion = durex/dt
        end if
        
        ntaper = ifix(taper*nmotion)  ! Increase duration of motion by tapers 
                                      ! (figure 1/2 front and back):
                                      ! taper is fractional, not percent
        nmotion = nmotion + ntaper  
        
! Set tb, te, tw to 0.0 is a box window, because if not set to a value,
! a run time error is returned when compiled with the -chk switch
        tb = 0.0
        te = 0.0
        tw = 0.0

      else                           ! EXPONENTIAL WINDOW

!        durex = dursource(w_fa, fa, w_fb, fb)+
!     :          durpath(rmod, nknots, rdur, dur, slast, numsource, amag)
        tb = durex
        te = f_tb2te * tb
        tw = f_te_xtnd * te
        
        if (tw < dt) then
          print *,' tw (= ', tw,
     :      ') < dt (= ', dt,
     :      '); QUIT!!!'
          stop
        else
          nmotion = tw/dt
        end if
        
      end if

! Calculate nstart and nstop, depending on tshift and if a
!   low-cut filter is used.

      if (fcut .eq. 0.0) then
        tfpad = 0.0
      else
        tfpad = 1.5 * (norder/2) / fcut ! (Converse, USGS OFR 92-296A, p. 2-3)
      end if

      if (tfpad .gt. tshift) tshift = tfpad

      nstart = tshift/dt + 1

      nstop = nstart + nmotion

! Calculate npts, depending on tshift, dur_fctr, and if a
!   low-cut filter is used.

! compute smallest power of two for which the resulting duration is greater
! than or equal to tsimdur:

      tsimdur = nstart*dt + dur_fctr * nmotion * dt + tfpad

      npw2 = 2.0**ifix(alog10(tsimdur/dt + 1.0)/alog10(2.0))
      if ( (npw2-1)*dt .lt. tsimdur) npw2 = 2 * npw2

      return
      end
! ---------------------------------------------------- Get_Npts_For_TD_Calcs


!  ------------------- BEGIN WIND_BOX -------------------------------
      function wind_box( i, nstart, nstop, ntaper)
!
! applies cosine tapered window.
! unit amplitude assumed
!
! written by D. M. Boore
!
! latest revision: 9/26/95
      real wind_box

      wind_box = 0.0
      if ( i .lt. nstart .or. i. gt. nstop) return
      wind_box = 1.0
      if ( i .ge. nstart+ntaper .and. i .le. nstop-ntaper ) return
!
      pi = 4.0 * atan(1.0)
!
      dum1 = (nstop+nstart)/2.0
      dum2 = (nstop-nstart-ntaper)/2.0
!
      wind_box = 0.5 * (1.0 - sin( pi*
     *  ( abs(float(i)-dum1) - dum2 ) /float(ntaper) ) )
      return
      end
!  ------------------- END WIND_BOX -------------------------------



!  ------------------- BEGIN WIND_EXP ---------------------------
      function  wind_exp( t, te, eps_w, eta_w, new_mr)
!
!     apply Sargoni and Hart (1974) window, with parameters
!     tw, eps (fraction of tw to reach peak), and
!     eta ( fraction of peak ampl. at tw).  See Boore (BSSA, 1983,
!     p. 1869).  Note that t can be larger than tw.

! Dates:
!         05/30/95 - Initially written by Dave Boore, based on a routine
!                    by G. Atkinson but with significant structural 
!                    changes and the taper to be a raised cosine taper
!         12/06/95 - Removed the taper and simplified the parameters in the
!                    calling list.  Also added a switch so that the window
!                    shape coefficients are only computed once.  This assumes
!                    that the shape coefficients will be the same for
!                    any run of the program... a good assumption.
!         12/28/95 - Used new_mr, set from driver, to control computation
!                    of b,c,a; before I used "firstcall", set in this 
!                    subprogram, but this gave an error if the driver
!                    looped over multiple values of m and r.
!         01/27/01 - Modified time series window computation (changed variable
!                    names, use normalized time)


      real wind_exp
      logical new_mr
      save a, b, c

      if (new_mr) then
        b = -eps_w * alog(eta_w)/
     :      (1. + eps_w*(alog(eps_w)-1.))
        c = b/eps_w
        a = (exp(1.0)/eps_w)**b
        new_mr = .false.
      end if

      wind_exp = 0.
      if( t .lt. 0.0) return
!
!     Apply Sargoni and Hart window.
!
      wind_exp = a*(t/te)**b * exp(-c*(t/te))

      return
      end
!------------------- END WIND_EXP ---------------------------


! ----------------------------- BEGIN FORK --------------------------
      SUBROUTINE FORK(LX,CX,SIGNI)
! FAST FOURIER                                  2/15/69
!                          LX
!    CX(K) = SQRT(1.0/LX)* SUM (CX(J)*EXP(2*PI*SIGNI*I*(J-1)*(K-1)/LX))
!                          J=1                        FOR K=1,2,...,LX
!
!  THE SCALING BETWEEN FFT AND EQUIVALENT CONTINUUM OUTPUTS
!  IS AS FOLLOWS.
!
!
!     GOING FROM TIME TO FREQUENCY:
!             F(W)=DT*SQRT(LX)*CX(K)
!
!                  WHERE W(K)=2.0*PI*(K-1)*DF

!                  and    DF = 1/(LX*DT)
!
!
!     GOING FROM FREQUENCY TO TIME, WHERE THE FREQUENCY
!     SPECTRUM IS GIVEN BY THE DIGITIZED CONTINUUM SPECTRUM:
!
!             F(T)=DF*SQRT(LX)*CX(K)
!
!                  WHERE T(K)=(K-1)*DT
!
!
!  THE RESULT OF THE SEQUENCE...TIME TO FREQUENCY,POSSIBLE MODIFICATIONS
!  OF THE SPECTRUM (FOR FILTERING,ETC.), BACK TO TIME...
!  REQUIRES NO SCALING.
!
!
!  THIS VERSION HAS A SLIGHT MODIFICATION TO SAVE SOME TIME...
!  IT TAKES THE FACTOR 3.1415926*SIGNI/L OUTSIDE A DO LOOP (D.BOORE 12/8
!  FOLLOWING A SUGGESTION BY HENRY SWANGER).
!

! Some brief notes on usage:

! "signi" is a real variable and should be called either with the value "+1.0"
! of "-1.0".  The particular value used depends on the conventions being used
! in the application (e.g., see Aki and Richards, 1980, Box 5.2, pp. 129--130).

! Time to frequency:
! In calling routine,
! 
!       do i = 1, lx
!         cx(i) = CMPLX(y(i), 0.0)
!       end do
!  where y(i) is the time series and lx is a power of 2
! 
!  After calling Fork with the complex array specified above, the following 
! symmetries exist:
! 
!        cx(1)        = dc value (f = 0 * df, where df = 1.0/(lx*dt))
!        cx(lx/2 + 1) = value at Nyquist (f = (lx/2+1-1)*df = 1.0/(2*dt))
!        cx(lx)       = CONJG(cx(2))
!        cx(lx-1)     = CONJG(cx(3))
!         |           =      |
!        cx(lx-i+2)   = CONJG(cx(i))
!         |           =      |
!        cx(lx/2+2)   = CONJG(cx(lx/2))
! 
! where "CONJG" is the Fortran complex conjugate intrinsic function
! 
! This symmetry MUST be preserved if modifications are made in the frequency 
! domain and another call to Fork (with a different sign for signi) is used
! to go back to the time domain.  If the symmetry is not preserved, then the
! time domain array will have nonzero imaginary components.  There is one case
! where advantage can be taken of this, and that is to find the Hilbert 
! transform and the window of a time series with only two calls to Fork (there 
! is a short note in BSSA {GET REFERENCE} discussing this trick, which amounts 
! to zeroing out the last half of the array and multiplying all but the dc and 
! Nyquist values by 2.0; in the time domain, REAL(cx(i)) and AIMAG(cx(i)) 
! contain the filtered (if a filter was applied) and Hilbert transform of the 
! filtered time series, respectively, while CABS(cx(i)) and ATAN2(AIMAG(cx(i)), 
! REAL(cx(i))) are the window and instantaneous phase of the filtered time 
! series, respectively.

! Some references:

! Farnbach, J.S. (1975). The complex envelope in seismic signal analysis, 
! BSSA 65, 951--962. 
! He states that the factor of 2 is applied for i = 2...npw2/2 (his indices 
! start at 0, I've added 1), which is different than the next reference:

! Mitra, S.K. (2001). Digital Signal Processing, McGraw-Hill, New York.
! He gives an algorithm on p. 794 (eq. 11.81), in which the factor of 2 is 
! applied from 0 frequency to just less than Nyquist.

! 
! The easiest way to ensure the proper symmetry is to zero out the
! last half of the array (as discussed above), but the following is what
! I usually use:  
! modify (filter) only half
! of the cx array:
! 
!       do i = 1, lx/2
!         cx(i) = filter(i)*cx(i)
!       end do
! 
! where "filter(i)" is a possibly complex filter function (and recall that 
! the frequency corresponding to i is f = float(i-1)*df).  After this, fill out
! the last half of the array using
!       
!       do i = lx/2+2, lx
!         cx(i) = CONJG(cx(lx+2-j))
!       end do
! 
! Note that nothing is done with the Nyquist value.  I assume (but am not sure!)
! that this value should be 0.0
! 
! Dates: xx/xx/xx - Written by Norm Brenner(?), Jon Claerbout(?)
!        12/21/00 - Replaced hardwired value of pi with pi evaluated here,
!                     and added comments regarding usage.  Also deleted
!                     dimension specification of cx(lx) and replace it with
!                     cx(*) in the type specification statement.  I also
!                     cleaned up the formatting of the subroutine.
!        08/28/01 - Added comment about variable "signi" being real, and 
!                   added "float" in equations for "sc" and "temp", although 
!                   not strictly required.
!        06/19/02 - Added some comments about computing envelopes and
!                   instantaneous frequencies
!        01/19/15 - Modernize code (get rid of go to statements)
             
      complex cx(*),carg,cexp,cw,ctemp

      pi = 4.0*atan(1.0)

      j=1
      sc=sqrt(1./real(lx))

      do i=1,lx
      
        if (i <= j) then
          ctemp=cx(j)*sc
          cx(j)=cx(i)*sc
          cx(i)=ctemp
        end if
        
        m=lx/2        
        
        DO
          if (j <= m) EXIT
          j=j-m
          m=m/2
          if (m < 1) EXIT
        END DO
        
        j = j + m
        
      end do

      l=1
      DO WHILE (l < lx)
        istep=2*l
        temp= pi * signi/real(l)

        do m=1,l
          carg=(0.,1.)*temp*(m-1)
          cw=cexp(carg)
          do i=m,lx,istep
            ctemp=cw*cx(i+l)
            cx(i+l)=cx(i)-ctemp
            cx(i)=cx(i)+ctemp
          end do
        end do

        l=istep
      END DO
 
! Previous code (as of 18 July 2010):
!      pi = 4.0*atan(1.0)
!
!      j=1
!      sc=sqrt(1./float(lx))
!
!      do i=1,lx
!        if(i.gt.j) go to 2
!        ctemp=cx(j)*sc
!        cx(j)=cx(i)*sc
!        cx(i)=ctemp
!2       m=lx/2
!3       if(j.le.m) go to 5
!        j=j-m
!        m=m/2
!        if(m.ge.1) go to 3
!5       j=j+m
!      end do
!
!      l=1
!6     istep=2*l
!      temp= pi * signi/float(l)
!
!      do m=1,l
!        carg=(0.,1.)*temp*(m-1)
!        cw=cexp(carg)
!        do i=m,lx,istep
!          ctemp=cw*cx(i+l)
!          cx(i+l)=cx(i)-ctemp
!          cx(i)=cx(i)+ctemp
!        end do
!      end do
!
!      l=istep
!      if(l.lt.lx) go to 6

      return
      end
! ----------------------------- END FORK --------------------------


!----------------- BEGIN MEAN -----------------------------
      subroutine mean(work, nstart, nstop, rmean)

! Also consider (no confidence limits, however):

!      SUBROUTINE avg_std(data,n,avg,std,itype)
      
! Computes mean and standard deviation for an array of data.
! This is based on the Numerical Recipes program avevar.
! Itype specifies the type of mean:
! itype = 1: arithmetic mean, std = standard deviation
! itype = 2: geometric mean, std = factor by which mean is divided
!                                  and multiplied to yield the
!                                  equivalent of minus and plus
!                                  one standard deviation


*  Dates: 01/22/96 - Written by D. Boore
      real work(*)
* find mean of the array:
      sum = 0.0
      do i = nstart, nstop
        sum = sum + work(i)
      end do
      rmean = sum/float(nstop-nstart+1)
      return
      end
!----------------- END MEAN -----------------------------


! ------------------------ begin dcdt -------------------
      subroutine dcdt (y,dt,npts,indx1,indx2,ldc,ldt)
!+
!  dcdt - fits dc or trend between indices indx1 and indx2.
!         then removes dc or detrends whole trace.
!         y is real, dt = delta t.
!         if remove dc, ldc = .true.
!         if detrend, ldt = .true.
!-

! Dates: 12/14/00 - Cleaned up formatting of original program
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      real y(*)
      logical ldc,ldt

      if (.not. ldc .and. .not. ldt) then
        return
      end if

!
!...fit dc and trend between indices indx1 and indx2.
      nsum = indx2-indx1+1
      sumx = 0.0
      sumx2 = 0.0
      sumy = 0.0
      sumxy = 0.0
      do i=indx1,indx2
         xsubi = (i-1)*dt
         sumxy = sumxy+xsubi*y(i)
         sumx = sumx+xsubi
         sumx2 = sumx2+xsubi*xsubi
         sumy = sumy+y(i)
      end do
!
!... remove dc.
      if (ldc) then
        avy = sumy/nsum
        do i=1,npts
          y(i) = y(i)-avy
        end do
! Debug
        write(*,'(a)') ' indx1, indx2, avy'
        write(*, *)      indx1, indx2, avy
! Debug



        return
      endif
!
!... detrend. see draper and smith, p. 10.
      if (ldt) then
        bxy = (sumxy-sumx*sumy/nsum)/(sumx2-sumx*sumx/nsum)
        axy = (sumy-bxy*sumx)/nsum
        qxy = dt*bxy
        do i=1,npts
          y(i) = y(i)-(axy+(i-1)*qxy)
        end do
        return
      endif
!
      return
      end
! ------------------------ end dcdt -------------------


! ----------------------------- BEGIN RMVTREND ----------------
      subroutine rmvtrend(y, n)

! Removes a straightline fit to first and last points, replacing
! the input array with the detrended array

! Dates: 02/09/99 - written by D. Boore
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)


      real y(*)

      y1 = y(1)
      y2 = y(n)
      slope = (y2 - y1)/float(n-1)

      do i = 1, n
        y(i) = y(i) - (y1 + slope*float(i-1))
      end do

      return
      end
! ----------------------------- END RMVTREND ----------------


!----------------- BEGIN AccSqInt -----------------------------
      subroutine accsqint(acc, npts, dt, rmv_trnd, a_sq_int)


! Form integral of acceleration squared, assuming that the acceleration
! is represented by straight lines connecting the digitized values.  This
! routine can be used to compute Arias intensity, defined as

!            Ixx = (pi/2g)*int(acc^2*dt), integrating from 0.0 to the total
!  duration of the record.  The units of Ixx are 
!  velocity [ l^(-1)t^2*(l/t^2)^2*t ] =  l^(-1+2)*t^(2-4+1) = l*t^(-1) = l/t

! Be sure to use consistent units for the acceleration of gravity (g) and acc.
! I am not sure what is conventionally used, but Wilson (USGS OFR 93-556) 
! uses m/sec.

! Dates: 01/13/99 - Written by D.M. Boore
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      real a_sq_int(*), acc(*)
      logical rmv_trnd
      double precision cum, a1, a2, ddt_3

      if (rmv_trnd) then      
! remove trend first
        call dcdt(acc, dt, npts, 1, npts, .false., .true.)
      end if

! compute integral of squared acceleration (assume a_sq_int = 0 for first point)

      ddt_3 = dble(dt/3)

      cum = 0.0

      a_sq_int(1) = sngl(cum)
      do j=2,npts
        a1 = acc(j-1)
        a2 = acc(j)
        cum = cum + (a1**2+a1*a2+a2**2)*ddt_3
        a_sq_int(j) = sngl(cum)
      end do

! high pass filter the velocity (may want to allow this in a future version;
! as it is, the acceleration time series can be filtered, so there is no need
! to do it again).

      return
      end
!----------------- END AccSqInt -----------------------------


!------------------- BEGIN AVGSQ_REALFT -------------------------------
* Dates: 01/22/96 - Written by D.M. Boore
      subroutine avgsq_realft( s, npts, avgsqamp)
      real s(*)
      sum=0.
      do j = 2, npts/2                   !don't include the dc or Nyquist values
        sum=sum + s(2*j-1)**2 + s(2*j)**2 ! odd, even = real, imag spect
      end do
      avgsqamp = sum/float(npts/2 - 1)
      return
      end
!  ------------------- END AVGSQ_REALFT -------------------------------



!----------------- BEGIN Acc2VD -----------------------------
      subroutine acc2vd(acc, npts, dt, rmv_trnd, vel0, dis0, vel, dis)


* Compute velocity and displacement time series from acceleration,
* assuming that the acceleration
* is represented by straight lines connecting the digitized values.

* Dates: 02/09/99 - Written by D.M. Boore
*        01/07/00 - Added initial velocity and displacements (v0, d0).
*                   This also requires the addition of a linear trend in
*                   displacement.
*                   Also, Bill Joyner, Chris Stephens, and I considered how to
*                   handle the first point.  Assuming v0 = 0, BAP uses a 
*                   trapezoidal rule such that the first vel(1) = 0.5*dt*a(1),
*                   but in acc2vd, vel(1) = 0.  In effect, BAP starts the
*                   integration at -dt rather than 0.0, with the result that
*                   because vel(1) depends on dt, vel(1) will change if the
*                   digitization interval is changed.  We agreed that this is 
*                   not correct.
*        12/18/02 - Some minor changes, such as specifying 0.0d0 rather 
*                   than 0.0 and using the function "dble" in various places
*        10/15/07 - Rename "d0" to "dis0" so as not to confuse with "d0"
*                   in "0.0d0".  For consistency, change "v0" to "vel0".
*                   More importantly, restructure the calculations so as not to
*                   lose the double-precision estimates of velocity when
*                   accumulating the displacement, and abandon the calculation 
*                   of disp based on integrating straightlines in acceleration, 
*                   using a simple trapezoidal rule instead.  These changes were
*                   made in an attempt to track down a drift in displacement
*                   for the HHE component of the KYTH recording of the
*                   Kythera earthquake recorded on a station of the EGELADOS
*                   velocity-sensor network.   The drift did not appear when I
*                   integrated the acceleration in Excel, and the SD computed 
*                   using SMC2RS also showed no indication of the drift (the
*                   long-period SD equals the peak displacement during the
*                   strong part of the shaking).   Unfortunately, the changes
*                   made today did not eliminate the drift.   My next check is
*                   to see if the 5 digit resolution in the smc files might
*                   be responsible (the data given to me by Andreas Skarlatoudis
*                   were in column files with 7 digits of resolution, with a maximum
*                   exponent of 9).  When I 
*                   opened the asc file made by smc2asc in Excel and computed dis, 
*                   I found the same trend as in the smc2vd file. This tells me
*                   that the problem is in the asc2smc conversion (but why the
*                   SD computed using the smc files shows no sign of the drift
*                   is a mystery to me).  I eventually confirmed that the problem
*                   is the limited resolution of the standard format; I have added
*                   an option to the smc utility programs to use a higher-resolution
*                   format (converting all of the utility programs will take some
*                   time---I am only doing those that I currently need right now).
!        02/06/11 - Added comments to make it clear that now use trapezoidal integration.
!                 - Delete "sngl(cumv)" and "sngl(cumd)" for vel(1) and dis(1)
!        02/07/11 - Call a subroutine to do the integration
!        06/01/11 - Delete the include statement for the integration routine; it should be placed in the programs that
!                   call acc2vd instead.  The reason for this is because
!                   the subroutine acc2vd is also included in \smsim\td_subs, and a compile
!                   error can occur if the \forprogs folder with the integration routine
!                   is not available.  This means that the trapezoidal_integration.for must be available in smsim.
!                   I guarantee this by including it in make_td_subs_file.bat, which should be called
!                   in time domain cl*.bat files.

      real acc(*), vel(*), dis(*) 
      logical rmv_trnd
      double precision cumv, cumd, a1, a2, v1, v2,
     : ddt, ddt_2, ddtdt_6

      if (rmv_trnd) then      
* remove trend first (straight line between first and last points)
* Note: acc is replaced with detrended time series
*        call dcdt(acc, dt, npts, 1, npts, .false., .true.)  ! old routine,
*                                                         ! gives steps at ends

         call rmvtrend(acc, npts)
      end if

* compute velocity and displacement

      call Trapezoidal_Integration(acc, npts, dt, vel0, vel)
      call Trapezoidal_Integration(vel, npts, dt, dis0, dis)
      
!      ddt     = dble(dt)
!      ddt_2   = 0.5d0 * dble(dt)
!      
!!      ddtdt_6 = dble(dt)*dble(dt)/6.0d0
!
!      cumv = 0.0d0
!      cumd = 0.0d0
!
!      v1 = 0.0d0
!      v2 = 0.0d0
!      
!!      vel(1) = sngl(cumv) + vel0
!      dis(1) = sngl(cumd) + dis0
!
!      vel(1) = vel0
!      dis(1) = dis0
!
!      do j=2,npts
!      
!        a1 = dble(acc(j-1))
!        a2 = dble(acc(j))
!        
!        cumv = cumv + (a1 + a2)*ddt_2
!        v1 = v2
!        v2 = cumv + dble(vel0)  ! the vel0 term is the constant of integration
!        vel(j) = sngl(v2)
!        
!        cumd = cumd + (v1 + v2)*ddt_2
!        dis(j) = sngl(cumd) + dis0  ! the dis0 term is the constant of integration
!        
!! Legacy code, when I used analytical formulas based
!! on representing the acceleration as a series of straightline segments.
!!        cumd = cumd + v1*ddt + (2.0*a1 + a2)*ddtdt_6
!        dis(j) = sngl(cumd) + dis0  ! linear term accounted for in v1*ddt
!        dis(j) = sngl(cumd) + v0*float(j-1)*dt + dis0
!
!      end do

      return
      end
!----------------- END Acc2VD -----------------------------



!----------------- BEGIN Trapezoidal_Integration -----------------------------
      subroutine Trapezoidal_Integration(y, npts, dt, yint_0, yint)


* Integrate a time series y with initial condition yint_0

 
* Dates: 02/07/11 - Written by D.M. Boore, patterned after acc2vd.for
      real y(*), yint(*) 
      double precision cumv, y1, y2, v1, v2,
     : ddt, ddt_2 

 
* Integrate

      ddt     = dble(dt)
      ddt_2   = 0.5d0 * dble(dt)
 
      cumv = 0.0d0
 
      v1 = 0.0d0
      v2 = 0.0d0
      
      yint(1) = yint_0
 
      do j=2,npts
      
        y1 = dble(y(j-1))
        y2 = dble(y(j))
        
        cumv = cumv + (y1 + y2)*ddt_2
        v1 = v2
        v2 = cumv + dble(yint_0)  ! the yint_0 term is the constant of integration
        yint(j) = sngl(v2)
        
      end do

      return
      end
!----------------- END Trapezoidal_Integration -----------------------------


!----------------- BEGIN RD_CALC -----------------------------
      subroutine rd_calc(acc,na,omega,damp,dt,rd, d0, v0)
* This is a modified version of "Quake.For", originally
* written by J.M. Roesset in 1971 and modified by
* Stavros A. Anagnostopoulos, Oct. 1986.  The formulation is that of
* Nigam and Jennings (BSSA, v. 59, 909-922, 1969).  This modification 
* eliminates the computation of the relative velocity and absolute 
* acceleration; it returns only the relative displacement.  

*   acc = acceleration time series
*    na = length of time series
* omega = 2*pi/per
*  damp = fractional damping (e.g., 0.05)
*    dt = time spacing of input
*    rd = relative displacement of oscillator
* d0,v0 = initial displacement and velocity (usually set to 0.0)

* Dates: 05/06/95 - Modified by David M. Boore
*        04/15/96 - Changed name to RD_CALC and added comment lines
*                   indicating changes needed for storing the oscillator 
*                   time series and computing the relative velocity and 
*                   absolute acceleration
!        03/11/01 - Double precision version of Rd_Calc
!        03/14/01 - Changed name back to rd_calc and added d0, v0
*        01/31/03 - Moved implicit statement before the type declarations
!        10/22/09 - Initial variable assignments and iteration loop modified 
!                   to double-precision (Chris Stephens)

      implicit real*8 (a - h, o - z)

      real*4 acc(*)

      real*4 omega, damp, dt, rd, d0, v0
 
      omt=dble(omega)*dble(dt)
      d2=1.d0-dble(damp)*dble(damp)
      d2=dsqrt(d2)
      bom=dble(damp)*dble(omega)
*      d3 = 2.*bom                 ! for aa
      omd=dble(omega)*d2
      om2=dble(omega)*dble(omega)
      omdt=omd*dble(dt)
      c1=1.0d0/om2
      c2=2.0d0*damp/(om2*omt)
      c3=c1+c2
      c4=1./(dble(omega)*omt)
      ss=dsin(omdt)
      cc=dcos(omdt)
      bomt=dble(damp)*omt
      ee=dexp(-bomt)
      ss=ss*ee
      cc=cc*ee
      s1=ss/omd
      s2=s1*bom
      s3=s2+cc
      a11=s3
      a12=s1
      a21=-om2*s1
      a22=cc-s2
      s4=c4*(1.0d0-s3)
      s5=s1*c4+c2
      b11=s3*c3-s5
      b12=-c2*s3+s5-c1
      b21=-s1+s4
      b22=-s4
      rd=0.
*      rv = 0.                           ! for rv
*      aa = 0.                           ! for aa
      n1=na-1
      y=    -dble(d0)
      ydot= -dble(v0) ! negative because forcing function is -acc
!      y=0.
!      ydot=0.
      do i=1,n1
        y1=a11*y+a12*ydot+b11*dble(acc(i))+b12*dble(acc(i+1))
        ydot=a21*y+a22*ydot+b21*dble(acc(i))+b22*dble(acc(i+1))
        y=y1    ! y is the oscillator output at time corresponding to index i
        z=dabs(y)
        if (z.gt.rd) rd=z
*        z1 = dabs(ydot)                   ! for rv
*        if (z1.gt.rv) rv = z1            ! for rv
*        ra = -d3*ydot -om2*y1            ! for aa
*        z2 = dabs(ra)                     ! for aa
*        if (z2.gt.aa) aa = z2            ! for aa
      end do
      return
      end
!----------------- END RD_CALC -----------------------------



! ----------------------------------------------------------------- begin rscalc_interp_acc
      subroutine rscalc_interp_acc(acc, na, omega, damp_in, dt_in,
     :                             rd, rv, aa)
        
!-----------------------------------------------------------------------
! This version does not return response time series.

! Dates: 03/04/10 - Program rsp obtained from B. Chiou.  cmpmax written by
!                   I. Idriss; ucmpmx by R. Youngs.  D. Boore changed the input 
!                   parameters to be equivalent to rdrvaa.for
!        03/05/10 - Substituted rdrvaa for cmpmax
!        03/06/10 - Renamed from rsp_rdrvaa_ucmpmx to rscalc_interp_acc.
!                 - Renamed subroutine ucmpmx to icmpmx ("i" for "interpolate
!                   acceleration) and modified icmpmx.
!        08/13/12 - Norm Abrahamson suggests doing a more exact interpolation 
!                   when period < n*dt (n=10 here).   He suggests interpolating
!                   by adding zeros in the frequency domain, figuring
!                   out at the beginning what will be the shortest period desired,
!                   interpolating the input time series accordingly, and then feeding 
!                   this into rdrvaa.    This requires a major restructuring of this 
!                   subroutine.  I will not do this yet; I just wanted to write
!                   down the suggested revision someplace.
!        10/09/12 - Following up on the last comment, the interpolation is in the driver
!                   smc_interpolate_time_series_using_fork.for, and it uses 
!                   interpolate_time_series_using_fork.for.  I have not incorporated
!                   it yet into programs such as smc2rs, smc2rs2, or blpadflt.
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)
!        05/16/15 - Use Implicit None and replace real*4 and double precision with modern equivalents
!                 - Get rvrdaa.for via an include statement rather than including it in this file
!                   in order to get the most recent version (I had done this before, but for
!                   some reason had copied rvrdaa.for to the bottom of this file, probably
!                   to simplify the distribution of the program).

      implicit none

      real(4) :: acc(*), omega, damp_in, dt_in, 
     :       rd, rv, aa, d0, v0 
      
      integer :: na, kg, kug, npr


      real(8) :: ug(:), pr, damp, dt, z(3)

      real(8) :: w, twopi
      
      allocatable :: ug
      
      integer :: i, nn

      dt = dble(dt_in)
      damp = dble(damp_in)
      
      kg = na
      
      allocate(ug(na))

          do i=1,kg
            ug(i) = dble(acc(i))
          enddo
!...
!... Compute response spectra
!...
           kug=kg-1
           
      w = dble(omega)
      
      twopi = 4.0d0*dasin(1.0d0)
      
      pr = twopi/w

      if(dt == 0.0d0 .or. pr < 10.0d0*dt) then
        call icmpmx(kug, ug, dt, pr, w, damp, z)
        rd = sngl(z(1))
        rv = sngl(z(2))
        aa = sngl(z(3))
      else
        d0 = 0.0
        v0 = 0.0
        call rdrvaa(acc,kg,omega,damp_in,dt_in,rd,rv,aa, d0, v0)
      endif
            
      deallocate(ug)

      return
      end

!-----------------------------------------------------------------------
      subroutine icmpmx(kug, ug, dt_in, pr, w, d, z)
      
! z(1) = SD
! z(2) = RV
! z(3) = AA

! Dates: 03/06/10 - Original program ucmpmx (written by Bob Youngs/I. Idriss)
!                   renamed icmpmx and modified to assume equal time spacing
!                   if the original and interpolated acceleration time
!                   series (the "u" in the original name referred to 
!                   "u"nequal spacing).
!        05/16/15 - Use Implicit None and replace real*4 and double precision with modern equivalents

      implicit none

! Input
      integer :: kug
      real(8) :: ug(*), pr, w, d, dt_in

! Output
      real(8) :: z(*)

! Working variables
      real(8) :: t(3), c(3), x(2,3)
      real(8) :: f1, f2, f3, f4, f5, f6, wd, w2, w3
      real(8) :: dt, e, g1, g2, h1, h2, dug, g, z1, z2, z3, z4
      real(8) :: a, b
      integer :: nn, i, k, ns, is, j
!
      nn=1
      wd=sqrt(1.-d*d)*w
      w2=w*w
      w3=w2*w
      DO i=1,3
        x(1,i)=0.
        z(i)=0.
      END DO
      
      f2=1./w2
      f3=d*w
      f4=1./wd
      f5=f3*f4
      f6=2.*f3
      
      ns= int(10.*dt_in/pr-0.01)+1   !! 05/05/2008
      dt=dt_in/real(ns)
      
      DO k=1,kug
      
        f1=2.*d/w3/dt
        e=dexp(-f3*dt)
        g1=e*dsin(wd*dt)
        g2=e*dcos(wd*dt)
        h1=wd*g2-f3*g1
        h2=wd*g1+f3*g2
        dug=(ug(k+1)-ug(k))/real(ns)
        g=ug(k)
        z1=f2*dug
        z3=f1*dug
        z4=z1/dt
        
        DO is=1,ns
          z2=f2*g
          b=x(1,1)+z2-z3
          a=f4*x(1,2)+f5*b+f4*z4
          x(2,1)=a*g1+b*g2+z3-z2-z1
          x(2,2)=a*h1-b*h2-z4
          x(2,3)=-f6*x(2,2)-w2*x(2,1)
          nn = nn + 1
          DO j=1,3
            c(j)=abs(x(2,j))
            IF (c(j) > z(j)) THEN
              z(j)=c(j)
            END IF
            x(1,j)=x(2,j)
          END DO
          
          g=g+dug
          
        END DO
      END DO
  
      RETURN
      END

!------------------------------
 
!      include '\forprogs\rdrvaa.for'

! ----------------------------------------------------------------- end rscalc_interp_acc

! NOTE: This program was incorporated into rscalc_interp_acc.for.
! Note that rdrvaa includes initial conditions in the argument
! list, while rscalc_interp_acc does not.

!----------------- BEGIN RDRVAA -----------------------------
      subroutine rdrvaa(acc,na,omega,damp,dt,rd,rv,aa, d0, v0)
! This is a modified version of "Quake.For", originally
! written by J.M. Roesset in 1971 and modified by
! Stavros A. Anagnostopoulos, Oct. 1986.  The formulation is that of
! Nigam and Jennings (BSSA, v. 59, 909-922, 1969).  

!   acc = acceleration time series
!    na = length of time series
! omega = 2*pi/per
!  damp = fractional damping (e.g., 0.05)
!    dt = time spacing of input
!    rd = relative displacement of oscillator
!    rv = relative velocity of oscillator
!    aa = absolute acceleration of oscillator
! d0,v0 = initial displacement and velocity (usually set to 0.0)

! Dates: 02/11/00 - Modified by David M. Boore, based on RD_CALC
!        03/11/01 - Double precision version
!        03/14/01 - Added d0, v0 (note on 05 March 2010: I recommend 
!                   that they not be used, by setting them to 0.0.  
!                   I've kept them as arguments in the subroutine call
!                   so that I do not have to modify programs that use
!                   this subroutine).                   
!        03/14/01 - Changed name back to rdrvaa
!        01/31/03 - Moved implicit statement before the type declarations
!        10/10/07 - Initial variable assignments and iteration loop modified 
!                   to double-precision (Chris Stephens)
!        03/05/10 - Delete old (single precision) lines of code
!        12/22/10 - Remove minus sign in front of the initialization of y, ydot. 
!                   The minus sign was a remnant of an earlier version where I did not
!                   understand the meaning of y and ydot.
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)
!        05/16/15 - Use Implicit None and replace real*4 and double precision with modern equivalents

      real(4) :: acc(*), omega, damp, dt, rd, rv, aa, d0, v0
      
      real(8) :: d2, bom, d3, omd, om2, c1, omt, omdt, c2, c3, c4, ss, 
     :           cc, bomt, ee, s1, s2, s3, a11, a12, a21, a22,
     :           s4, s5, b11, b12, b21, b22, y, ydot, y1, z, z1, z2, ra
     
      integer :: i, na
      
      
      d2=1.d0-dble(damp)*dble(damp)
      d2=dsqrt(d2)
      bom=dble(damp)*dble(omega)
      d3 = 2.d0*bom                 ! for aa
      omd=dble(omega)*d2
      om2=dble(omega)*dble(omega)
      c1=1.d0/om2
      
      omt=dble(omega)*dble(dt)
      omdt=omd*dble(dt)
      c2=2.d0*dble(damp)/(om2*omt)
      c3=c1+c2
      c4=1.d0/(dble(omega)*omt)
      ss=dsin(omdt)
      cc=dcos(omdt)
      bomt=dble(damp)*omt
      ee=dexp(-bomt)
      ss=ss*ee
      cc=cc*ee
      s1=ss/omd
      s2=s1*bom
      s3=s2+cc
      a11=s3
      a12=s1
      a21=-om2*s1
      a22=cc-s2
      s4=c4*(1.d0-s3)
      s5=s1*c4+c2
      b11=s3*c3-s5
      b12=-c2*s3+s5-c1
      b21=-s1+s4
      b22=-s4
      
      rd=0.
      rv = 0.                           ! for rv
      aa = 0.                           ! for aa
      
      y=    dble(d0)
      ydot= dble(v0)    
!      y=0.
!      ydot=0.

      do i=1, na-1
      
        y1=a11*y+a12*ydot+b11*dble(acc(i))+b12*dble(acc(i+1))
        ydot=a21*y+a22*ydot+b21*dble(acc(i))+b22*dble(acc(i+1))
        y=y1    ! y is the oscillator output at time corresponding to index i
        z=dabs(y)
        if (z > rd) rd=z
        z1 = dabs(ydot)                   ! for rv
        if (z1 > rv) rv = z1            ! for rv
        ra = -d3*ydot -om2*y1            ! for aa
        z2 = dabs(ra)                     ! for aa
        if (z2 > aa) aa = z2            ! for aa
        
      end do
      
      return
      end
!----------------- END RDRVAA -----------------------------



!------------------   Begin SMCRead   ----------------------------------
      subroutine SMCRead(f_in,  
     :                   tskip, tlength,
     :                   y, char_head, int_head, real_head, comments)

!  If tskip = 0.0, no data are skipped
!  If tlength = 0.0, only headers are read
!  If tlength < 0.0, read all data, less tskip
!  Otherwise tlength of data is read, after skipping tskip, unless 
!  tskip + tlength exceeds the length of the input time series, in which
!  case tlength is adjusted so that all data, less tskip, are read.  Thus
!  setting tlength to a large number (or setting it to a negative number)
!  guarantees that all data (minus tskip) are read.

! The program requires get_lun.for

! The output arrays char_head, int_head, real_head should be dimensioned
! as follows in the calling program:

!      character char_head(11)*80, comments(500)*80 ! "80" is the critical parameter
!      integer int_head(48)
!      real real_head(50)

!  Dates: 07/17/97 - Modified order of input parameters, to put input
!                    first (unit, file, tskip), followed by output.
!         12/18/97 - Read tlength
!         05/19/98 - Determine unit from within program; remove "unit"
!                    from argument list
!         03/09/99 - Change logic to determine amount of record to
!                    read and allow reading of all record, minus tskip,
!                    by setting tlength < 0.0.
!         05/18/99 - Deleted npts, sps from input parameter list.  Added 
!                    character array with comments string (allow for 
!                    50 comments).
!         12/16/99 - Renamed from ReadSMC
!         05/10/00 - Replaced
!                        tlength = float(npts_out/sps)
!                    with
!                        tlength = float(npts_out)/sps
!                    (the error and correction was pointed out by Wang Guoquan
!                    in email on 05/06/2000)
!         01/04/01 - Check for unevenly spaced data (sps = rnull) and
!                    return 0.0 in y
!         02/07/01 - If tlength = 0, only read headers
!         12/14/01 - Fixed logic for case when tlength < 0 and tskip > 0.
!                    Also, program used to reset tlength, but it does not do
!                    that now.  The user can compute tlength as 
!                    float(int_head(17))/real_head(2)
!         01/01/05 - Use "int" in calculation of npts (do not use
!                    implicit conversion from real to integer)
!         10/15/07 - Use higher precision input format if int_head(47) = 8
!         04/04/10 - Add call to trim_c, and use nc_f_in.
!         10/20/15 - Changed format of high precision input.
!                  - This was not necessary, as the old format statement can read
!                    either e14.7 or e14.7e1 (the new format being used in smcwrite for
!                    high precision).
!         12/30/15 - Replaced "int" with "nint" when computing nskip and npts2read
!                  - A few modernizations of syntax.

      real real_head(*)
      integer int_head(*)
      character*80 char_head(*), comments(*)
      character f_in*(*) 
      real y(*)

      
      
      call get_lun(nu)
      call trim_c(f_in, nc_f_in)
      open(unit= nu, file=f_in(1:nc_f_in), status='unknown')

      do i = 1, 11
        read(nu, '(a)') char_head(i)
      end do

      read(nu, '(8I10)') (int_head(i), i=1, 48)

      read(nu, '(5e15.7)') (real_head(i), i = 1, 50)

      sps = real_head(2)

      do i = 1, int_head(16)
        read(nu,'(a)') comments(i)
      end do

      if(real_head(2) == real_head(1)) then   ! unevenly sampled data

         int_head(17) = 2
         do i = 1, int_head(17)
           y(i) = 0.0
         end do

      else

! Skip into the trace:

        nskip = nint(tskip * sps)
!        nskip = int(tskip * sps)

! How much record to read?

        npts_in = int_head(17)
        npts2read = nint(tlength * sps)
!        npts2read = int(tlength * sps)

        if (tlength < 0.0) then  
          npts_out = npts_in - nskip
        else if (nskip + npts2read <= npts_in) then
          npts_out = npts2read
        else
          npts_out = npts_in - nskip
!          tlength = float(npts_out)/sps
        end if

        int_head(17) = npts_out
      
        if (tlength == 0.0) then
          return    ! only read headers
        else
          if (int_head(47) == 8) then
            read(nu, '(5(e14.7))') (y(i), i = 1, npts_out + nskip)
!            read(nu, '(5(e14.7e1))') (y(i), i = 1, npts_out + nskip)
          else
            read(nu, '(8(1pe10.4e1))') (y(i), i = 1, npts_out + nskip)
          end if
          do i = 1, npts_out
            y(i) = y( i + nskip)
          end do
        end if

      end if

      close(unit=nu)

      return
      end
!------------------   End SMCRead   ----------------------------------





!------------------   Begin SMCWrite  ----------------------------------
      subroutine SMCWrite(f_out, precision,
     :                    y, char_head, itype, 
     :                    int_head, real_head, comments)

!  Reformats a time series 
!  into SMC format (the format used on the CD-ROM)

! If itype < 0, the first text header is given by char_head(1) and
!               not set to one of the standard headers
!    itype = 0 for 'UNKNOWN'
!    itype = 1 for 'UNCORRECTED ACCELEROGRAM'
!    itype = 2 for 'CORRECTED ACCELEROGRAM'
!    itype = 3 for 'VELOCITY'
!    itype = 4 for 'DISPLACEMENT'
!    itype = 5 for 'RESPONSE SPECTRA'

! The program requires:
!        get_lun.for
!        mnmaxidx.for

! The arrays char_head, int_head, real_head must be dimensioned
! by the calling program as follows:
!
!      character char_head(11)*80, comments(N)*80 ! see "N" below
!      integer int_head(48)
!      real real_head(50)

! Note that 
!      int_head(17) = npts
!      real_head(2) = sps
!      int_head(16) = ncomments (dimension array "comments" accordingly
!                                so that N is a number > = ncomments)
!      I have found that if no comments are included, BAP cannot process
!      the resulting file.  For this reason, be sure that at least one comment
!      is included (ncomments = 1, comments(1) = '|').
!
! default values for int_head = -32768
!                    real_head = 1.7e+38

! The minimum required input for char_head is to set each of the 11 variables
! to "*" as in:
!  do i = 1, 11
!    char_head(i) = '*'
!  end do


! Dates: 05/13/99 - revised by D. Boore
!        05/18/99 - continued, adding the comments string array
!        12/15/99 - Renamed from WriteSMC to SMCWrite, and added
!                   itype to argument list and find and write peak
!                   accelerations (itype < 0 to echo input char_head(1)).
!        01/29/00 - Added comments regarding dimensioning of input variables
!        02/18/00 - Substitute 0.0 if abs(y) < 1.0e-09 (otherwise, the field
!                   is stored as ********, because an exponent of -10 is
!                   larger than allowed by the format statement).
!        02/25/00 - Changed output format of peak motion in text headers from
!                   1pe10.3 to 1pe9.3e1
!        02/27/01 - Set char_head(7)(42:60) to blanks before filling with
!                   peak motion
!        08/22/01 - Added comment about minimum requirement for char_head
!        10/15/07 - Use high precision format if precision=HIGH
!        04/24/09 - Changed format of read header output from 5e15.7 to 1p5e15.7
!        04/04/10 - Add call to trim_c, and use nc_f_out.
!        02/02/12 - Compute r_ep if eq and sta coords /= null
!        06/24/13 - Replaced ".eq." with "==", etc.
!                 - Replaced 1pex.x with esx.x
!                 - In writing real_head, revert to the format 5e15.7 rather than 5es15.7,
!                   as the latter might imply more precision than available in the
!                   single precision numbers.
!        10/20/15 - Change high precision output format to include blanks between
!                   numbers (as suggested by Chris Stephens)
!        10/24/00 - For both output precision options substitute 0.0 if abs(y) < 1.0e-09 (otherwise, the field
!                   is stored as ********, because an exponent of -10 is
!                   larger than allowed by the format statement).

      real real_head(*)
      integer int_head(*)
      character*80 char_head(*), comments(*)
      character f_out*(*), chead1(6)*26, precision*(*)
      real y(*)
      data chead1/
     : '0 UNKNOWN                 ', '1 UNCORRECTED ACCELEROGRAM',
     : '2 CORRECTED ACCELEROGRAM  ', '3 VELOCITY                ',
     : '4 DISPLACEMENT            ', '5 RESPONSE SPECTRA        '/

! Open output file:
      call get_lun(nu_out)
      call trim_c(f_out, nc_f_out)
      open(unit=nu_out, file=f_out(1:nc_f_out), status='unknown')

! Write name to screen:
      print *, 
     :  ' nu_out, Output file name = ', nu_out, ', ', f_out(1:nc_f_out)

! Get peak motions:
      
      call mnmaxidx(y, 1, int_head(17), 1,
     :             ymin, ymax, indx_min, indx_max) 
      peak_mtn = ymax
     
      if (abs(ymin) > abs(ymax)) peak_mtn = ymin
      
! Compute r_ep if eq, sta coords /= null:  NOTE: Need to include distaz in the calling program

      rnull = real_head(1)
      if (real_head(3) /= rnull .and. real_head(3) /= rnull) then  ! eq coords not null
        if (real_head(11) /= rnull .and. real_head(12) /= rnull) then  ! sta coords not null
          eqlat = real_head(3)
          eqlong = real_head(4)
          stalat = real_head(11)
          stalong = real_head(12)
          call distaz( -1.0, eqlat,eqlong,stalat,stalong,
     :    rdeg, rkm, az, baz)
          real_head(17) = rkm
        end if
      end if
          

! Write headers:

! First write the 11 lines of comments:

      if (itype < 0) then
        write(nu_out,'(a)') char_head(1)
      else
        write(nu_out,'(a)') chead1(itype+1)
      end if

      char_head(7)(34:41) = 'pk mtn ='    ! makes more sense than 'pk acc ='
      char_head(7)(42:60) = ' '
      write(char_head(7)(42:50),'(1pe9.3e1)') peak_mtn

      do i = 2, 11
        write(nu_out,'(a)') char_head(i)
      end do
          
      if (precision(1:4) == 'HIGH') then
        int_head(47) = 8
      else
        int_head(47) = int_head(1) ! null value
      end if
      
      write(nu_out, '(8I10)') (int_head(i), i=1, 48)

      dt = 1.0/real_head(2)
      real_head(29) = float(indx_max - 1) * dt
      real_head(30) = ymax
      real_head(31) = float(indx_min - 1) * dt
      real_head(32) = ymin

!      write(nu_out, '(5es15.7)') (real_head(i), i = 1, 50)
!DEBUG
!      write(nu_out, '(a, 1x,e15.7, 1x,f7.2, 1x,es12.5)') 
!     :   ' In SMCWRITE: real_head(2), dt = ', 
!     :     real_head(2), real_head(2), dt
!DEBUG
      write(nu_out, '(5e15.7)') (real_head(i), i = 1, 50)


      if (int_head(16) > 0) then
        do i = 1, int_head(16)
          write(nu_out,'(a)') comments(i)
        end do
      end if
          
        do i = 1, int_head(17)
          if (abs(y(i)) < 1.0e-09) y(i) = 0.0
        end do

      if (precision(1:4) == 'HIGH') then
        write(nu_out, '(5(es14.7e1))') (y(i), i = 1, int_head(17))
      else
!        do i = 1, int_head(17)
!          if (abs(y(i)) < 1.0e-09) y(i) = 0.0
!        end do
        write(nu_out, '(8(es10.4e1))') (y(i), i = 1, int_head(17))
      end if
      

! That should be it; close the output file.

      close(unit=nu_out)

      return
      end
!------------------   End SMCWrite   ----------------------------------


! ----------------------------------------------- acc2seismo_response
      subroutine acc2seismo_response(acc, nacc, dt, instr_type, 
     :           taper_beginning, taper_end,
     :           strip_pads,
     :           yout, nout)
     
! Compute instrument response in the frequency domain,
! using a poles and zeros representation of the instrument.

! instr_type = 'WA', 'WWSSN-SP', 'USGS-SP', 'PIDC-SP', ...


! Dates: 01/18/10 - Written by D. M. Boore, based on locut_fd
!        07/07/10 - poles_zeros_response has a logical input variable so that the
!                   phase can be set to zero for acausal filters (so that
!                   do not have to do two time domain filters)
!        07/19/10 - Call poles_zeros_values rather than assigning 
!                   values in this routine. Also, no longer use
!                   set_phase_to_0 as an argument in the call to
!                   poles_zeros_response.
 
      real acc(*), yout(*) 
      
      character instr_type*(*), strip_pads*1
      
      complex sp(20)
      character comments(200)*80
      
      
      complex CX(:), seismo_response
      allocatable :: cx 
      
      logical remove_mean_before, remove_mean_after,
     :  zcross_l, use_first, zcross_t, use_last, 
     :  replace_discarded_data_with_zeros
     
      call upstr(instr_type)
      call trim_c(instr_type,nc_instr_type)
      
      call poles_zeros_values(
     :      instr_type(1:nc_instr_type), gain, fgain, nz, np, sp)

! Determine factor to give proper gain 

      dum = 1.0
        
      idva = 2
      call poles_zeros_response(fgain, dum, nz, np, sp, idva, 
     :                              ampnorm, phase)
      gainfactor = gain/ampnorm
        
! Fill output array with input array:

      do i = 1, nacc
        yout(i) = acc(i)
      end do
      
* GET NPW2:      
      
      signnpw2 = +1.0
      call get_npw2(nacc,signnpw2,npw2) 
! DEBUG
!      write(*,*) ' In acc2seismo_response, after call to '//
!     :           'get_npw2, nacc, npw2 = ', nacc, npw2
! DEBUG
* sample rate and frequency spacing:

      sps = 1.0 / dt
      df_fft = sps/ float(npw2)

      npw2d2 = npw2 / 2

* Taper and add zeros:

      nzpad = npw2 - nacc      
      nlz = nzpad/2      
      ntz = nzpad - nlz

      remove_mean_before = .false.
      remove_mean_after = .false.
      zcross_l = .false.
      use_first = .false.
      zcross_t = .false.
      use_last = .false. 
      replace_discarded_data_with_zeros = .true.
      comments = ' '
      ncomments = 0      

      call smcpadf(yout, nacc, sps, nlz, ntz, 
     :  replace_discarded_data_with_zeros, 
     :  taper_beginning, taper_end, 
     :  nout, izcl, izct, 
     :  remove_mean_before, remove_mean_after, 
     :  zcross_l, use_first, zcross_t, use_last,
     :  comments, ncomments)  
     
      if (nout /= npw2) then
        write(*,*) ' ERROR: nout = ', nout,'; /= npw2 = ', npw2
        stop
      end if      

* Allocate and initialize working array
      
      n_alloc = npw2
      allocate( cx(n_alloc) )
      
      cx = cmplx(0.0,0.0)
      
c Fill working complex array

      do j = 1, npw2   
        cx(j)=cmplx(yout(j), 0.0)
      end do

c     FFT to get spectrum

      call fork(npw2,cx,-1.0)

      do j = 1, npw2d2
* Apply filter
        f = float(j-1)*df_fft
      idva = 2
        call poles_zeros_response(f, gainfactor, nz, np, sp, idva, 
     :                              amp, phase)
      if (instr_type(1:nc_instr_type) == 'PIDC-SP') then
          phase = 0.0
        end if
        seismo_response = cmplx(amp*cos(phase), amp*sin(phase))
        cx(j) = seismo_response * cx(j)
      end do
* Set Nyquist value to 0.0
      cx(npw2d2+1) = 0.0
* Fill in other half of array (for a real output)
      do j = 2, npw2d2
        cx(npw2-j+2) = conjg(cx(j))
      end do
      
* Go back to time domain:

      call fork(npw2,cx,+1.0)
      
      if (strip_pads == 'Y') then
        nstartm1 = nlz
        nout = nacc      
      else if (strip_pads == 'N') then 
        nstartm1 = 0
        nout = npw2
      else
        write(*,*) ' strip_pads = '//strip_pads//
     :             '-- not Y or N; QUITTING!'
        stop
      end if

      do j = 1, nout
        yout(j) = real(cx(j+nstartm1))  ! no scaling is needed with FORK
      end do
      
      deallocate(cx)

      return
      end
! ----------------------------------------------- acc2seismo_response


! ------------------------------------------------------------- Get_NPW2
      subroutine get_npw2(npts,signnpw2,npw2)

! Find npw2 (less than npts if signnpw2 < 0)

! Dates: 12/12/00 - Written by D. Boore
!        04/04/10 - Correct error that it does not return
!                   npw2 = npts, if npts is a power of 2.
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      npw2_exp = int( alog(float(npts))/alog(2.0) )
      if (signnpw2 < 0.0) then
        npw2 = 2.0**npw2_exp
      else 
        npw2_temp = 2.0**npw2_exp
        if (npw2_temp == npts) then 
          npw2 = npts
        else
          npw2 = 2.0**(npw2_exp+1)
        end if
      end if

      return
      end
! ------------------------------------------------------------- Get_NPW2


! ---------------------------------------------------------------- smcpadf
      subroutine smcpadf(y, npts_in, sps, nlz, ntz, 
     :  replace_discarded_data_with_zeros, 
     :  taper_beginning, taper_end, 
     :  npts_out, izcl, izct, 
     :  remove_mean_before, remove_mean_after, 
     :  zcross_l, use_first, zcross_t, use_last,
     :  comments, ncomments)    

* Here is the padding action taken for some combinations of input parameters:
*
* zcross_l  use_first  pad action (similar for zcross_t, use_last)
*     T         T         pad with 0.0
*     T         F         pad with 0.0
*     F         T         pad with work(1)
*     F         F         pad with 0.0
 
* Dates: 06/04/04 - Replaced tlz, ttz with nlz, ntz.  Also, allow the 
*                   discarded data to be replaced with zeros (thus 
*                   preserving the length of the input time series).
*        03/13/06 - Remove f_smc from smcpadf
*        11/03/06 - If zcross_l or zcross_t = F and use_first or 
*                   use_last = T, pad with values equal to the last points.
*                   Allow this option to deal with cases such as filtering
*                   a displacement time series with a low-frequency drift, 
*                   which will have a step offset if pad with zeros at the
*                   end.  Be careful in in using this option for the first
*                   points, however (in a future version I may eliminate
*                   the option for the beginning, as generally the leading
*                   pad should be zero).  The need for this option arose
*                   when I tried to filter a displacement obtained by 
*                   integrating a zeroth-order corrected acceleration.
*                   The results were different than filtering the 
*                   acceleration before integration.
*        01/24/07 - Store the snipped portion of y_in in a work array.  This
*                   is to be sure that I am not overwriting the input data
*                   when shifting the time series.  Probably the existing
*                   code was OK, but on reading it some years after writing
*                   it, I find it difficult to follow.  It is much better to
*                   make things a bit less efficient, but easier to follow.
*        01/24/07 - Added option to taper end of time series before adding 
*                   trailing zero pad.
*        07/15/07 - Added option to taper beginning of time series before 
*                   adding leading zero pad.
*        07/17/07 - Corrected a subtle error:  the names for the input 
*                   parameters taper_beginning and taper_end were being used
*                   for the variable containing the taper, and thus their
*                   values were being reset to the last taper value.   I now
*                   use "taper" as the variable containing the taper value.
*        03/24/08 - Write leading zpads as a time as well as n points.
*        01/18/10 - Add comments above about padding
!        04/04/10 - Correct error that pad_value_l and pad_value_t could be
!                   undefined in some cases.

  
      logical remove_mean_before, remove_mean_after,
     :  zcross_l, use_first, zcross_t, use_last, 
     :  replace_discarded_data_with_zeros

      real y(*)
      character comments(*)*80

      real work(:)
      allocatable :: work

      parameter (small=1.0e-10)
!      parameter (small=1.0e-05)

      pi = 4.0*atan(1.0)

!* DEBUG
!      write(*,'(a)') ' Inside smcpadf:'
!
!      write(*,'(a)') ' nlz, ntz = '
!      write(*, *)      nlz, ntz
!
!      write(*,'(a)') ' zcross_l, zcross_t = '
!      write(*, *)      zcross_l, zcross_t
!
!      write(*,'(a)') ' use_first, use_last = '
!      write(*, *)      use_first, use_last
!
!      write(*,'(a,1x,l1)') 
!     :      ' remove_mean_before = ', remove_mean_before
!
!      write(*,'(a,1x,l1)') 
!     :      ' remove_mean_after = ', remove_mean_after
!
      if (remove_mean_before) then        
        n_start = 1
        n_stop = npts_in
        icorr_cumavg_before = 0
        call Get_Avg(y, n_start, n_stop, npts_in, 
     :                   sps, icorr_cumavg_before, avg_before)
        do i = n_start, n_stop
          y(i) = y(i) - avg_before
        end do
        n_start_before = n_start
        n_stop_before = n_stop
      end if

* Get peak motion:
      
      call mnmaxidx(y, 1, npts_in, 1,
     :             ymin, ymax, indx_min, indx_max) 
      peak_mtn = ymax
      if (abs(ymin) .gt. abs(ymax)) peak_mtn = ymin

* Define fraction small of peak:
      smidge = small * abs(peak_mtn)


      if (zcross_l) then ! Find first zero crossing
        izcl = -1
        if (abs(y(1)) .lt. smidge) then
          izcl = 1
        else
          do i = 2, npts_in
            if (y(i)*y(1) .lt. 0.0) then   ! a zero crossing has been found
* Now determine if the point just before the zero crossing is smaller in 
* amplitude than the point after.  If so, let it be the first point unless 
* the first value is smaller than this value, in which case y(1) is the 
* first value.
              if (abs(y(i)) .lt. abs(y(i-1))) then
                izcl = i
              else
                izcl = i - 1
                if (use_first) then
                  if (abs(y(i-1)) .gt. abs(y(1))) then
                    izcl = 1
                  end if
                end if
              end if
              go to 2345
            end if
          end do
2345      continue
        end if
      else
        izcl = 1
      end if
      if (zcross_t) then ! Find last zero crossing
        izct = -1
        if (abs(y(npts_in)) .lt. smidge) then
          izct = npts_in
        else
          do i = npts_in -1, 1, -1
            if (y(i)*y(npts_in) .lt. 0.0) then ! a zero crossing has been found
* Now determine if the point just after the zero crossing is smaller in 
* amplitude than the point before.  If so, let it be the last point unless 
* the last value is smaller than this value, in which case y(npts_in) is the 
* last value.
              if (abs(y(i)) .lt. abs(y(i+1))) then
                izct = i
              else
                izct = i+1
                if (use_last) then
                  if (abs(y(i+1)) .gt. abs(y(npts_in))) then
                    izct = npts_in
                  end if
                end if
              end if
              go to 5432
            end if
          end do
5432      continue
        end if
      else
        izct = npts_in
      end if

!      write(*,'(a)') ' izcl, izct = '
!      write(*,*)       izcl, izct

* izcl, izct = index in input smc file of zero crossings

* April's code is more compact, but I think mine is easier to read        
            
      if (replace_discarded_data_with_zeros) then
        nlznew = nlz + izcl - 1
        ntznew = ntz + npts_in - izct
      else
        nlznew = nlz
        ntznew = ntz
      end if

* Allocate a work array and save the snipped portion in this array

      npts_snip = izct - izcl + 1  ! number of original points between zeros
      allocate (work(npts_snip)) 
      do i = 1, npts_snip
        work(i) = y(i+izcl-1)
      end do
      
* Compute new npts_out:

      npts_out = npts_snip + nlznew + ntznew

* Indices of original data

        n_start = nlznew + 1
        n_stop = npts_snip + nlznew

* Compute and remove mean if specified:

      if (remove_mean_after) then        
        n_start4avg = 1
        n_stop4avg = npts_snip 
        icorr_cumavg_after = 1

        call Get_Avg(work, n_start4avg, n_stop4avg, npts_snip, 
     :                   sps, icorr_cumavg_after, avg_after)
        do i = 1, npts_snip
          work(i) = work(i) - avg_after
        end do
        n_start_after = n_start
        n_stop_after = n_stop
      end if

* Taper beginning, if desired:

      ntaper_beginning = taper_beginning*sps
      
      if (ntaper_beginning .gt. 0) then
      
!* DEBUG
!        print *,' ntaper_beginning = ', ntaper_beginning
!* DEBUG
      
        do i = 1, ntaper_beginning + 1
        
           ishift = i - (1 - ntaper_beginning)
           arg = pi*float(ishift)/float(ntaper_beginning)
           taper = 0.5*(1.0+cos(arg))
           
           work(i) = taper * work(i)
           
        end do
        
      end if



* Taper end, if desired:

      ntaper_end = taper_end*sps
      
      if (ntaper_end .gt. 0) then
      
!* DEBUG
!        print *,' ntaper_end = ', ntaper_end  
!* DEBUG
      
        do i = npts_snip - ntaper_end, npts_snip
        
           ishift = i - (npts_snip - ntaper_end)
           arg = pi*float(ishift)/float(ntaper_end)
           taper = 0.5*(1.0+cos(arg))
           
           work(i) = taper * work(i)
           
        end do
        
      end if



* Store work in shifted location in y

      do i = 1, npts_snip
        y(i + nlznew) = work(i)
      end do
      
* And pad with leading zeros:

      pad_value_l = 0.0
      if ( nlznew .gt. 0) then
        if ((.not. zcross_l) .and. use_first) then
          pad_value_l = work(1)
        else
          pad_value_l = 0.0
        end if
        
        do i = 1, nlznew
          y(i) = pad_value_l
        end do
      end if

* And pad with trailing zeros:

      pad_value_t = 0.0
      if ( ntznew .gt. 0) then
        if ((.not. zcross_t) .and. use_last) then
          pad_value_t = work(npts_snip)
        else
          pad_value_t = 0.0
        end if

        do i = 1, ntznew
          y(npts_snip + nlznew + i) = pad_value_t
        end do
      end if

      deallocate(work)

* Store  comments:
      ncomments = ncomments + 1
      write(comments(ncomments),'(a)') 
     :     '| Added pads:'
      if (remove_mean_before) then        
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i1,a,1x,1p,e11.4)') 
     :   '| Using icorr_cumavg = ', icorr_cumavg_before, 
     :   ', determined and removed the mean', avg_before
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i5,1x,a,1x,i5,a)') 
     :   '| between indices ', n_start_before, ' and ',
     :        n_stop_before, ' before padding with pad_value'
      else
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i1,a,1x,1p,e11.4)') 
     :   '| No mean removed before adding pad' 
      end if
      if (remove_mean_after) then        
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i1,a,1x,1p,e11.4)') 
     :   '| Using icorr_cumavg = ', icorr_cumavg_after, 
     :   ', determined and removed the mean', avg_after
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i5,1x,a,1x,i5,a)') 
     :   '| between indices ', n_start_after, ' and ',
     :        n_stop_after, ' after padding with pad_value'
      else
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i1,a,1x,1p,e11.4)') 
     :   '| No mean removed after adding pads' 
      end if
      
      if (ntaper_beginning .gt. 0) then
!* DEBUG
!        print *,' In write comments:'
!        print *,' ntaper_beginning = ', ntaper_beginning  
!* DEBUG
      
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i7,a)')
     :     '| Taper beginning ', ntaper_beginning,
     :     ' points before adding leading pad'
      end if
          
      if (ntaper_end .gt. 0) then
!* DEBUG
!        print *,' In write comments:'
!        print *,' ntaper_end = ', ntaper_end 
!* DEBUG
      
        ncomments = ncomments + 1
        write(comments(ncomments),'(a,1x,i7,a)')
     :     '| Taper last ', ntaper_end,
     :     ' points before adding trailing pad'
      end if
          
      ncomments = ncomments + 1
      if (replace_discarded_data_with_zeros) then
        write(comments(ncomments),'(a,1x,i7,a)') 
     :     '| The program replaced ', izcl - 1, 
     :      ' points from start of record with pad value'
      else
        write(comments(ncomments),'(a,1x,i7,a)') 
     :     '| The program discarded ', izcl - 1, 
     :      ' points from the start of the record'
      end if
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,i7,a,
     :                            f7.3,a)') 
     :     '|   and added ', nlz, ' leading values ( ', 
     :                     float(nlz)/sps, ' s), using'
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,l1,a,1x,l1)') 
     :     '|   zcross_l = ', zcross_l,
     :        ' and use_first = ', use_first
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,1p, e10.3)') 
     :     '|   and pad_value = ', pad_value_l
      ncomments = ncomments + 1
      if (replace_discarded_data_with_zeros) then
        write(comments(ncomments),'(a,1x,i7,a)') 
     :     '| The program replaced ', npts_in - izct, 
     :      ' points from end of record with pad_value'
      else
        write(comments(ncomments),'(a,1x,i7,a)') 
     :     '| The program discarded ', npts_in - izct, 
     :      ' points from the end of the record'
      end if
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,i7,a)') 
     :     '|   and added ', ntz, ' trailing values, using'
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,l1,a,1x,l1)') 
     :     '|   zcross_t = ', zcross_t,
     :        ' and use_last = ', use_last
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,1p, e10.3)') 
     :     '|   and pad_value = ', pad_value_t
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,i7)') 
     :     '| The total number of points in the input time'//
     :        ' series = ', npts_in
      ncomments = ncomments + 1
      write(comments(ncomments),'(a,1x,i7)') 
     :     '| The total number of points in the '//
     :       'pad-stripped series = ', npts_out - nlz - ntz

      return
      end
! ---------------------------------------------------------------- smcpadf



! -------------------------------------------------------------------- FILTER
      subroutine filter(a, npts, dt, flc, nroll_lc,
     :                  fhc, nroll_hc, icaus, d4notch)

* Butterworth filter time series stored in 'a', according to the following:
* if fhc = 0 = flc       : no filtering
* if fhc = 0             : low-cut filter
* if flc = 0             : high-cut filter
* if neither flc, fhc = 0: band-pass filter
* if flc = fhc /= 0, a notch filter, where the width parameter d is given by real(nslope_lc)*10**(-real(nslope_hc))
*          if (icaus .eq. 0) then   ! acausal
*            if (nslope_lc .lt. 4) nslope_lc = 4
*            if (nslope_hc .lt. 4) nslope_hc = 4
*            nslope_lc = 4 * int(float(nslope_lc)/4.0)
*            nslope_hc = 4 * int(float(nslope_hc)/4.0)
*            nroll_lc = nslope_lc/4
*            nroll_hc = nslope_hc/4
*          else                     ! causal
*            if (nslope_lc .lt. 2) nslope_lc = 2
*            if (nslope_hc .lt. 2) nslope_hc = 2
*            nslope_lc = 2 * int(float(nslope_lc)/2.0)
*            nslope_hc = 2 * int(float(nslope_hc)/2.0)
*            nroll_lc = nslope_lc/2
*            nroll_hc = nslope_hc/2
*          end if
*
** Dates: 09/11/00 - Written by D. Boore
**        02/04/01 - Added separate nroll for lc and hc filters; for
*                   the bandpass filter the max of nroll_lc and nroll_hc 
*                   is used.  In the future, the bandpass subroutine may be 
*                   changed to allow for separate rolloffs at low and high
*                   frequencies.
!        02/20/10 - Added notch filter, and the d parameter for this filter
      
      real a(*)

      if (fhc == 0.0 .and. flc == 0.0) then
        return
      else if (fhc == 0.0) then
        call locut(a, npts, flc, dt, nroll_lc, icaus)
      else if (flc == 0.0) then
        call hicut(a, npts, fhc, dt, nroll_hc, icaus)
      else if (flc == fhc .and. flc /= 0.0) then  ! notch
        if (icaus .eq. 0) then   ! acausal
          nslope_lc = 4 * nroll_lc
          nslope_hc = 4 * nroll_hc
        else                     ! causal
          nslope_lc = 2 * nroll_lc
          nslope_hc = 2 * nroll_hc
        end if
        d = d4notch
        call notch(a, npts, flc, dt, d, icaus)
      else                                        ! bandpass
        nroll_bp = max0(nroll_lc, nroll_hc)
        call band(a, npts, flc, fhc, dt, nroll_bp, icaus)
      end if

      return
      end
! -------------------------------------------------------------------- FILTER    

!* ---------------------------------------------------------------------- BAND
      subroutine band(s,nd,f1,f2,delt,nroll,icaus)
c  Butterworth bandpass filter order 2*nroll (nroll<=8) (see Kanasewich, 
c    Time Sequence Analysis in Geophysics, Third Edition, 
c    University of Alberta Press, 1981)
c  written by W.B. Joyner 01/07/97
c  causal if icaus.eq.1 - zero phase shift otherwise
c  s(j) input = the time series to be filtered - output = the 
c    filtered series
c  dimension of s(j) must be as least as large as the larger of 
c    the following:
c    nd+3.0*float(nroll)/(f1*delt)
c    nd+6.0*float(nroll)/((f2-f1)*delt)
c  nd = the number of points in the time series
c  f1, f2 = the cutoff frequencies
c  delt = the timestep

* Dates: xx/xx/xx - Written by Bill Joyner
*        09/12/00 - Changed "n" to "nroll" to eliminate confusion with
*                   Kanesewich, who uses "n" as the order (=2*nroll), and
*                   cleaned up appearance of code by using spaces, indents, etc.
*        09/12/00 - double precision statements added so that the output
*                   series has sufficient precision for double integration.
*        11/08/00 - Increased dimension of s from 50000 to 100000
*        02/14/01 - Modified/corrected real numbers function calls to 
*                   double precision - cds
*        02/22/01 - Removed dimension of s (it is up to the user to specify
*                   it properly)
!        06/26/14 - Adopted some minor suggested changes by Chris Stephens

      real s(*)
      real*8 fact(16),b1(16),b2(16)
      real*8 pi,w1,w2,xp,yp,x1,x2,y1,y2
      real*8 pre, pim, argre, argim, rho, theta, sjre, sjim
      real*8 bj, cj, con

      if(f1.eq.0..or.f1.ge.f2) return  ! Changed "f1.eq.f2" to "f1.ge.f2" on 06/26/14

      pi=4.0d0*datan(1.0d0)
      w1=2.0d0*pi*f1
      w1=2.0d0*dtan(w1*delt/2.0d0)/delt
      w2=2.0d0*pi*f2
      w2=2.0d0*dtan(w2*delt/2.0d0)/delt

      do k=1,nroll
        pre=-dsin(pi*dfloat(2*k-1)/dfloat(4*nroll))
        pim=dcos(pi*dfloat(2*k-1)/dfloat(4*nroll))
        argre=(pre**2-pim**2)*(w2-w1)**2/4.0d0-w1*w2
        argim=2.0d0*pre*pim*(w2-w1)**2/4.0d0
        rho=(argre**2+argim**2)**(1.0d0/4.0d0)
        theta=pi+datan2(argim,argre)/2.0d0
        do i=1,2
          sjre=pre*(w2-w1)/2.0d0+(-1)**i*rho*(-dsin(theta-pi/2.0d0))
          sjim=pim*(w2-w1)/2.0d0+(-1)**i*rho*dcos(theta-pi/2.0d0)
          bj=-2.0d0*sjre
          cj=sjre**2+sjim**2
          con=1.0d0/(2.0d0/delt+bj+cj*delt/2.0d0)
          fact(2*k+i-2)=(w2-w1)*con
          b1(2*k+i-2)=(cj*delt-4.0d0/delt)*con
          b2(2*k+i-2)=(2.0d0/delt-bj+cj*delt/2.0d0)*con
        end do
      end do

      np2=nd

      if(icaus.ne.1) then
        npad=3.0*float(nroll)/(f1*delt)
        if( npad .lt. 6.0*float(nroll)/((f2-f1)*delt) ) then
          npad=6.0*float(nroll)/((f2-f1)*delt)
        end if
        np1=nd+1
        np2=nd+npad
        do j=np1,np2
          s(j)=0.0
        end do
      end if

      do k=1,2*nroll
        x1=0.0d0
        x2=0.0d0
        y1=0.0d0
        y2=0.0d0
        do j=1,np2
          xp=s(j)
          yp=fact(k)*(xp-x2)-b1(k)*y1-b2(k)*y2
          s(j)=yp
          y2=y1
          y1=yp
          x2=x1
          x1=xp
        end do
      end do

      if(icaus.ne.1) then
        do k=1,2*nroll
          x1=0.0d0
          x2=0.0d0
          y1=0.0d0
          y2=0.0d0
          do j=1,np2
            xp=s(np2-j+1)
            yp=fact(k)*(xp-x2)-b1(k)*y1-b2(k)*y2
            s(np2-j+1)=yp
            y2=y1
            y1=yp
            x2=x1
            x1=xp
          end do
        end do
      end if

      return

      end
!* ---------------------------------------------------------------------- BAND


!* ------------------------ BEGIN NOTCH ------------------------
      subroutine notch(s,nd,fr,delt,d,icaus)
c  notch filter (see Kanasewich, Time Sequence Analysis in 
c    Geophysics, Third Edition, University of Alberta Press, 
c    1981; Shanks, Feb. 1967 Geophysics)
c  written by W. B. Joyner 01/07/97
c  causal if icaus.eq.1 - zero phase shift otherwise
c  s(j) input = the time series to be filtered - output = the 
c    filtered series
c  dimension of s(j) must be at least as large as
c    nd+3.0*pi/d
c  nd = the number of points in the time series
c  fr = the reject frequency
c  delt = the timestep
c  d = parameter << 1 - the smaller d the narrower the notch - 
c    the value 0.01 is suggested for first trial
* Dates: 01/07/97 - Written by W. Joyner
*        07/20/01 - Option to return (no filtering) if fr < 0.0
!        03/20/10 - Tidied up by D. Boore

      real s(*)
      pi=4.0*atan(1.0)
      
      IF (fr < 0.0) THEN
        return
      END IF
      
      r = 1.0+d
      w = pi*fr*delt*2.0
      cosw = cos(w)
      fact = (1.0+2.0*cosw/r+1.0/r**2)/(2.0*(1.0+cosw))
      a1 = -2.0*cosw
      b1 = -2.0*cosw/r
      b2 = 1.0/r**2
      np2 = nd
      
      IF (icaus /= 1) THEN
      
        npad = 3.0*pi/d
        np1 = nd+1
        np2 = nd+npad
        
        DO j = np1, np2
          s(j) = 0.0
        END DO
        
      END IF
      
      x1 =0.0
      x2 =0.0
      y1 =0.0
      y2 =0.0
      
      DO j = 1, np2
      
        xp  = s(j)
        yp  = fact*(xp+a1*x1+x2)-b1*y1-b2*y2
        s(j) = yp
        y2 = y1
        y1 = yp
        x2 = x1
        x1 = xp
        
      END DO
      
      IF (icaus /= 1) THEN
      
        x1 = 0.0
        x2 = 0.0
        y1 = 0.0
        y2 = 0.0
        
        DO j = 1, np2 
        
          xp = s(np2-j+1)
          yp = fact*(xp+a1*x1+x2)-b1*y1-b2*y2
          s(np2-j+1) = yp
          y2 = y1
          y1 = yp
          x2 = x1
          x1 = xp
          
        END DO
        
      END IF
      
      RETURN
      
      END
!* ------------------------ END NOTCH ------------------------


!* ---------------------------------------------------------------------- LOCUT
      subroutine locut(s,nd,fcut,delt,nroll,icaus)

c  Butterworth locut filter order 2*nroll (nroll<=8) (see Kanasewich, 
c    Time Sequence Analysis in Geophysics, Third Edition, 
c    University of Alberta Press, 1981)
c  written by W. B. Joyner 01/07/97
c  causal if icaus.eq.1 - zero phase shift otherwise
c  s(j) input = the time series to be filtered - output = the 
c    filtered series
c  dimension of s(j) must be at least as large as 
c    nd+3.0*float(nroll)/(fcut*delt)
c  nd = the number of points in the time series
c  fcut = the cutoff frequency
c  delt = the timestep

* There is a fundamental difference in the shapes of a causal and acausal 
* filter.

* The response for a causal filter is given by eq. 15.8-6 in Kanasewich:
*    Y = sqrt((f/fcut)**(2*n)/(1+(f/fcut)**(2*n))), where n = 2*nroll
* The acausal filter is obtained by applying the causal filter twice.
* The consequence is that the response of the acausal filter is
*    Y =      (f/fcut)**(2*n)/(1+(f/fcut)**(2*n)),  where n = 2*nroll
* Note that for the same nroll as input, for f/fcut << 1
* the causal filter goes as
*    Y -> (f/fcut)**(2*nroll)
* whereas the acausal filter falls off more rapidly:
*    Y -> (f/fcut)**(4*nroll)
* Also note the difference when f = fcut:
*   causal:  Y = 1/sqrt(2)
*  acausal:  Y = 1/2
* This is true no matter what value is chosen for nroll.  Because of this,
* it is not possible to make the response of the causal and acausal filters the
* same.  In particular, caution should be used if nroll is chosen so as to
* make the low-frequency asymptotes the same.  This requires nroll for the 
* acausal filter to be half that of the causal filter.  The problem is that
* the response for frequencies higher than fcut will be reduced for the
* acausal as compared to the causal filter.  


* Dates: xx/xx/xx - Written by Bill Joyner
*        09/30/97 - double precision statements added so that the output
*                   series has sufficient precision for double integration.
*        12/17/99 - D. Boore added check for fcut = 0.0, in which case
*                   no filter is applied.  He also cleaned up the
*                   appearance of the code (indented statements in
*                   loops, etc.)
*        02/04/00 - Changed "n" to "nroll" to eliminate confusion with
*                   Kanesewich, who uses "n" as the order (=2*nroll)
*        09/12/00 - Made trig functions double precision
*        11/08/00 - Increased dimension of s from 50000 to 100000
*        02/14/01 - Modified/corrected real numbers function calls to 
*                   double precision - cds
*        02/15/01 - Added response of acausal filter in the comments
*        02/22/01 - Removed dimension of s (it is up to the user to specify
*                   it properly)
*        02/22/01 - Added more discussion about the shapes of the causal
*                   and acausal filters.
*        07/20/01 - No filter of fcut .le. 0.0
!        08/01/12 - Added "dble" to b1(k)= statement and changed "2.0d0" to "2d0"
!                   (before doing that, part of the statement was highlighted in
!                   red when viewed in Lahey's ED4W, although I don't know why).

      real s(*)
      real*8 fact(8),b1(8),b2(8)
      real*8 pi,w1,xp,yp,x1,x2,y1,y2


      if (fcut .le. 0.0) return       ! Added by DMB

      pi=4.0d0*datan(1.0d0)
      w1=2.0d0*pi*fcut
      w1=2.0d0*dtan(w1*delt/2.0d0)/delt

      do k=1,nroll
        fact(k)=1.0d0/(1.0d0+dsin(pi*dfloat(2*k-1)/dfloat(4*nroll))
     *   *w1*delt+
     *   (w1*delt/2.0d0)**2)
        b1(k)=( (w1*dble(delt))**2/2d0-2d0 )*fact(k)
        b2(k)=(1.0d0-dsin(pi*dfloat(2*k-1)/dfloat(4*nroll))*w1*delt+
     *   (w1*delt/2.0d0)**2)*fact(k)
      end do

      np2=nd

      if(icaus.ne.1) then
        npad=3.0*float(nroll)/(fcut*delt)
        np1=nd+1
        np2=nd+npad
        do j=np1,np2
          s(j)=0.0
        end do
      end if

      do k=1,nroll
        x1=0.0d0
        x2=0.0d0
        y1=0.0d0
        y2=0.0d0
        do j=1,np2
          xp=s(j)
          yp=fact(k)*(xp-2.0d0*x1+x2)-b1(k)*y1-b2(k)*y2
          s(j)=yp
          y2=y1
          y1=yp
          x2=x1
          x1=xp
        end do
      end do

      if(icaus.ne.1) then
        do k=1,nroll
          x1=0.0d0
          x2=0.0d0
          y1=0.0d0
          y2=0.0d0
          do j=1,np2
            xp=s(np2-j+1)
            yp=fact(k)*(xp-2.0d0*x1+x2)-b1(k)*y1-b2(k)*y2
            s(np2-j+1)=yp
            y2=y1
            y1=yp
            x2=x1
            x1=xp
          end do
        end do
      end if

      return
      end
!* ---------------------------------------------------------------------- LOCUT

!* --------------------------------------------------------------------- HICUT
      subroutine hicut(s,nd,fcut,delt,n,icaus)

!  Butterworth hicut filter order 2*n (n<=8) (see Kanasewich, 
!    Time Sequence Analysis in Geophysics, Third Edition, 
!    University of Alberta Press,1981)
!  written by W. B. Joyner 01/07/97
!  causal if icaus.eq.1 - zero phase shift otherwise
!  s(j) input = the time series to be filtered - output = the 
!    filtered series
!  dimension of s(j) must be at least as large as
!    nd+3.0*float(n)/(fcut*delt)
!  nd = the number of points in the time series
!  fcut = the cutoff frequency
!  delt = the timestep
! Dates: xx/xx/xx - Written by Bill Joyner
!        01/07/00 - D. Boore added check for fcut = 0.0d0, in which case
!                   no filter is applied.  He also cleaned up the
!                   appearance of the code (indented statements in
!                   loops, etc.)
!        09/12/00 - double precision statements added so that the output
!                   series has sufficient precision for double integration.
!        11/08/00 - Increased dimension of s from 50000 to 100000
!        02/14/01 - Modified/corrected real numbers function calls to 
!                   double precision - cds
!        02/22/01 - Removed dimension of s (it is up to the user to specify
!                   it properly)
!        07/20/01 - No filter of fcut .le. 0.0
!        11/18/12 - Increased array dimensions for that n<=16

      real s(*)
      real*8 fact(16),b1(16),b2(16)
!      real*8 fact(8),b1(8),b2(8)
      real*8 pi,w1,xp,yp,x1,x2,y1,y2

      if (fcut <= 0.0) return       ! Added by DMB

      pi=4.0d0*datan(1.0d0)
      w1=2.0d0*pi*fcut
      w1=2.0d0*dtan(w1*delt/2.0d0)/delt

      do k = 1, n
        fact(k)=1.0d0/((2.0d0/(w1*delt))**2+
     :   4.0d0*dsin(pi*dfloat(2*k-1)/dfloat(4*n))/(w1*delt)+1.0d0)
        b1(k)=2.0d0*(1.0d0-(2.0d0/(w1*delt))**2)*fact(k)
        b2(k)=((2.0d0/(w1*delt))**2-
     :   4.0d0*dsin(pi*dfloat(2*k-1)/dfloat(4*n))/(w1*delt)+1.0d0)
     :   *fact(k)
      end do

      np2=nd

      if (icaus /= 1) then
        npad=3.0d0*dfloat(n)/(fcut*delt)
        np1=nd+1
        np2=nd+npad
        do j = np1, np2
          s(j)=0.0
        end do
      end if

      do k = 1, n
        x1=0.0d0
        x2=0.0d0
        y1=0.0d0
        y2=0.0d0
        do j = 1, np2
          xp=s(j)
          yp=fact(k)*(xp+2.0d0*x1+x2)-b1(k)*y1-b2(k)*y2
          s(j)=yp
          y2=y1
          y1=yp
          x2=x1
          x1=xp
        end do
      end do

      if(icaus /= 1) then
        do k = 1, n
          x1=0.0d0
          x2=0.0d0
          y1=0.0d0
          y2=0.0d0
          do j = 1, np2
            xp=s(np2-j+1)
            yp=fact(k)*(xp+2.0d0*x1+x2)-b1(k)*y1-b2(k)*y2
            s(np2-j+1)=yp
            y2=y1
            y1=yp
            x2=x1
            x1=xp
          end do
        end do
      end if

      return
      end
!* --------------------------------------------------------------------- HICUT

 
