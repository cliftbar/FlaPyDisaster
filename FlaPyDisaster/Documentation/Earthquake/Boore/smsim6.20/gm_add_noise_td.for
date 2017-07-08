
*----------------- BEGIN gm_add_noise_td -----------------------------
      subroutine gm_add_noise_td(per, nper, 
     :        iseed2, acc_noise_white, 
     :        freq_acc_noise_spike, acc_noise_spike, n_acc_noise_spike,
     :        save_fas, freq_fas_sim, fas_sim, nfreq_fas,
     :        psvsim, psvsim_std,
     :        pgasim, pgasim_std,
     :        pgvsim, pgvsim_std,
     :        pgdsim, pgdsim_std,
     :        arias, arias_std,
     :        psvsimgm, psvsimgm_std,
     :        pgasimgm, pgasimgm_std,
     :        pgvsimgm, pgvsimgm_std,
     :        pgdsimgm, pgdsimgm_std,
     :        ariasgm, ariasgm_std,
     :        dur_75_05, dur_75_05gm,
     :        psvcalc, nacc_save, acc_save, vel_save, dis_save,
     :        nstart, nstop, npw2, te, ntaper,
     :        itype_mean, t4mean_strt_in, t4mean_stop_in, spec_mean, 
     :        flc, nslope_lc, fhc, nslope_hc, icaus, 
     :        taper_beginning, taper_end
     :                                               )

* Compute various measures of ground motion using time-domain stochastic 
* model simulations.
 
* The indices controlling the extent and duration of motion are
* obtained by calling Get_Npts_For_TD_Calcs fron within the calling program, using this
* statement:
*
*       call Get_Npts_For_TD_Calcs(nstart, nstop, npw2, te, ntaper)


* Dates:  05/10/95 - Written by D. M. Boore
*         05/30/95 - Modified by Basil Margaris to allow choice of window
*         06/02/95 - Further renamed (from psvsimnu) and modified.
*         06/08/95 - Normalize by sqrt(avg square amp)
*         06/09/95 - Get r, amag from common, not parameter list
*         06/12/95 - Removed writing of acc, vel to a file; this should
*                    be done in the front-end driver program.
*         08/08/95 - Removed dc from noise segment before going to
*                    the frequency domain.
*         08/08/95 - Changed things so that remove dc from noise sample before
*                    applying the window.
*         08/18/95 - Added subroutine Write_Params
*         10/17/95 - Added flag to Get_Params and Write_Params
*         10/17/95 - Remove dc or not from random series, depending on a flag
*                    (irmvdc) that is passed through the include statement
*         11/14/95 - Added call to const_am0_gsprd
*         11/14/95 - Broke out numerical recipes and subroutines used
*                    by both rv and td into separate collections
*                    of Fortran subroutines
*         11/16/95 - Replaced 'shape = ...' statement with call to spect_amp
*         12/06/95 - Major simplification in exponential window, removing
*                    tapers (the window itself provides tapers) and correcting
*                    a bug so that the window can be used for a duration
*                    other than the duration used to determine the window 
*                    parameters.
*         12/14/95 - Assigned durex the source plus path duration
*         12/22/95 - Removed rmean real part of work only
*         01/22/96 - Use REALFT rather than FORK
*         04/12/96 - Changed names psv, sd to psv, rd
*         04/15/96 - Changed "rd_spect" to "rd_calc"
*         08/01/98 - Calculate standard deviation peak values
*         09/11/98 - Increased array dimensions from 16400 to 33000
*         01/13/99 - Added computation of Arias intensity
*         02/09/99 - Added computation of displacement; removed subtracting
*                    linear trend from acc in acc2vd routine
*         03/05/99 - Removed include rvtdsubs.for, recipes.for from the
*                    end; these must now be included in the driver programs.
*         07/02/99 - Added arguments to durpath to incorporate Atkinson and 
*                    Silva (1999) magnitude-dependent modification to distance.
*         08/03/99 - Added uniform deviate if desired
*         02/10/99 - Transfer choice of npts from rvtdsubs.for to this
*                    subroutine.  This also includes pad extension if a low-cut
*                    filter is used.  
*                    In all cases 
*                    npts*dt >= max(tshift, tfpad) + dur_fctr * nmotion * dt 
*                               + tfpad 
*         03/12/99 - Suggest increasing dt if the time series is too long.
*         01/07/00 - Upgraded to most recent version of acc2vd
*         01/27/01 - Modified time series window computation (new parameters)
*         01/28/01 - Increased dimensions to 66000 (should use allocatable 
*                    arrays)
*         02/12/01 - Combine gm_td and get_acc, and use dynamically 
*                    allocatable arrays.  Put calculation of ndimen into
*                    a subroutine.  Rename "npts" to "npw2".
*         03/12/01 - Use double precision version of rd_calc
*         05/28/02 - Separate out computation of acceleration time series
*                    as a separate subroutine ("smsim_acc", contained in
*                    file "smsimacc.for").
*         06/13/02 - Renamed "smsim_td" to "gm_td", and deleted routines
*                    now contained in smsimacc, td_subs, and rvtdsubs, which
*                    are brought into the calling program via include
*                    statements.  In addition, pass variables 
*                    nstart, nstop, npw2, and te through the argument list.
*         06/14/02 - Bowing to convention, I changed "prv" to "psv", although 
*                    I prefer "prv" (and "paa" rather than "psa")
*         02/05/03 - Changed units of g from 980 cm/s/s to 981 cm/s/s.
*        05/11/07 - Removed "\smsim\" from include statements
*        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
*                   I caught this only when I compiled the program using the -chk
*                   switch.  This probably means that all of my time-domain runs
*                   up till now used ntaper = 0, even if a nonzero taper was 
*                   specified (the standard being 0.05).
*        02/21/09 - Use a call to avg_std to obtain mean and standard deviation.
*                   This makes the coding less cumbersome and is useful when
*                   computing both geometric and arithmetic means
*                   (as in this revision).
*        02/21/09 - Compute 95%-5% duration
*        02/23/09 - Hardwire 75% rather than 95% in determining duration, as 
*                   recommended by Ou and Herrmann (this is also consistent with
*                   some Husid plots I made).
*        12/04/09 - Call rd_calc rather than rdcalcdp
*        03/25/10 - Replaced call to rd_calc with call to rscalc_interp_acc.
!                 - Renamed subroutine and added noise input and addition to acc_ts.
!        03/26/10 - Added computation of E(FAS^2)^(1/2).
!        03/29/10 - Add mean removal and filtering capability, from blpadflt.
!        04/02/10 - Determine what measure(s) of ground-motion intensity to compute
!                   from per, rather than psvcalc, and allow for WA or WWSSN-SP 
!                   response.
!        04/05/10 - If WWSSN-SP or WA, store sample time series in vel_save and
!                   compute average FAS of the intrument response time series, not 
!                   the input accelerations.
!                 - Declare dimensions of acc_save, vel_save, dis_save as npw2
!                   (an input variable, although it might be changed in this subroutine)
!                   rather than "*" because I was getting these compiler error messages:
!                   "Invalid assumed-size array reference" and "Shape of arrays on 
!                   left and right sides of assignment do not conform"

      complex fas_td_work(:)
      allocatable :: fas_td_work
     
      real acc_temp(:), acc(:), vel(:), dis(:), a_sq_int(:) 
      allocatable :: acc_temp, acc, vel, dis, a_sq_int

      real pga4avg(:), pgv4avg(:), pgd4avg(:), asqint4avg(:), 
     :     psv4avg(:,:), dur75_5array(:)
      allocatable :: pga4avg, pgv4avg, pgd4avg, asqint4avg, 
     :     psv4avg, dur75_5array

      real freq_fas_sim(*), fas_sim(*)
      
      real per(*), 
     :     psvsim(*), psvsim_std(*), 
     :     psvsimgm(*), psvsimgm_std(*), 
     :     acc_save(npw2), vel_save(npw2), dis_save(npw2)
     
      real acc_noise_white
      real acc_noise_spike(*), freq_acc_noise_spike(*) 
      integer n_acc_noise_spike
      
      real*8 avg_dp

      logical rmv_trend

      logical zcross_l, zcross_t, use_first, use_last
      logical remove_mean_before, remove_mean_after, detrend
      logical apply_pads, save_padded_time_series, 
     :        replace_discarded_data_with_zeros

      character psvcalc*1, strip_pads4instr_resp*1, save_fas*(*)
      character comments(200)*80
      character instr_type*20

      include 'smsim.fi'
      
* pi, twopi now computed in smsim.fi
*      pi = 4.0 * atan(1.0)
*      twopi = 2.0 * pi

      allocate(pga4avg(nsims), pgv4avg(nsims), pgd4avg(nsims), 
     :     asqint4avg(nsims), 
     :     psv4avg(nsims,nper), dur75_5array(nsims))
     
      pga4avg = 0.0
      pgv4avg = 0.0
      pgd4avg = 0.0
      aintsq4avg = 0.0
      psv4avg = 0.0
 
! SET PARAMETERS FOR FILTER PROGRAM:

* Force power of two and four for causal and acausal filters, respectively:

          if (flc > 0.0 .or. fhc > 0.0) then   ! filtering requested
            d4notch = 0.0
            if (flc == fhc .and. flc /= 0.0) then  ! notch
              d4notch = real(nslope_lc)*10.0**(-abs(real(nslope_hc)))
              nroll_lc = 0
              nroll_hc = 0
            else if (icaus == 0) then   ! acausal
              if (nslope_lc < 4) nslope_lc = 4
              if (nslope_hc < 4) nslope_hc = 4
              nslope_lc = 4 * int(float(nslope_lc)/4.0)
              nslope_hc = 4 * int(float(nslope_hc)/4.0)
              nroll_lc = nslope_lc/4
              nroll_hc = nslope_hc/4
            else                          ! causal
              if (nslope_lc < 2) nslope_lc = 2
              if (nslope_hc < 2) nslope_hc = 2
              nslope_lc = 2 * int(float(nslope_lc)/2.0)
              nslope_hc = 2 * int(float(nslope_hc)/2.0)
              nroll_lc = nslope_lc/2
              nroll_hc = nslope_hc/2
            end if
          end if
          
! COMPUTE PAD LENGTHS

* NOTE: the pad computation here is done because of the need to allocate the
* time series; nlz and ntz are passed into subroutine smcpadf.

        if (flc == 0.0 .and. fhc == 0.0) then   ! no filtering
          npad = 0      
        else if (flc == fhc .and. flc /= 0.0) then  ! notch filter
          if (icaus == 1) then
            npad = 0                                ! causal, no pads
          else
            npad = 3.0*pi/d4notch
          end if
        else                                        ! other filtering
          if (flc /= 0.0 .and. fhc /= 0.0) then ! bandpass implied
              nroll_bp = max0(nroll_lc, nroll_hc)
              nslope_bp = max0(nslope_lc, nslope_hc)
          end if
          if (icaus == 1) then
            npad = 0                                ! causal, no pads
          else
            if (flc == 0.0) then                  ! hicut implied
              npad = 3.0*float(nroll_hc)/(fhc*dt)
            else if (fhc .eq. 0.0) then             ! locut implied
              npad = 3.0*float(nroll_lc)/(flc*dt)
            else                                    ! band implied
              npad1=3.0*float(nroll_lc)/(flc*dt)
              npad2=6.0*float(nroll_bp)/((fhc-flc)*dt)
              npad = max0(npad1, npad2)
            end if
          end if
        end if

! ALLOCATE ARRAYS (outside of the loop over simulations):

        xfactor_zpads = 1.0
        if (xfactor_zpads < 1.0) then
          npad4alloc =  npad  ! need this because the allocations in
                              ! subroutine locut assume that npad is at
                              ! least as large as the standard value.
        else
          npad4alloc = int(xfactor_zpads * float(npad))
        end if
        
        npad = int(xfactor_zpads * float(npad))

        nlz = npad/2
        ntz = npad/2
        izcl = nlz
        izct = ntz
        npts4alloc = npw2 + 2*npad4alloc + 2

        allocate ( acc_temp(npw2) )
      
        allocate ( acc(npts4alloc), vel(npts4alloc), dis(npts4alloc), 
     :          a_sq_int(npts4alloc) )

        do i = 1, nfreq_fas
          fas_sim(i)  = 0.0
        end do
        
      write(*,*)
      
      LOOP OVER SIMULATIONS: DO isim = 1, nsims

        write(*,*)      
        write(*,'(3(a,i3),a)') 
     :    ' Patience! Computing accelerogram # ', isim,
     :    ' of ', nsims,
     :    ' and rs at ', nper,' periods.'

        acc_temp = 0.0
        
* Step 1: COMPUTE ACCELERATION TIME SERIES:

        call acc_ts(acc_temp, nstart, nstop, npw2, te, ntaper)
        
        tend = real(npw2-1)*dt
        sps = 1.0/dt
         
* Step 2: ADD NOISE, IF SPECIFIED:

! Add noise:
!    Initialize iseed 2 in calling program (iseed2 = -abs(seed2)
!    Don't use iseed, seed, as these are passed through smsim.fi
!    (although using iseed would probably be OK, as it will give a random number).

        if (acc_noise_white > 0.0) then
          do i = 1, npw2
            random_number = 2.0 * ran1(iseed2) - 1.0
!DEBUG      
!            if (i <=10) then
!               write(*,*) ' i, acc_noise_white, random_number = ', 
!     :                      i, acc_noise_white, random_number
!            end if
!DEBUG      
            
            acc_temp(i) = acc_temp(i) + 
     :                acc_noise_white * random_number
          end do
        end if

        IF (n_acc_noise_spike > 0) THEN
          DO j = 1, n_acc_noise_spike
            IF (acc_noise_spike(j) > 0.0) THEN
              rphase = pi*(2.0 * ran1(iseed2) - 1.0)
              DO i = 1, npw2
                acc_temp(i) = acc_temp(i) + acc_noise_spike(j) *
     :           sin(twopi*freq_acc_noise_spike(j)*real(i-1)*dt+rphase)
              END DO
            END IF
          END DO
        END IF

! At this point, acc_temp is the acceleration time series that would normally be returned
! by the smsim computation.  If it is to be filtered, must store the time series
! into new arrays.
              
      acc = 0.0
      vel = 0.0
      dis = 0.0
      a_sq_int = 0.0

* Step 3: REMOVE MEAN FROM WHOLE RECORD

*  Determine the appropriate mean:

        t4mean_strt = t4mean_strt_in
        t4mean_stop = t4mean_stop_in
        
        if (itype_mean == 0) then  ! no mean removed
          n_t4mean = 0
          avg_all = 0.0
        else if (itype_mean == -1) then   ! use specified mean
          avg_all = spec_mean
        else if (itype_mean == 1) then  ! standard mean removal
          if (t4mean_strt < 0.0) t4mean_strt = 0.0
          if (t4mean_stop > tend) t4mean_stop = tend
          n_t4mean_strt = nint(sps * t4mean_strt + 1.1)
          n_t4mean_stop = nint(sps * t4mean_stop + 1.1)
          n_t4mean = n_t4mean_stop - n_t4mean_strt + 1
          t4mean = float(n_t4mean -1)/sps
          avg_dp = 0.0d0
          do i = n_t4mean_strt, n_t4mean_stop
            avg_dp = avg_dp + dble(acc_temp(i))
          end do
          avg_all = sngl(avg_dp/dble(n_t4mean))
        else            ! shouldn't be here!
          write(*) '  Should not be here, '//'
     :         improper value of itype_mean.  itype_mean = ', 
     :         itype_mean, ' QUITTING!!'
          STOP
        end if

*  Remove the mean from the whole record:
         write(*,'(a,1x,1pe12.5,1x,a)') 
     :   ' Removing mean = ', avg_all, ' from the whole record'

        do i = 1, npw2
          acc(i) = acc_temp(i) - avg_all
        end do

* Step 4: PAD TIME SERIES IF USE ACAUSAL FILTER

        apply_pads                          = .true.
        zcross_l			    = .true.
        zcross_t			    = .true.
        use_first			    = .true.
        use_last			    = .true.
        remove_mean_before		    = .true.
        remove_mean_after 		    = .false.
        save_padded_time_series		    = .false.
        replace_discarded_data_with_zeros   = .true.
        
        if ((flc > 0.0 .or. fhc > 0.0) 
     :         .and. icaus == 0
     :         .and. apply_pads) then  ! acausal filter, apply pads

      comments = ' '
      ncomments = 0    ! this is the input value and does not indicate
                       ! that no comments will be stored in "comments"

          call smcpadf(acc, npw2, sps, nlz, ntz, 
     :      replace_discarded_data_with_zeros, 
     :      taper_beginning, taper_end, 
     :      npts_out, izcl, izct,  
     :      remove_mean_before, remove_mean_after, 
     :      zcross_l, use_first, zcross_t, use_last,
     :      comments, ncomments)    

!DEBUG
        write(*,*) 
     :    ' After call smcpadf: npw2, npts4alloc, npts_out, nlz = ', 
     :                          npw2, npts4alloc, npts_out, nlz
!DEBUG
        else

          nlz = 0
          ntz = 0
          izcl = 1
          izct = npw2
          npts_out = npw2

!DEBUG
        write(*,*) 
     :    ' No filtering:       npw2, npts4alloc, npts_out = ', 
     :                          npw2, npts4alloc, npts_out
!DEBUG
        end if

* Step 5: FILTER

        if (flc > 0.0 .or. fhc > 0.0) then

          call filter(acc, npts_out, dt, flc, nroll_lc, 
     :              fhc, nroll_hc, icaus, d4notch)
          write(*,*) ' *** Filter with flc, fhc = ', flc, fhc

        else
     
          write(*,*) ' *** No filter '

        end if

* Step 6: COMPUTE MEASURES OF GROUND-MOTION INTENSITY

! INTEGRATE TO VELOCITY AND DISPLACEMENT
! (note: if per <= 0.0 do not want pga and pgv both, but
!  I'll keep the calculation, because it costs little, rather
! than a bunch of if statements deciding what to calculate.
! I'll save those for deciding what to print in the driver.

        v04vd = 0.0
        d04vd = 0.0

        rmv_trend = .false.
        call acc2vd(acc, npts_out, dt, rmv_trend, v04vd, d04vd, 
     :              vel, dis)
 
        if (isim == nacc_save) then  ! remove pads
          do i = 1, npw2
            acc_save(i) = acc(i+nlz)
            vel_save(i) = vel(i+nlz)
            dis_save(i) = dis(i+nlz)
          end do
        end if
         


! Compute pga, pgv, pgd, asqint:

        call mnmax(acc, 1, npts_out, 1, amin, amax)
        pga = amax1(abs(amin), abs(amax))
        pga4avg(isim) = pga
         
        call mnmax(vel, 1, npts_out, 1, vmin, vmax)
        pgv = amax1(abs(vmin), abs(vmax))
        pgv4avg(isim) = pgv

        call mnmax(dis, 1, npts_out, 1, dmin, dmax)
        pgd = amax1(abs(dmin), abs(dmax))
        pgd4avg(isim) = pgd

        call accsqint(acc, npts_out, dt, .false., a_sq_int)
         
        a_sq_int_max = a_sq_int(npts_out)
        asqint4avg(isim) = a_sq_int_max

        call locate(a_sq_int,npts_out,0.05*a_sq_int_max,j05) 
        call locate(a_sq_int,npts_out,0.75*a_sq_int_max,j75) 
        
        dur75_5array(isim) = float(j75-j05)*dt
        
        if (per(1) == -3.0 .or. per(1) == -2.0) then  ! want WA or WWSSN_SP response
          vel = 0.0     ! A new use for array vel.
          psvcalc = 'Y'
          instr_type = ' '
          if (per(1) == -3.0) instr_type = 'WWSSN-SP'
          if (per(1) == -2.0) instr_type = 'WA'
          taper_start = 0.0
          taper_stop = 0.0
          strip_pads4instr_resp = 'Y'
          call acc2seismo_response(acc, npts_out, dt, instr_type, 
     :           taper_start, taper_stop,
     :           strip_pads4instr_resp,
     :           vel, nout)   ! "vel" is the instrument response in this case
          n4mnmax = min0(nout, npts_out)  ! they should be the same
!DEBUG
          write(*,*) ' npts_out, nout, n4mnmax, npw2 = ', 
     :                 npts_out, nout, n4mnmax, npw2
!DEBUG
          ninc=1
          call mnmax(vel,1,n4mnmax,ninc,velmin,velmax) 
          psv4avg(isim,1) = amax1(abs(velmin), abs(velmax))  ! again, let a 
                                                           ! variable have a 
                                                           ! different meaning 
                                                           ! than would be 
                                                           ! inferred from 
                                                           ! its name
                                                           
          if (isim == nacc_save) then  
            vel_save = 0.0
            do i = 1, nout
              vel_save(i) = vel(i)
            end do
          end if
         
! If I want to be more precise for the computation of mb, I should also
! find local period around the peak.  I can use mnmaxindx for this purpose

        else if (per(1) > 0.0) then  ! want response spectrum
          psvcalc = 'Y'

* Compute psv:
          d0 = 0.0
          v0 = 0.0
          do i = 1, nper
             omega = twopi/per(i)
             call rscalc_interp_acc(acc, npts_out, omega, damp, dt,
     :                             rd, rv, aa)
             psv = omega * rd
             psv4avg(isim,i) = psv
          end do                                     ! iper
        end if

* Step 8: COMPUTE FOURIER SPECTRUM

! NOTE: First, need to compute new npw2, because pads may have been added
!       Second, want saved FAS at frequencies corresponding to the original npw.
        

* fas_td *****************
        if (save_fas(1:1) == 'Y') then
          signnpw2 = +1.0
          call get_npw2(npts_out,signnpw2,npw2_fas)
!DEBUG
          write(*,*) 
     :          ' isim, npts_out, npw2, npw2_fas, npw2_fas/npw2 = ', 
     :            isim, npts_out, npw2, npw2_fas, npw2_fas/npw2
          write(*,*) ' Array fas_td_work allocated? ', 
     :            allocated(fas_td_work)
!DEBUG
          allocate ( fas_td_work(npw2_fas) )
          
          fas_td_work = cmplx(0.0,0.0)
 
          if (per(1) == -3.0 .or. per(1) == -2.0) then  ! want WA or WWSSN_SP response
            do ifas_td = 1, n4mnmax   ! not npw2_fas, which might be greater than npts_out
              fas_td_work(ifas_td) = cmplx(vel(ifas_td),0.0)
              fas_td_work(ifas_td) = cmplx(vel(ifas_td),0.0)
            end do
          else
            do ifas_td = 1, npts_out   ! not npw2_fas, which might be greater than npts_out
              fas_td_work(ifas_td) = cmplx(acc(ifas_td),0.0)
              fas_td_work(ifas_td) = cmplx(acc(ifas_td),0.0)
            end do
          end if

          call fork(npw2_fas,fas_td_work,-1.)

          df = 1.0/(float(npw2)*dt)
          df_fas = 1.0/(float(npw2_fas)*dt)
          
          do i = 1, nfreq_fas
            f = (i-1) * df
            freq_fas_sim(i) = f
            fas_td_fact = dt * sqrt(float(npw2_fas))
            i_fas = 1 + (i-1)*(npw2_fas/npw2)
            fas_sim(i) = fas_sim(i) + 
     :           (fas_td_fact * cabs(fas_td_work(i_fas)))**2
          end do
          
          deallocate (fas_td_work)
          
        end if
* fas_td *****************

      END DO loop over simulations                          ! isim

* Compute averages

      if (save_fas(1:1) == 'Y') then
        DO ifas  = 1, nfreq_fas
          fas_sim(ifas ) = sqrt(fas_sim(ifas)/nsims)
        END DO
      end if
      
      itype_avg = 1 ! arithmetic
      call avg_std(pga4avg,nsims,pgasim,pgasim_std,itype_avg)
      call avg_std(pgv4avg,nsims,pgvsim,pgvsim_std,itype_avg)
      call avg_std(pgd4avg,nsims,pgdsim,pgdsim_std,itype_avg)
      call avg_std(asqint4avg,nsims,asqintsim,asqintsim_std,
     :             itype_avg)
      call avg_std(dur75_5array,nsims,dur_75_05,dur_75_05_std,
     :             itype_avg)

      itype_avg = 2 ! geometric
      call avg_std(pga4avg,nsims,pgasimgm,pgasimgm_std,itype_avg)
      call avg_std(pgv4avg,nsims,pgvsimgm,pgvsimgm_std,itype_avg)
      call avg_std(pgd4avg,nsims,pgdsimgm,pgdsimgm_std,itype_avg)
      call avg_std(asqint4avg,nsims,asqintsimgm,asqintsimgm_std,
     :             itype_avg)
      call avg_std(dur75_5array,nsims,dur_75_05gm,dur_75_05gm_std,
     :             itype_avg)

      if (psvcalc == 'y' .or. psvcalc == 'Y') then ! cannot assume upper
                                                       ! case
        do i = 1, nper
          itype_avg = 1 ! arithmetic
          call avg_std(psv4avg(1,i),nsims,psvsim(i),psvsim_std(i),
     :               itype_avg)
          itype_avg = 2 ! geometric
          call avg_std(psv4avg(1,i),nsims,psvsimgm(i),psvsimgm_std(i),
     :               itype_avg)
        end do

      end if

      g = 981.0    ! acceleration of gravity, assuming acc units of cm/s^2

      arias_fctr = pi/(2.0*g)
      arias = arias_fctr * asqintsim
      arias_std = arias_fctr * asqintsim_std 
      ariasgm = arias_fctr * asqintsimgm
      ariasgm_std = arias_fctr * asqintsimgm_std 
      
      deallocate (acc_temp)      
      deallocate (acc, vel, dis, a_sq_int)
      deallocate (pga4avg, pgv4avg, pgd4avg, 
     :     asqint4avg, 
     :     psv4avg, dur75_5array)

      return
      end
*----------------- END gm_add_noise_td -----------------------------

