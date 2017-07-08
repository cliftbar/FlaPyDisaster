
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

