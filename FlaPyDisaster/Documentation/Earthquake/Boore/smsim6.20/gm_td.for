
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

