
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

