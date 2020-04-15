
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


