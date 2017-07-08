
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

