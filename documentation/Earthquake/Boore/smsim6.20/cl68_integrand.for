!----------------- BEGIN CL68_INTEGRAND -----------------------------
      function cl68_integrand(z)
! Dates: 06/09/95 - Written by D.M. Boore.  See 7/11/82 notes for
!                   stochastic model, with 6/9/95 addition that uses
!                   a variable transformation to remove the sqrt
!                   singularity at the origin.
!        01/03/95 - Name changed from cl_int to cl68_integrand
!        02/05/00 - Made changes suggested by C. Mueller to avoid
!                   possible numerical problem when "an" is large

      common /clint/ xi, an
!      real*8 y, dum

!      dum = 1.0d0-xi*dexp(-z**2)
!      y = an * dlog(dum)   ! Mueller modification
!      if (y .lt. -10.0d0**(-10)) then !based on machine epsilon for dble precsn
!        cl68_integrand = 0.0  ! or could let compiler deal with underflow
!      else
!        cl68_integrand = 2.0*(1.0d0-dexp(y))  ! Mueller modification
!      end if

      y = an * alog(1.0-xi*exp(-z**2))   ! Mueller modification
      cl68_integrand = 2.0*(1.0-exp(y))  ! Mueller modification

!      cl68_integrand = 2.0*(1.0-(1.0-xi*exp(-z**2))**an)  ! original

      return
      end
!----------------- END CL68_INTEGRAND -----------------------------
