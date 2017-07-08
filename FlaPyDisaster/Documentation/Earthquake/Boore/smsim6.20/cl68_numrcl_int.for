!----------------- BEGIN CL68_NUMRCL_INT -----------------------------
      function cl68_numrcl_int( an_in, xi_in, zup)
! Numerical integration of eq. 6.8 in Cartwright and Longuet-Higgins
! Dates: 06/09/95 - Written by D.M. Boore, and tested using CHK_INT.
!                   I also plotted the integrand for typical values
!                   of an, xi, and found that it decays strongly to zero
!                   by a value of 5 for the variable.  I use 10 as an upper
!                   limit, which should be much more than enough.  The 
!                   integration routines are such, however, that I could 
!                   probably use a much larger m=number with little extra
!                   time.
!        12/19/95 - Added zup to exact_numrcl
!        12/28/95 - Name changed from exact_numrcl to cl68_numrcl
!        01/03/96 - Name changed from cl68_numrcl to cl68_numrcl_int
!        03/13/99 - On the advice of R. Herrmann, substituted qromb for
!                   qmidpnt.  Numerical tests indicate that both give the
!                   same answers, but qromb evaluates the function at the 
!                   endpoints (zlow and zup), whereas qmidpnt does not (and
!                   is appropriate for an improper integral for which
!                   the integrand cannot be evaluated right at the endpoints;
!                   this is not the case here).
!        01/27/02 - Reset zup so that the integrand is not equal to 0.0 
!                   (except the last point).  I did this because I was being 
!                   plagued with small differences in the pk_rms factor for
!                   some values of D, M, and zup.  After a bunch of testing,
!                   using embedded write statements and the program chk_int,
!                   I'm pretty sure that the problem is related to the attempt
!                   to numerically integrate the function when it has been set
!                   to 0.0 by the compiler (because of underflow).  The 
!                   integrand decreases rapidly, so that zup=10 is generally
!                   much larger than needed.  Note that I also eliminated the
!                   "glitch" in pk_rms by setting eps in qromb to 1.0e-8, but I
!                   think the current fix is better.

      external cl68_integrand

      common /clint/ xi, an

      an = an_in
      xi = xi_in      
      zlow = 0.0 ! where is zup? It is read in as a parameter in the input file.

! Reset zup if need be:
      zup_step = 0.1
      do while (cl68_integrand(zup) .eq. 0.0) 
        zup = zup - zup_step
      end do
      zup = zup + zup_step
     
      call qromb(cl68_integrand,zlow,zup,result)

      cl68_numrcl_int = result/sqrt(2.0)

      return
      end
!----------------- END CL68_NUMRCL_INT -----------------------------

