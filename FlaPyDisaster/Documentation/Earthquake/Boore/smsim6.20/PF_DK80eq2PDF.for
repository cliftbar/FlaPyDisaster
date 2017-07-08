! ------------------------------------------------------------------ PF_DK80eq2PDF
      SUBROUTINE PF_DK80eq2PDF(anz,q,tol,na,pf)
      
!Dates: 06/20/14 - Written by D> Boore, patterned after a program by E. Rathje.
!       06/24/14 - Do not require that nz is an integer.  This leads to small jumps in the peak 
!                  factor with period.  I replaced nz with anz here, in keeping with
!                  the usage in SMSIM
!       06/24/14 - Written by D> Boore: use analytic PDF

      REAL q,da, amax, pf, tol, anz
      REAL a(10000),pdf(10000)  ! should make these allocatible
      INTEGER i, nint
      
      amax = 6 ! Based on runs of evaluate_dkeq2.for; see plots of output in
               ! Evaluate_DKeq2.out.draw
       
      na = 10 ! To initiate computations
      da = amax/real(na)
        
      DO i = 1,na
        a(i) = real(i) * da
      ENDDO
      
      pf = 0

      nint = na-2

      DO i = 2,na-2
        pf = pf + a(i)*DK80eq2PDF(a(i),q, anz)
      END DO
      
      pf1 = pf * da
      
      DO
      
        na = 2*na
        da = amax/real(na)
          
        DO i = 1,na
          a(i) = real(i) * da
        ENDDO
        
        pf = 0

        nint = na-2
        
        DO i = 2,na-2
          pf = pf + a(i)*DK80eq2PDF(a(i),q, anz)
        END DO
        
        pf = pf * da
        
        if ((pf-pf1)/pf1 < tol) EXIT
        
        pf1 = pf
        
      END DO       
 
      END
! ------------------------------------------------------------------ PF_DK80eq2PDF
