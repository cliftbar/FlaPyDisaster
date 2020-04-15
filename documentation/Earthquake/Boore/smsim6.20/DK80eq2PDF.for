! ------------------------------------------------------------------ DK80eq2PDF
      FUNCTION DK80eq2PDF(pf,q,anz)

! The PDF corresponding to DK80 eq. 2, using analytic derivatives      
! Dates: 06/24/14 - Written by D. Boore, DK80eq2     

      REAL pf, q, sqrtpidiv2, qe, anz
      
      sqrtpidiv2 = sqrt(2.0*atan(1.0))
      
      qe = q**1.2     ! l. 2 below eq. 2 in DK(1980)
      
      e1 = exp(-0.5*pf**2)
      e2 = exp(-sqrtpidiv2*qe*pf)
      
      A = (1.0-e1)
      B = (1.0-e2)
      C = A/e1
      E = exp(-anz*B/C)
      
      dAda = pf * e1
      dBda = sqrtpidiv2*qe*e2
      dCda = pf/e1
      
      dEda = -anz*E*(C*dBda - B*dCda)/C**2
      
      DK80eq2PDF = A*dEda + E*dAda
       
      
      END
! ------------------------------------------------------------------ DK80eq2PDF
