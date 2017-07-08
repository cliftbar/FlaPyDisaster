      function bt12osc_eval_eq(per_dex, damp, 
     :         c1, c2, c3, c4, c5, c6, c7, twopi)
      
! Eric's params equivalences:

!  c1 = A
!  c2 = B
!  c3 = 2.0
!  c4 = eps
!  c5 = alpha
!  c6 = n
!  c7 = delta
  
      
! Dates: 06/18/11 - Written by D. Boore
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.

      eta = per_dex

      F = c1+c2*(1.0-eta**c3)/(1.0+eta**c3)
      
      bt12osc = 
     :      1.0 + c4*(1.0/(twopi*damp))*(eta/(1.0+c5*eta**c6))**c7
      
      bt12osc_eval_eq = F * bt12osc
      
      return
      
      end
      
      