      function drms_dex_f(per_dex, m, r, damp, twopi, rnull,
     :  nm4bt12osc, nr4bt12osc,
     :  m4bt12osc, logr4bt12osc,
     :  c1bt12osc, c2bt12osc, c3bt12osc,
     :  c4bt12osc, c5bt12osc, c6bt12osc,
     :  c7bt12osc )
      
! Find Drms/Dex for specified M and R by interpolating log Drms/Dex evaluated for
! tabulated values of M and R surrounding the desired M and R

! Eric's params equivalences:

!  c1 = A
!  c2 = B
!  c3 = 2.0
!  c4 = eps
!  c5 = alpha
!  c6 = n
!  c7 = delta
  
      
! Dates: 06/18/11 - Written by D. Boore
!        07/02/11 - Allow for use of BT12 ena, wna, or both oscillator corrections.
!                   I think it is best if the necessary files are passed 
!                   via argument lists rather than a common block
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.
!        07/23/14 - Temp change for the new BT14 factors
!        07/28/14 - Return rnull if c1 = c2 = 0
!        11/17/14 - Changed above statement to "Return rnull if m, r is out of range".
!                 - Comment out "stop" in debug statements

      real per_dex, m, r, logr, damp, twopi, rnull
      
      real y_interp, u, t, y1,y2,y3,y4
      
      integer, parameter :: nmparam=13, nrparam=15  ! Added 07/23/14
      integer nm4bt12osc, nr4bt12osc
      real m4bt12osc(nmparam), logr4bt12osc(nrparam)
      real c1bt12osc(nmparam,nrparam), c2bt12osc(nmparam,nrparam), 
     :     c3bt12osc(nmparam,nrparam)
      real c4bt12osc(nmparam,nrparam), c5bt12osc(nmparam,nrparam), 
     :     c6bt12osc(nmparam,nrparam)
      real c7bt12osc(nmparam,nrparam) 

!      include 'bt12osc.fi'

! Find j:

      call locate(m4bt12osc,nm4bt12osc,m,j)

      if (j == 0 .or. j == nm4bt12osc) then ! out of range
!DEBUG
       print *,' In drms_dex_f, m is out of range: m, j,  m4bt12osc = '   
       print *, m, j,  m4bt12osc  
!       stop
!DEBUG
        drms_dex_f = rnull
        return
      end if
      
! Find k:

      logr = alog10(r)

      call locate(logr4bt12osc,nr4bt12osc,logr,k)

      if (k == 0 .or. k == nr4bt12osc) then ! out of range
!DEBUG
       print *,' In drms_dex_f, logr is out of range:'//
     :           ' r, logr, k,  logr4bt12osc = '   
       print *,    r, logr, k,  logr4bt12osc  
!       stop
!DEBUG
        drms_dex_f = rnull
        return
      end if
      
      if (c1bt12osc(j,k) == 0.0 .and. c2bt12osc(j,k) == 0.0) then
        drms_dex_f = rnull
        return
      end if


! Evaluate log(drms_dex) at the four corners:

      y1 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j,k), c2bt12osc(j,k), c3bt12osc(j,k), 
     :      c4bt12osc(j,k), c5bt12osc(j,k), c6bt12osc(j,k), 
     :      c7bt12osc(j,k), twopi)
     
      y2 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j+1,k), c2bt12osc(j+1,k), c3bt12osc(j+1,k), 
     :      c4bt12osc(j+1,k), c5bt12osc(j+1,k), c6bt12osc(j+1,k), 
     :      c7bt12osc(j+1,k), twopi)

      y3 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j+1,k+1), c2bt12osc(j+1,k+1), c3bt12osc(j+1,k+1), 
     :      c4bt12osc(j+1,k+1), c5bt12osc(j+1,k+1), c6bt12osc(j+1,k+1), 
     :      c7bt12osc(j+1,k+1), twopi)

      y4 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j,k+1), c2bt12osc(j,k+1), c3bt12osc(j,k+1), 
     :      c4bt12osc(j,k+1), c5bt12osc(j,k+1), c6bt12osc(j,k+1), 
     :      c7bt12osc(j,k+1), twopi)
     
      y1 = alog10(y1)
      y2 = alog10(y2)
      y3 = alog10(y3)
      y4 = alog10(y4)


      t = (m -m4bt12osc(j))/(m4bt12osc(j+1) - m4bt12osc(j))
      u = (logr -logr4bt12osc(k))/(logr4bt12osc(k+1) - logr4bt12osc(k))
      
       
      y_interp = (1.0-t)*(1.0-u)*y1 + t*(1.0-u)*y2 + 
     :           t*u*y3 + (1.0-t)*u*y4
     
      drms_dex_f = 10.0**y_interp
      
        
      return
      
      end
      
      