      function bt12_td_rv_for_pga_or_pgv_f(m, r, ynull,
     :  nm4bt12, nr4bt12,
     :  m4bt12, logr4bt12,
     :  bt12_td_rv )
      
! Find td/rv for specified M and R by interpolating td/rv evaluated for
! tabulated values of M and R surrounding the desired M and R

! The input arrays are read in subroutine get_params, contained in rv_td_subs, and
! are passed through common blocks in bt12osc.fi.

! Dates: 09/20/11 - Written by D. Boore, based on drms_dex_f.for
!        07/23/14 - Temp change for the new BT14 factors
!        07/28/14 - Set to ynull if bt12_td_rv = 0
!        11/17/14 - Changed above statement to "Set to ynull if m, r is out of range".

      real m, r, logr, ynull
      
      real y_interp, u, t, y1,y2,y3,y4
      
      integer, parameter :: nmparam=13, nrparam=15  ! Added 07/23/14
      integer nm4bt12, nr4bt12
      real m4bt12(nmparam), logr4bt12(nrparam)
      real bt12_td_rv(nmparam,nrparam)


! Find j:

      call locate(m4bt12,nm4bt12,m,j)

      if (j == 0 .or. j == nm4bt12) then ! out of range
        bt12_td_rv_for_pga_or_pgv_f = ynull
        return
      end if
      
! Find k:

      logr = alog10(r)

      call locate(logr4bt12,nr4bt12,logr,k)

      if (k == 0 .or. k == nr4bt12) then ! out of range
        bt12_td_rv_for_pga_or_pgv_f = ynull
        return
      end if
      
      if (bt12_td_rv(j,k) == 0.0) then
        bt12_td_rv_for_pga_or_pgv_f = ynull
!DEBUG
      print *,' For M, R, j, k, bt12_td_rv(j,k), '//
     :         'bt12_td_rv_for_pga_or_pgv_f = '
      print *,  M, R, j, k, bt12_td_rv(j,k),  
     :         bt12_td_rv_for_pga_or_pgv_f
!DEBUG
        return
      end if
        

! Evaluate TD/RV at the four corners (I made plots of TD/RV and log TD/RV as a function
! of log R for the M's; I see no reason to interpolate log TD/RV rather than TD/RV):

      y1 = bt12_td_rv(j,k) 
     
      y2 = bt12_td_rv(j+1,k)

      y3 = bt12_td_rv(j+1,k+1) 

      y4 = bt12_td_rv(j,k+1) 
 
      t = (m -m4bt12(j))/(m4bt12(j+1) - m4bt12(j))
      u = (logr -logr4bt12(k))/(logr4bt12(k+1) - logr4bt12(k))
      
       
      y_interp = (1.0-t)*(1.0-u)*y1 + t*(1.0-u)*y2 + 
     :           t*u*y3 + (1.0-t)*u*y4
     
      bt12_td_rv_for_pga_or_pgv_f = y_interp
      
        
      return
      
      end
      
      