! --------------------------- BEGIN TRIM_C -----------------------
      subroutine trim_c(cstr, nchar)

* strips leading and trailing blanks from cstr, returning the
* result in cstr, which is now nchar characters in length

* Strip off tabs also.

* Here is a sample use in constructing a column header, filled out with 
* periods:

** Read idtag:
*        idtag = ' '
*        read(nu_in, '(1x,a)') idtag
*        call trim_c(idtag, nc_id)
** Set up the column headings:
*        colhead = ' '
*        colhead = idtag(1:nc_id)//'......' ! nc_id + 6 > length of colhead

* Dates: 12/23/97 - written by D. Boore
*        12/08/00 - pad with trailing blanks.  Otherwise some comparisons
*                   of the trimmed character string with other strings
*                   can be in error because the original characters are left
*                   behind after shifting.  For example, here is a string
*                   before and after shifting, using the old version:
*                      col:12345
*                           MTWH  before
*                          MTWHH  after (but nc = 4).
*        03/21/01 - Check for a zero length input string
*        11/09/01 - Change check for zero length string to check for all blanks
*        10/19/09 - Strip off tabs

      character cstr*(*)

      if(cstr .eq. ' ') then
        nchar = 0
        return
      end if

      nend = len(cstr)

! Replace tabs with blanks:

      do i = 1, nend
        if(ichar(cstr(i:i)) .eq. 9) then
           cstr(i:i) = ' '
        end if
      end do



*      if(nend .eq. 0) then
*        nchar = 0
*        return
*      end if

      do i = nend, 1, -1
        if (cstr(i:i) .ne. ' ') then
           nchar2 = i
           goto 10
        end if
      end do

10    continue

      do j = 1, nchar2
        if (cstr(j:j) .ne. ' ') then
          nchar1 = j
          goto 20
        end if
      end do

20    continue
   
      nchar = nchar2 - nchar1 + 1
      cstr(1:nchar) = cstr(nchar1: nchar2)
      if (nchar .lt. nend) then
        do i = nchar+1, nend
          cstr(i:i) = ' '
        end do
      end if

      return
      end
! --------------------------- END TRIM_C -----------------------

! --------------------- BEGIN UPSTR ----------------------------------
      Subroutine UPSTR ( text )
* Converts character string in TEXT to uppercase
* Dates: 03/12/96 - Written by Larry Baker

C
      Implicit   None
C
      Character  text*(*)
C
      Integer    j
      Character  ch
C
      Do 1000 j = 1,LEN(text)
         ch = text(j:j)
         If ( LGE(ch,'a') .and. LLE(ch,'z') ) Then
            text(j:j) = CHAR ( ICHAR(ch) - ICHAR('a') + ICHAR('A') )
         End If
 1000    Continue
C
      Return
      End
! --------------------- END UPSTR ----------------------------------
! ---------------------- BEGIN SKIP -------------------
      subroutine SKIP(lunit, nlines)
        if (nlines .lt. 1) then
          return
        else
          do i = 1, nlines
             read(lunit, *)
          end do
          return
        end if
      end
! ---------------------- END SKIP -------------------
! ------------------------------------------------------------------ skipcmnt
      subroutine skipcmnt(nu, comment, ncomments)

* Skip text comments in the file attached to unit nu, but save skipped 
* comments in character array comment.  Skip at least one line, and more as 
* long as the lines are preceded by "|" or "!".

* Dates: 04/16/01 - Written by D. Boore
*        12/07/01 - Added check for eof
*        11/04/03 - Use trim_c to trim possible leading blank
*        02/03/07 - Initialize comments to blank

      character comment(*)*(*), buf*80

      ncomments = 0
100   buf = ' '
      read (nu,'(a)',end=999) buf
      call trim_c(buf,nc_buf)
      if (buf(1:1) .eq.'!' .or. buf(1:1) .eq.'|' .or. 
     :                     ncomments + 1 .eq. 1) then
        ncomments = ncomments + 1
        comment(ncomments) = ' '
        comment(ncomments) = buf(1:nc_buf)
        goto 100
      else 
        backspace nu
      end if

999   continue
 
      return
      end
! ------------------------------------------------------------------ skipcmnt

!* --------------------- BEGIN LOCATE -----------------
      SUBROUTINE locate(xx,n,x,j)
      
* Comments added by D. Boore on 26feb2010:
*  finds j such that xx(j) < x <= xx(j+1)
*  EXCEPT if x = xx(1), then j = 1 (logically it would be 0 from
*  the above relation, but this is the same returned value of j
*  for a point out of range).
*  Also, if x = xx(n), j = n-1, which is OK
*  Note that j = 0 or j = n indicates that x is out of range.
*
* See the program test_locate.for to test this routine.

      INTEGER j,n
      REAL x,xx(n)
      INTEGER jl,jm,ju
      jl=0
      ju=n+1
10    if(ju-jl.gt.1)then
        jm=(ju+jl)/2
        if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      if(x.eq.xx(1))then
        j=1
      else if(x.eq.xx(n))then
        j=n-1
      else
        j=jl
      endif
      return
      END
!* --------------------- END LOCATE -----------------

* --------------------- BEGIN LOCATE_D -----------------
      SUBROUTINE locate_d(xx,n,x,j)
      INTEGER j,n
      double precision x,xx(n)
      INTEGER jl,jm,ju
      jl=0
      ju=n+1
10    if(ju-jl.gt.1)then
        jm=(ju+jl)/2
        if((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      if(x.eq.xx(1))then
        j=1
      else if(x.eq.xx(n))then
        j=n-1
      else
        j=jl
      endif
      return
      END
* --------------------- END LOCATE_D -----------------
* --------------------- BEGIN ZBRENT -----------------
      FUNCTION zbrent(func,x1,x2,tol)
      INTEGER ITMAX
      REAL zbrent,tol,x1,x2,func,EPS
      EXTERNAL func
      PARAMETER (ITMAX=100,EPS=3.e-8)
      INTEGER iter
      REAL a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
      a=x1
      b=x2
      fa=func(a)
      fb=func(b)
      if((fa.gt.0..and.fb.gt.0.).or.(fa.lt.0..and.fb.lt.0.))pause
     *'root must be bracketed for zbrent'
      c=b
      fc=fb
      do 11 iter=1,ITMAX
        if((fb.gt.0..and.fc.gt.0.).or.(fb.lt.0..and.fc.lt.0.))then
          c=a
          fc=fa
          d=b-a
          e=d
        endif
        if(abs(fc).lt.abs(fb)) then
          a=b
          b=c
          c=a
          fa=fb
          fb=fc
          fc=fa
        endif
        tol1=2.*EPS*abs(b)+0.5*tol
        xm=.5*(c-b)
        if(abs(xm).le.tol1 .or. fb.eq.0.)then
          zbrent=b
          return
        endif
        if(abs(e).ge.tol1 .and. abs(fa).gt.abs(fb)) then
          s=fb/fa
          if(a.eq.c) then
            p=2.*xm*s
            q=1.-s
          else
            q=fa/fc
            r=fb/fc
            p=s*(2.*xm*q*(q-r)-(b-a)*(r-1.))
            q=(q-1.)*(r-1.)*(s-1.)
          endif
          if(p.gt.0.) q=-q
          p=abs(p)
          if(2.*p .lt. min(3.*xm*q-abs(tol1*q),abs(e*q))) then
            e=d
            d=p/q
          else
            d=xm
            e=d
          endif
        else
          d=xm
          e=d
        endif
        a=b
        fa=fb
        if(abs(d) .gt. tol1) then
          b=b+d
        else
          b=b+sign(tol1,xm)
        endif
        fb=func(b)
11    continue
      pause 'zbrent exceeding maximum iterations'
      zbrent=b
      return
      END
* --------------------- END ZBRENT -----------------
* --------------------------- BEGIN DIST_3DF ---------------------
      subroutine dist_3df(alat_sta, along_sta, 
     :   alat_ref, along_ref, h_ref, strike_f, dip_f, w1, w2, s1, s2, 
     :   h_min_c, 
     :   d_jb, az_jb, 
     :   d_cd2f, az_cd2f, d_c, az_c,
     :   d_sta_n, d_sta_e, icase_cd2f, icase_c, icase_jb)

* Computes various distance measures from a station to a fault.  The 
* orientation of the fault is that used by Spudich et al., Yucca Mt. project.
* The fault is assumed to be a rectangle whose upper and lower edges are
* horizontal.

* Input:
*      alat_sta, along_sta:  latitude and longitude of station, in degrees,
*                            west longitude is negative
*      alat_ref, along_ref:  as above, for reference point used in defining
*                            fault orientation
*      h_ref:                depth to reference point
*      strike_f, dip_f:      azimuth and dip of fault, in degrees.  strike_f is
*                            measured positive clockwise from north; dip_f is
*                            measured from the horizontal.  When looking in 
*                            the direction strike_f, a positive dip is down to
*                            the right.
*      w1, w2, s1, s2:       distances from the reference point to the edges of
*                            the fault.  s1 and s2 are the distances along
*                            strike to the near and far edges; w1 and w2 are
*                            the distances along the dip direction to the upper
*                            and lower edges of the fault.
*      h_min_c:              minimum depth for computing Campbell's distance
*                            (usually 3.0 km)

* Output:
*      d_jb, d_cd2f, d_c:    Joyner & Boore, closest distance to fault surface,
*                            and Campbell distance, respectively.
*      az_jb, az_cd2f, az_c: as above, for azimuths (NOT YET IMPLEMENTED IN THIS
*                            SUBROUTINE)
*      d_sta_n, d_sta_e:     north and east components of station location
*                            relative to the reference point
*      irgn_cd2f, etc:       region in fault-plane coordinates used to 
*                            compute distances.  I could include a sketch here,
*                            but I will not take the time.  These output 
*                            variables were included mainly to help me check
*                            the subroutine.




* Dates: 12/06/98 - Written by D. Boore
*        09/17/00 - Bring in subroutines via include statement;
*                   renamed faz and az_f to fstrike, strike_f
*        11/12/01 - Changed specification in headers above to indicate that
*                   west longitude is negative (consistent with revision to
*                   subroutine deg2km_f)
!        09/16/10 - Compute az_jb (following Kaklamanos et al definitions).
!                 - Changed "rgn" to "case" (Kaklamanos et al., terminology and definition).

      real dist_sta(3)
      real ix(3), iy(3), iz(3)

      pi = 4.0*atan(1.0)
      dtor = pi/ 180.0

* set up unit vectors in fault coordinates in terms of north, east, down 
* unit vectors.

* Convert angles to radians:
      fstrike =  dtor * strike_f
      fdip = dtor * dip_f

* Initialize arrays:

      do i = 1, 3
         ix(i) = 0.0
         iy(i) = 0.0
         iz(i) = 0.0
      end do

* Compute unit vectors:        ! 1, 2, 3 correspond to n, e, d

      ix(1) = cos(fstrike)
      ix(2) = sin(fstrike)
      ix(3) = 0.0

      iy(1) = -sin(fstrike)*sin(fdip)
      iy(2) =  cos(fstrike)*sin(fdip)
      iy(3) =          -cos(fdip)

      iz(1) = -sin(fstrike)*cos(fdip)
      iz(2) =  cos(fstrike)*cos(fdip)
      iz(3) =           sin(fdip)

* Convert station lat, long into distance north and east:
      call deg2km_f(alat_sta, along_sta, alat_ref, along_ref,
     :              dist_sta(1), dist_sta(2))
      dist_sta(3) = -h_ref    ! note minus sign
      
      d_sta_n = dist_sta(1)
      d_sta_e = dist_sta(2)

* Convert coordinates of reference-to-station vector from n,e,d coordinates
* into fault coordinates:
 
      rx = 0.0
      ry = 0.0
      rz = 0.0
  
      do i = 1, 3
        rx = rx + dist_sta(i) * ix(i)
        ry = ry + dist_sta(i) * iy(i)
        rz = rz + dist_sta(i) * iz(i)
      end do

* Find region and closest distance to fault in the fault plane coordinates:

      call find_h(rx, rz, w1, w2, s1, s2, h_cd2f,
     :            icase_cd2f)                         ! cd2f = Closest Distance
                                                      !        to Fault

* Now do it for Campbell:

* Define w1 for Campbell (I assume that w2 does not need defining; in other
* words, not all of the fault plane is above the Campbell depth)

      d2top_c = h_min_c
      d2top = h_ref + w1 * iz(3)        ! iz(3) = sin(fdip)
      if ( d2top .lt. d2top_c .and. iz(3) .ne. 0.0) then
        w1_c = (d2top_c - h_ref)/ iz(3)
      else
        w1_c = w1
      end if

      call find_h(rx, rz, w1_c, w2, s1, s2, h_c,      ! c = Campbell
     :            icase_c)

* (Work on azimuths later)
      az_cd2f = 999.9
      az_c    = 999.9

* Now do it for Joyner-Boore:

* Need to find rx, ry, rz, w1, w2, s1, s2 in terms of coordinates
* of the fault plane projected onto the surface:

      s1_jb = s1
      s2_jb = s2
      w1_jb = w1 * cos(fdip)
      w2_jb = w2 * cos(fdip)

      rx_jb = rx
      rz_jb = -sin(fstrike) * dist_sta(1) + cos(fstrike) * dist_sta(2)

* Then find the region and distance in the plane to the fault surface
      call find_h(rx_jb, rz_jb, 
     :            w1_jb, w2_jb, s1_jb, s2_jb, h_jb,
     :            icase_jb)

* Now compute the distances:

      d_cd2f = sqrt(h_cd2f**2 + ry   **2)
      d_c    = sqrt(h_c   **2 + ry   **2)
      d_jb   = h_jb

      if (icase_jb == 1 .or. icase_jb == 2 .or. icase_jb == 3 ) then
        az_jb = atan2(rz_jb - w1_jb, rx_jb - s2_jb)/dtor
      else if (icase_jb == 4) then
        az_jb = -90.0
      else if (icase_jb == 5 .or. icase_jb == 6) then
        az_jb = +90.0
      else if (icase_jb == 7 .or. icase_jb == 8 .or. icase_jb == 9 )
     :                                                             then
        az_jb = atan2(rz_jb - w1_jb, rx_jb - s1_jb)/dtor
      else
        write(*,*) 
     :   ' Computing az_jb in dist_3df, icase_jb /= 1-9; QUIT!'
        stop        
      end if
      
      return

      end
* --------------------------- END DIST_3DF ---------------------

*-------------------- BEGIN FIND_H ----------------------------
      subroutine find_h(rx, rz, w1, w2, s1, s2, h, icase)

! 09/16/10 - Redefine regions to be those of Kaklamanos et al

! Here is a conversion table:

!    My notes (old definition of region)   Kaklamanos et al case
!           1                                     7
!           2                                     4
!           3                                     1
!           4                                     2
!           5                                     3
!           6                                     6
!           7                                     9
!           8                                     8
!           9                                     5

* Now it is easy to see where the station lies with respect to the fault;
* there are 9 possibilities:  

      if (   rx .le. s1 .and. rz .le. w1 ) then
* old region 1 (see notes)
!        iregion = 1
        icase = 7
        h = sqrt( (s1-rx)**2 + (w1-rz)**2 )
       
      else if (   rz .le. w1 .and. rx .ge. s1 .and. rx .le. s2 ) then
* old region 2 (see notes)
!        iregion = 2
        icase = 4
        h = w1 - rz

      else if (   rx .ge. s2 .and. rz .le. w1 ) then
* old region 3 (see notes)
!        iregion = 3
        icase = 1
        h = sqrt( (rx-s2)**2 + (w1-rz)**2 )
        
      else if (   rx .ge. s2 .and. rz .ge. w1 .and. rz .le. w2 ) then
* old region 4 (see notes)
!        iregion = 4
        icase = 2
        h = rx - s2

      else if (   rx .ge. s2 .and. rz .ge. w2 ) then
* old region 5 (see notes)
!        iregion = 5
        icase = 3
        h = sqrt( (rx-s2)**2 + (rz-w2)**2 )
        
      else if (   rz .ge. w2 .and. rx .ge. s1 .and. rx .le. s2 ) then
* old region 6 (see notes)
!        iregion = 6
        icase = 6
        h = rz - w2

      else if (   rz .ge. w2 .and. rx .le. s1 ) then
* old region 7 (see notes)
!        iregion = 7
        icase = 9
        h = sqrt( (s1-rx)**2 + (rz-w2)**2 )
        
      else if (   rx .le. s1 .and. rz .ge. w1 .and. rz .le. w2 ) then
* old region 8 (see notes)
!        iregion = 8
        icase = 8
        h = s1 - rx

      else if (      rx .ge. s1 .and. rx .le. s2 
     :         .and. rz .ge. w1 .and. rz .le. w2 ) then
* old region 9 (see notes)
!        iregion = 9
        icase = 5
        h = 0.0

      else
* reaching this is an error
        write(*,'(a)') ' ERROR: Region not found in find_h'

      end if

      return
      end
*-------------------- END FIND_H ----------------------------

!      include '\forprogs\deg2km_f.for'
!      include '\forprogs\locate.for'

!-------------------- BEGIN KM2DEG_F ----------------------------
      subroutine km2deg_f( vn_in, ve_in, alat_ref_in, along_ref_in, 
     :               vlat_out, vlong_out )
        
* convert km north and east from a reference point into lat, long

* assumes positive latitude between 0 and 70 degrees
* assumes east longitude is positive
* assumes angles in degrees

* WARNING: NEEDS DOUBLE PRECISION VERSION OF LOCATE (ATTACHED HERE)

* Dates:  10/01/95 - written by D. Boore
*         05/27/98 - Name changed to km2deg_f
*         06/01/98 - Changed to double precision
*         02/14/09 - Changed input to single precision
               
      double precision alat_tbl(71), b_tbl(71), adcoslat_tbl(71)
      double precision vn, ve, alat_ref, along_ref, vlat, vlong
      Data alat_tbl /
     : 0.000000, 1.000000, 2.000000, 3.000000, 4.000000, 5.000000,
     : 6.000000, 7.000000, 8.000000, 9.000000,10.000000,11.000000,
     :12.000000,13.000000,14.000000,15.000000,16.000000,17.000000,
     :18.000000,19.000000,20.000000,21.000000,22.000000,23.000000,
     :24.000000,25.000000,26.000000,27.000000,28.000000,29.000000,
     :30.000000,31.000000,32.000000,33.000000,34.000000,35.000000,
     :36.000000,37.000000,38.000000,39.000000,40.000000,41.000000,
     :42.000000,43.000000,44.000000,45.000000,46.000000,47.000000,
     :48.000000,49.000000,50.000000,51.000000,52.000000,53.000000,
     :54.000000,55.000000,56.000000,57.000000,58.000000,59.000000,
     :60.000000,61.000000,62.000000,63.000000,64.000000,65.000000,
     :66.000000,67.000000,68.000000,69.000000,70.000000
     :/
      Data b_tbl /
     : 1.842808, 1.842813, 1.842830, 1.842858, 1.842898, 1.842950,
     : 1.843011, 1.843085, 1.843170, 1.843265, 1.843372, 1.843488,
     : 1.843617, 1.843755, 1.843903, 1.844062, 1.844230, 1.844408,
     : 1.844595, 1.844792, 1.844998, 1.845213, 1.845437, 1.845668,
     : 1.845907, 1.846153, 1.846408, 1.846670, 1.846938, 1.847213,
     : 1.847495, 1.847781, 1.848073, 1.848372, 1.848673, 1.848980,
     : 1.849290, 1.849605, 1.849992, 1.850242, 1.850565, 1.850890,
     : 1.851217, 1.851543, 1.851873, 1.852202, 1.852531, 1.852860,
     : 1.853188, 1.853515, 1.853842, 1.854165, 1.854487, 1.854805,
     : 1.855122, 1.855433, 1.855742, 1.856045, 1.856345, 1.856640,
     : 1.856928, 1.857212, 1.857490, 1.857762, 1.858025, 1.858283,
     : 1.858533, 1.858775, 1.859008, 1.859235, 1.859452
     :/
      Data adcoslat_tbl /
     : 1.855365, 1.855369, 1.855374, 1.855383, 1.855396, 1.855414,
     : 1.855434, 1.855458, 1.855487, 1.855520, 1.855555, 1.855595,
     : 1.855638, 1.855683, 1.855733, 1.855786, 1.855842, 1.855902,
     : 1.855966, 1.856031, 1.856100, 1.856173, 1.856248, 1.856325,
     : 1.856404, 1.856488, 1.856573, 1.856661, 1.856750, 1.856843,
     : 1.856937, 1.857033, 1.857132, 1.857231, 1.857331, 1.857435,
     : 1.857538, 1.857643, 1.857750, 1.857858, 1.857964, 1.858074,
     : 1.858184, 1.858294, 1.858403, 1.858512, 1.858623, 1.858734,
     : 1.858842, 1.858951, 1.859061, 1.859170, 1.859276, 1.859384,
     : 1.859488, 1.859592, 1.859695, 1.859798, 1.859896, 1.859995,
     : 1.860094, 1.860187, 1.860279, 1.860369, 1.860459, 1.860544,
     : 1.860627, 1.860709, 1.860787, 1.860861, 1.860934
     :/

      pi = 4.0*atan(1.0)
      d2r = pi/ 180.0

      vn = dble(vn_in)
      ve = dble(ve_in)
      alat_ref =  dble(alat_ref_in)
      along_ref =  dble(along_ref_in) 

* interpolate to find proper arc distance:

      call locate_d( alat_tbl, 71, alat_ref, j)
      b = b_tbl(j) + (alat_ref-alat_tbl(j))*
     :  (b_tbl(j+1)-b_tbl(j))/
     :  (alat_tbl(j+1)-alat_tbl(j))

      adcoslat = adcoslat_tbl(j) + (alat_ref-alat_tbl(j))*
     :  (adcoslat_tbl(j+1)-adcoslat_tbl(j))/
     :  (alat_tbl(j+1)-alat_tbl(j))

      a = adcoslat * cos(d2r*alat_ref)

      dlambda = +ve/a ! version with minus used if assume west long is +
*      dlambda = -ve/a ! minus; positve ve corresponds to decrease in long
      dphi    =  vn/b

* convert from minutes of arc to degrees:
      dlambda = dlambda / 60.0
      dphi    = dphi    / 60.0

      vlat  = alat_ref  + dphi
      vlong = along_ref + dlambda

* Consider using the simpler sphere approximation:
*      vlat = alat_ref + vn/(6371.0 * d2r)
*      vlong = along_ref + ve/(6371.0 * d2r * 
*     :        cos(0.5 * (alat_ref + vlat) * d2r))

      vlat_out = sngl(vlat)
      vlong_out = sngl(vlong)
      
      return
      end
!-------------------- END KM2DEG_F ----------------------------

*-------------------- BEGIN DEG2KM_F ----------------------------
      subroutine deg2km_f( alat_sta, along_sta, alat_ref, along_ref, 
     :                       d_sta_n, d_sta_e   )
        
* convert lat, long into km north and east from a reference point

* assumes latitude between 0 and 70 degrees
* assumes west longitude is negative
* assumes angles in degrees

* Dates:  12/06/98 - written by D. Boore, based on km2deg_f
*         12/18/98 - modified to allow for negative latitudes
*         09/16/00 - Removed double precision
      
      real alat_tbl(71), b_tbl(71), adcoslat_tbl(71)
      real a, b, dphi, dlambda
      Data alat_tbl /
     : 0.000000, 1.000000, 2.000000, 3.000000, 4.000000, 5.000000,
     : 6.000000, 7.000000, 8.000000, 9.000000,10.000000,11.000000,
     :12.000000,13.000000,14.000000,15.000000,16.000000,17.000000,
     :18.000000,19.000000,20.000000,21.000000,22.000000,23.000000,
     :24.000000,25.000000,26.000000,27.000000,28.000000,29.000000,
     :30.000000,31.000000,32.000000,33.000000,34.000000,35.000000,
     :36.000000,37.000000,38.000000,39.000000,40.000000,41.000000,
     :42.000000,43.000000,44.000000,45.000000,46.000000,47.000000,
     :48.000000,49.000000,50.000000,51.000000,52.000000,53.000000,
     :54.000000,55.000000,56.000000,57.000000,58.000000,59.000000,
     :60.000000,61.000000,62.000000,63.000000,64.000000,65.000000,
     :66.000000,67.000000,68.000000,69.000000,70.000000
     :/
      Data b_tbl /
     : 1.842808, 1.842813, 1.842830, 1.842858, 1.842898, 1.842950,
     : 1.843011, 1.843085, 1.843170, 1.843265, 1.843372, 1.843488,
     : 1.843617, 1.843755, 1.843903, 1.844062, 1.844230, 1.844408,
     : 1.844595, 1.844792, 1.844998, 1.845213, 1.845437, 1.845668,
     : 1.845907, 1.846153, 1.846408, 1.846670, 1.846938, 1.847213,
     : 1.847495, 1.847781, 1.848073, 1.848372, 1.848673, 1.848980,
     : 1.849290, 1.849605, 1.849992, 1.850242, 1.850565, 1.850890,
     : 1.851217, 1.851543, 1.851873, 1.852202, 1.852531, 1.852860,
     : 1.853188, 1.853515, 1.853842, 1.854165, 1.854487, 1.854805,
     : 1.855122, 1.855433, 1.855742, 1.856045, 1.856345, 1.856640,
     : 1.856928, 1.857212, 1.857490, 1.857762, 1.858025, 1.858283,
     : 1.858533, 1.858775, 1.859008, 1.859235, 1.859452
     :/
      Data adcoslat_tbl /
     : 1.855365, 1.855369, 1.855374, 1.855383, 1.855396, 1.855414,
     : 1.855434, 1.855458, 1.855487, 1.855520, 1.855555, 1.855595,
     : 1.855638, 1.855683, 1.855733, 1.855786, 1.855842, 1.855902,
     : 1.855966, 1.856031, 1.856100, 1.856173, 1.856248, 1.856325,
     : 1.856404, 1.856488, 1.856573, 1.856661, 1.856750, 1.856843,
     : 1.856937, 1.857033, 1.857132, 1.857231, 1.857331, 1.857435,
     : 1.857538, 1.857643, 1.857750, 1.857858, 1.857964, 1.858074,
     : 1.858184, 1.858294, 1.858403, 1.858512, 1.858623, 1.858734,
     : 1.858842, 1.858951, 1.859061, 1.859170, 1.859276, 1.859384,
     : 1.859488, 1.859592, 1.859695, 1.859798, 1.859896, 1.859995,
     : 1.860094, 1.860187, 1.860279, 1.860369, 1.860459, 1.860544,
     : 1.860627, 1.860709, 1.860787, 1.860861, 1.860934
     :/

      pi = 4.0*atan(1.0)
      d2r = pi/ 180.0

* interpolate to find proper arc distance:

      call locate( alat_tbl, 71, abs(alat_ref), j)

      b = b_tbl(j) + (abs(alat_ref)-alat_tbl(j))*
     :  (b_tbl(j+1)-b_tbl(j))/
     :  (alat_tbl(j+1)-alat_tbl(j))

      adcoslat = adcoslat_tbl(j) + (abs(alat_ref)-alat_tbl(j))*
     :  (adcoslat_tbl(j+1)-adcoslat_tbl(j))/
     :  (alat_tbl(j+1)-alat_tbl(j))

      a = adcoslat * cos(d2r*abs(alat_ref))

* compute lat,long relative to reference:
      dphi = alat_sta - alat_ref
      dlambda = along_sta - along_ref

* convert from degrees to minutes of arc:
      dlambda = dlambda * 60.0
      dphi    = dphi    * 60.0

* compute distances (positive ve corresponds to increase in longitude:
*                    vn positive to the north, ve positive to the east)
      d_sta_e =  a * dlambda 
      d_sta_n =  b * dphi

* Consider replacing the above computation with the following simple
* computation based on assuming that the Earth is a perfect sphere:
*      vn = (alat_sta - alat_ref)*d2r*6371.0
*      ve = (along_sta - along_ref)*d2r*
*     :      cos(0.5*(alat_sta+alat_ref)*d2r)*6371.0

      return
      end
*-------------------- END DEG2KM_F ----------------------------

* --------------------- BEGIN YINTRF ------------------------------------
      function yintrf( x, xin, yin, n)
c
c returns an interpolated value (yintrf) based on straight line
c interpolation of the data in xin and yin.

* Needs Numerical recipe routine locate

c
c dates:  3/14/85 - written
*        11/30/95 - substituted LOCATE instead of starting from beginning
*                   each time
*        03/13/96 - added code to deal with xin increasing or decreasing
*        12/12/00 - Stripped off "locate.for"

      dimension xin(1), yin(1)
      logical incrs

* Is xin increasing or decreasing?
      incrs = .true.
      if (xin(n) .lt. xin(1)) incrs = .false.

* Set value if x is outside the range of xin:
      if (incrs) then
        if ( x .le. xin(1) ) then
            yintrf = yin(1)
            return
        end if
        if ( x .ge. xin(n) ) then
            yintrf = yin(n)
            return
        end if  
      else
        if ( x .ge. xin(1) ) then
            yintrf = yin(1)
            return
        end if
        if ( x .le. xin(n) ) then
            yintrf = yin(n)
            return
        end if  
      end if

* Locate the proper cell and interpolate:
      call locate(xin, n, x, j)
      yintrf = yin(j) + (x-xin(j))*(yin(j+1) - yin(j))/
     * (xin(j+1)-xin(j))

      return
      end
* --------------------- END YINTRF ------------------------------------

!----------------- BEGIN GSPRD_F -----------------------------

* I added entry points so that the program could be called using a single 
* argument, without passing the other arguments through common.  Using
* a single argument is necessary when called by sme Numerical Recipes programs.

* Use:

* Call the setup entry point:
*      dummy =  gsprd_f_setup(r_ref,nsprd_segs,rlow,
*     :                     a_s,b_s,m_s,
*     :                     amag)

* Call as a function:
*            gsprd_n = gsprd_f(rn)

* Deallocate arrays:
*      dummy =  gsprd_f_deallocate()





* Dates: 06/07/95 - Written by D.M. Boore
*        07/02/99 - Added magnitude-dependent "depth" from Atkinson
*                   and Silva, which required adding some parameters to
*                   the passed arguments
*        06/05/00 - Added some explanation of r
*        06/08/00 - Make gsprd nondimensional through the use of r_ref, which 
*                   now appears in the definition of variable const
*                   in const_am0_gsprd
*        01/27/02 - Following Silva, parameters added to allow magnitude
*                   dependent slope (to capture finite fault effects)
*        11/27/05 - Remove deff for Atkinson (2005) source
*        04/24/07 - Put "rmod = r" in the if statement
*        11/13/08 - Redo the function so that it can be used in Numerical
*                   Recipes routines, which assume calls to function(x).
*                   Added entry points rather than using common blocks
*                   to do this (using Larry Baker's help).
!        04/08/11 - Removed using AS00 deff to compute rmod, because the 
!                   application is for small size faults for which the
!                   finite-fault effect approximated by deff is not relevant
!                   (of course, amag will be small, so this would reduce 
!                   the impact of using deff).
!                 - Removed numsource from argument list.

      function gsprd_f(r)
     
      save
      
      real rlow_init(*), a_s_init(*), 
     :                            b_s_init(*), 
     :                            m_s_init(*)
      real, allocatable :: rlow(:), a_s(:), b_s(:), m_s(:)
      real geff(10)
      
* Note that generally r = hypocentral distance.  For Atkinson and Silva 
* (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
* their paper; below their eq. 4), so that rmod is, in effect, accounting
* source depth twice.  See comments in AS00 section of subroutine
* spect_scale

      
!      if (numsource .eq. 9 ) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if

      rmod = r
      
      geff(1) = r_ref/rlow(1)  ! usually set r_ref = 1.0 km.  Be careful
                               ! if a different value or different units are
                               ! used.  In particular, using different units
                               ! will require a change in the scaling factor
                               ! of 1.e-20 used in the definition of const in
                               ! const_am0_gsprd

      do i = 2, nsprd_segs
        slope = a_s(i-1) + b_s(i-1)*(amag - m_s(i-1))
        geff(i) = geff(i-1)*(rlow(i)/rlow(i-1))**slope
      end do
      if (rmod .le. rlow(1)) then
        j = 1
      else if (rmod .ge. rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd_f = (geff(j)) * (rmod/rlow(j))**slope
      
      return

      entry gsprd_f_setup(r_ref_init,nsprd_segs_init,rlow_init,
     :                  a_s_init,b_s_init,m_s_init,
     :                  amag_init)
     
      allocate(rlow(nsprd_segs_init), 
     :                                a_s(nsprd_segs_init), 
     :                                b_s(nsprd_segs_init),  
     :                                m_s(nsprd_segs_init))
      r_ref                    = r_ref_init
      nsprd_segs               = nsprd_segs_init
      rlow       = rlow_init(1:nsprd_segs_init) 
      a_s        = a_s_init(1:nsprd_segs_init) 
      b_s        = b_s_init(1:nsprd_segs_init) 
      m_s        = m_s_init(1:nsprd_segs_init)
      amag       = amag_init
      
      return
      
      entry gsprd_f_deallocate

      deallocate(rlow, a_s, b_s, m_s)
      
      gsprd_f_deallocate = 1.0
      
      return
     
      
      end
!----------------- END GSPRD_F -----------------------------
!----------------- BEGIN GSPRD_Q_F -----------------------------

* I added entry points so that the program could be called using a single 
* argument, without passing the other arguments through common.  Using
* a single argument is necessary when called by sme Numerical Recipes programs.

* Use:

* Call the setup entry point:
*      dummy =  gsprd_q_f_setup(r_ref,nsprd_segs,rlow,
*     :                     a_s,b_s,m_s,
*     :                     amag,
*     :                     q_fq, c_q, fq)

* Call as a function:
*            gsprdq_n = gsprd_q_f(rn)

* Deallocate arrays:
*      dummy =  gsprd_q_f_deallocate()


* Dates: 06/07/95 - Written by D.M. Boore
*        07/02/99 - Added magnitude-dependent "depth" from Atkinson
*                   and Silva, which required adding some parameters to
*                   the passed arguments
*        06/05/00 - Added some explanation of r
*        06/08/00 - Make gsprd nondimensional through the use of r_ref, which 
*                   now appears in the definition of variable const
*                   in const_am0_gsprd
*        01/27/02 - Following Silva, parameters added to allow magnitude
*                   dependent slope (to capture finite fault effects)
*        11/27/05 - Remove deff for Atkinson (2005) source
*        04/24/07 - Put "rmod = r" in the if statement
*        11/13/08 - Redo the function so that it can be used in Numerical
*                   Recipes routines, which assume calls to function(x).
*                   Added entry points rather than using common blocks
*                   to do this (using Larry Baker's help).
*        11/14/08 - Add q to the computation
!        04/08/11 - Removed using AS00 deff to compute rmod, because the 
!                   application is for small size faults for which the
!                   finite-fault effect approximated by deff is not relevant
!                   (of course, amag will be small, so this would reduce 
!                   the impact of using deff).
!                 - Removed numsource from argument list.

      function gsprd_q_f(r)
     
      save
      
      real rlow_init(*), a_s_init(*), 
     :                            b_s_init(*), 
     :                            m_s_init(*)
      real, allocatable :: rlow(:), a_s(:), b_s(:), m_s(:)
      real geff(10)
      
* Note that generally r = hypocentral distance.  For Atkinson and Silva 
* (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
* their paper; below their eq. 4), so that rmod is, in effect, accounting
* source depth twice.  See comments in AS00 section of subroutine
* spect_scale

      
!      if (numsource .eq. 9 ) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if

      rmod = r
      
      geff(1) = r_ref/rlow(1)  ! usually set r_ref = 1.0 km.  Be careful
                               ! if a different value or different units are
                               ! used.  In particular, using different units
                               ! will require a change in the scaling factor
                               ! of 1.e-20 used in the definition of const in
                               ! const_am0_gsprd

      do i = 2, nsprd_segs
        slope = a_s(i-1) + b_s(i-1)*(amag - m_s(i-1))
        geff(i) = geff(i-1)*(rlow(i)/rlow(i-1))**slope
      end do
      if (rmod .le. rlow(1)) then
        j = 1
      else if (rmod .ge. rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd = (geff(j)) * (rmod/rlow(j))**slope
      
      akappaq = rmod/(c_q*q_fq)

      dimin = exp(-pi*akappaq*fq)
      
      gsprd_q_f = gsprd*dimin
      
      return

      entry gsprd_q_f_setup(r_ref_init,nsprd_segs_init,rlow_init,
     :                  a_s_init,b_s_init,m_s_init,
     :                  amag_init, 
     :                  q_fq_init, c_q_init, fq_init)
     
      allocate(rlow(nsprd_segs_init), 
     :                                a_s(nsprd_segs_init), 
     :                                b_s(nsprd_segs_init),  
     :                                m_s(nsprd_segs_init))
      r_ref                    = r_ref_init
      nsprd_segs               = nsprd_segs_init
      rlow       = rlow_init(1:nsprd_segs_init) 
      a_s        = a_s_init(1:nsprd_segs_init) 
      b_s        = b_s_init(1:nsprd_segs_init) 
      m_s        = m_s_init(1:nsprd_segs_init)
      amag       = amag_init
      
      q_fq = q_fq_init
      fq = fq_init
      c_q = c_q_init
      
      pi = 4.0*atan(1.0)
      
      return
      
      entry gsprd_q_f_deallocate()

      deallocate(rlow, a_s, b_s, m_s)
      
      gsprd_q_deallocate = 1.0
      
      return
     
      
      end
!----------------- END GSPRD_Q_F -----------------------------
!----------------- BEGIN GSPRD_Q_AVG_F -----------------------------

* Computes gpsrdq - gpsrdq_average (obtained as the sqrt of the
* sum of the gsprdq**2
* from individual subfaults).  The difference is used in finding Reff
* using a root finding routine.

* I added entry points so that the program could be called using a single 
* argument, without passing the other arguments through common.  Using
* a single argument is necessary when called by sme Numerical Recipes programs.

* Use:

* Declare as an external function
*      external gsprd_q_avg_f

* Call the setup entry point:
*          dummy =  gsprd_q_avg_f_setup(r_ref,nsprd_segs,rlow,
*     :                     a_s,b_s,m_s,
*     :                     amag,
*     :                     q_fq, c_q, fq,
*     :                     gsprdq_avg)

* Use an an argument in a routine expecting a function with a single argument:
*         rn_effective = zbrent(gsprd_q_avg_f,x1,x2,tol)

* Deallocate arrays:
*         dummy =  gsprd_q_avg_f_deallocate()





* Dates: 11/14/08 - Written by D.M. Boore, patterned after gsprd_q_f
!        04/08/11 - Removed using AS00 deff to compute rmod, because the 
!                   application is for small size faults for which the
!                   finite-fault effect approximated by deff is not relevant
!                   (of course, amag will be small, so this would reduce 
!                   the impact of using deff).
!                 - Removed numsource from argument list.

      function gsprd_q_avg_f(r)
     
      save
      
      real rlow_init(*), a_s_init(*), 
     :                            b_s_init(*), 
     :                            m_s_init(*)
      real, allocatable :: rlow(:), a_s(:), b_s(:), m_s(:)
      real geff(10)
      
* Note that generally r = hypocentral distance.  For Atkinson and Silva 
* (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
* their paper; below their eq. 4), so that rmod is, in effect, accounting
* source depth twice.  See comments in AS00 section of subroutine
* spect_scale

      
!      if (numsource .eq. 9 ) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if
      
      rmod = r
      
      geff(1) = r_ref/rlow(1)  ! usually set r_ref = 1.0 km.  Be careful
                               ! if a different value or different units are
                               ! used.  In particular, using different units
                               ! will require a change in the scaling factor
                               ! of 1.e-20 used in the definition of const in
                               ! const_am0_gsprd

      do i = 2, nsprd_segs
        slope = a_s(i-1) + b_s(i-1)*(amag - m_s(i-1))
        geff(i) = geff(i-1)*(rlow(i)/rlow(i-1))**slope
      end do
      if (rmod .le. rlow(1)) then
        j = 1
      else if (rmod .ge. rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd = (geff(j)) * (rmod/rlow(j))**slope
      
      akappaq = rmod/(c_q*q_fq)

      dimin = exp(-pi*akappaq*fq)
      
      gsprd_q_avg_f = gsprd*dimin - gsprdq_avg
      
      return

      entry gsprd_q_avg_f_setup(r_ref_init,nsprd_segs_init,rlow_init,
     :                  a_s_init,b_s_init,m_s_init,
     :                  amag_init, 
     :                  q_fq_init, c_q_init, fq_init,
     :                  gsprdq_avg_init)
     
      allocate(rlow(nsprd_segs_init), 
     :                                a_s(nsprd_segs_init), 
     :                                b_s(nsprd_segs_init),  
     :                                m_s(nsprd_segs_init))
      r_ref                    = r_ref_init
      nsprd_segs               = nsprd_segs_init
      rlow       = rlow_init(1:nsprd_segs_init) 
      a_s        = a_s_init(1:nsprd_segs_init) 
      b_s        = b_s_init(1:nsprd_segs_init) 
      m_s        = m_s_init(1:nsprd_segs_init)
      amag       = amag_init
      
      q_fq = q_fq_init
      fq = fq_init
      c_q = c_q_init
      
      gsprdq_avg = gsprdq_avg_init
      
      pi = 4.0*atan(1.0)
      
      return
      
      entry gsprd_q_avg_f_deallocate()

      deallocate(rlow, a_s, b_s, m_s)
      
      gsprd_q_avg_deallocate = 1.0
      
      return
     
      
      end
!----------------- END GSPRD_Q-AVG_F -----------------------------

*----------------- BEGIN Q_f -----------------------------

* I added entry points so that the program could be called using a single 
* argument, without passing the other arguments through common.  Using
* a single argument is necessary when called by sme Numerical Recipes programs.

* Use:

* Call the setup entry point:
*      dummy = q_f_setup(fr1, qr1, s1, ft1, ft2,
*     :              fr2, qr2, s2, c_q)

* Call as a function:
*      q_fq = q_f(fq)



* Dates: 06/07/95 - Written by D.M. Boore
*        12/14/95 - Added check for equal transition frequencies
*        05/11/07 - Removed "\smsim\" from include statements
*        11/14/08 - Redo the function so that it can be used in Numerical
*                   Recipes routines, which assume calls to function(x).
*                   Added entry points rather than using common blocks
*                   to do this (using Larry Baker's help).

      function q_f(f) 

      save  
 
      q_f = 9999.0
      if (f .eq. 0.0) return
        
      if ( f .le. ft1) then
        q_f = qr1*(f/fr1)**s1
      else if ( f .ge. ft2) then
        q_f = qr2*(f/fr2)**s2
      else
        q_f = qt1*(f/ft1)**st
      end if

      return
      
      entry q_f_setup(fr1_init, qr1_init, s1_init, ft1_init, ft2_init,
     :              fr2_init, qr2_init, s2_init, c_q_init)
      
      fr1 = 	fr1_init
      qr1 = 	qr1_init
      s1  = 	s1_init 
      ft1 = 	ft1_init
      ft2 = 	ft2_init
      fr2 = 	fr2_init
      qr2 = 	qr2_init
      s2  = 	s2_init
      c_q = 	c_q_init

      qt1 = qr1*(ft1/fr1)**s1
      qt2 = qr2*(ft2/fr2)**s2
      st = 0.0
      if (ft1 .ne. ft2) then
        st = alog10(qt2/qt1)/alog10(ft2/ft1)
      end if
 
      return


      end
*----------------- END Q_f -----------------------------
