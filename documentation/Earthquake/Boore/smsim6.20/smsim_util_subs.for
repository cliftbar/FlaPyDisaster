
! ----------------------- BEGIN RCC -----------------------------------
      subroutine RCC (KPARM,CSTR_IN,NCSTR_IN,C,NC,ibgn,iend)
! RCC - [MUELLER.FS.GEN]CSMGENLB
! Read character string C(1:NC), the KPARMth element in CSTR_IN(1:NCSTR_IN).
!  CSTR_IN contains one-or-more mixed-type fields
!(OLD COMMENT:c  separated by blanks or commas to the right of an optional '='.)
!  KPARMth field contains C to the right of an optional '>'.
! If the KPARMth field is nonexistent or empty, RCC sets NC=0 and returns.
!  This simulates a <carriage-return> in a Q-format read.
!  (An empty field must be separated by commas, not blanks.)

! Dates:  xx/xx/xx - Written by C. Mueller, USGS
!         06/22/98 - Modified by L. Baker
!         11/12/00 - Dave Boore introduced a working array, and a call to 
!                    rmv_tabs (this replaces a tab with a blank, but does not
!                    expand the tabs).  This was done because a file in which 
!                    fields are separated by tabs characters will not be parsed 
!                    correctly; apparently the tab characters (ascii 9) is 
!                    not the same thing as a blank.
!         12/04/00 - added ibgn, iend to rcf, rci, rcc calling arguments
!         06/11/01 - Disable use of "=" to indicate where the parameters
!                    start.  I did this because I sometimes add comments
!                    to the right of the parameters, and if these comments
!                    contain the character "=" then the parameters are not
!                    parsed correctly.  I reasoned that eliminating the check
!                    for "=" is the best solution to the problem (as opposed
!                    to restricting the use of "=" to only indicate 
!                    where the parameters start) because I rarely used
!                    "=" in this way.
!         06/12/01 - Minor modification (i2 = 0 rather than i2 = 1)
!         10/29/02 - Changed "rmv_tabs" to "tabs_rmv"
!         07/25/10 - Increase dimension of cstr
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character CSTR_IN*(*),C*(*),term*1

      character cstr*500

      if (ncstr_in .gt. 500) then
         write(*,'(a, 1x,i5, a)') '  WARNING: IN RCC, NCSTR_IN = ', 
     :          ncstr_in,
     :         ' WHICH IS GREATER THAN THE DIMENSION OF THE'//
     :         ' WORK ARRAY; QUITTING!!!!!'
         return
      end if

      ncstr = ncstr_in

      cstr = ' '
      cstr(1: ncstr) = cstr_in
      call tabs_rmv(cstr, ncstr)

! Eliminate trailing white space.
      N = NCSTR+1
1     N = N-1
      if (CSTR(N:N).eq.' '.or.CSTR(N:N).eq.',') goto 1

! Changes on 6/11/01:
!c Find optional '='.
!      I2 = INDEX(CSTR(1:N),'=')
!      term = '='
      i2 = 0         ! DMB, 6/12/01
      term = ' '     ! DMB, 6/11/01
! Changes on 6/11/01:

! Find start and end of KPARMth field.
      do 4 K=1,KPARM
         I1 = I2
2        I2 = I1
3        I1 = I1+1
         if (CSTR(I1:I1).eq.' ') goto 3
         if (term.eq.' '.and.CSTR(I1:I1).eq.',') then
            term = ','
            goto 2
         end if
         IBLANK = INDEX(CSTR(I1:N),' ')
         ICOMMA = INDEX(CSTR(I1:N),',')
         if (IBLANK.eq.0.and.ICOMMA.eq.0.and.K.lt.KPARM) goto 801
         if (IBLANK.eq.0) IBLANK = N-I1+2
         if (ICOMMA.eq.0) ICOMMA = N-I1+2
         I2 = I1+MIN(IBLANK,ICOMMA)-1
         if (I2.gt.N) then
            term = '.'
         else
            term = CSTR(I2:I2)
         end if
4        continue
! Check for empty field.
      if (I2.eq.I1) goto 801
! Find optional '>'.
      I1 = I1+INDEX(CSTR(I1:I2),'>')
! Get C.
      C = CSTR(I1:I2-1)
      NC = I2-I1
      ibgn = i1
      iend = i2-1
      return
! Errors.
801   NC = 0
      return
      end
! ----------------------- END RCC -----------------------------------


      
! --------------------------- BEGIN RCF ------------------------------
      subroutine RCF (KPARM,CSTR_IN,NCSTR_IN,F,ibgn,iend, ISTAT)
! RCF - [MUELLER.FS.GEN]CSMGENLB
! Read real F, the KPARMth element in CSTR_IN(1:NCSTR_IN).
!  CSTR_IN contains one-or-more mixed-type fields
!(OLD COMMENT:c  separated by blanks or commas to the right of an optional '='.)
!  KPARMth field contains F to the right of an optional '>'.
! ISTAT= 0 for success;
!      = 1 if CSTR contains fewer than KPARM fields;
!      = 2 if KPARMth field is empty;
!      = 3 if KPARMth field is unreadable;
!      if ISTAT>0, return with F unchanged.


! Dates:  xx/xx/xx - Written by C. Mueller, USGS
!         06/22/98 - Modified by L. Baker
!         11/12/00 - Dave Boore introduced a working array, and a call to 
!                    rmv_tabs (this replaces a tab with a blank, but does not
!                    expand the tabs).  This was done because a file in which 
!                    fields are separated by tabs characters will not be parsed 
!                    correctly; apparently the tab characters (ascii 9) is 
!                    not the same thing as a blank.
!         12/04/00 - added ibgn, iend to rcf, rci, rcc calling arguments
!         06/11/01 - Disable use of "=" to indicate where the parameters
!                    start.  I did this because I sometimes add comments
!                    to the right of the parameters, and if these comments
!                    contain the character "=" then the parameters are not
!                    parsed correctly.  I reasoned that eliminating the check
!                    for "=" is the best solution to the problem (as opposed
!                    to restricting the use of "=" to only indicate 
!                    where the parameters start) because I rarely used
!                    "=" in this way.
!         06/12/01 - Minor modification (i2 = 0 rather than i2 = 1)
!         10/29/02 - Changed "rmv_tabs" to "tabs_rmv"
!         07/25/10 - Increase dimension of cstr
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character CSTR_IN*(*), term*1

      character cstr*500

      if (ncstr_in .gt. 500) then
         write(*,'(a, 1x,i5, a)') '  WARNING: IN RCF, NCSTR_IN = ', 
     :          ncstr_in,
     :         ' WHICH IS GREATER THAN THE DIMENSION OF THE'//
     :         ' WORK ARRAY; QUITTING!!!!!'
         return
      end if

      ncstr = ncstr_in

      cstr = ' '
      cstr(1: ncstr) = cstr_in
      call tabs_rmv(cstr, ncstr)

      ISTAT = 0
! Eliminate trailing white space.
      N = NCSTR+1
1     N = N-1
      if (CSTR(N:N).eq.' '.or.CSTR(N:N).eq.',') goto 1

! Changes on 6/11/01:
!c Find optional '='.
!      I2 = INDEX(CSTR(1:N),'=')
!      term = '='
      i2 = 0         ! DMB, 6/12/01
      term = ' '     ! DMB, 6/11/01
! Changes on 6/11/01:

! Find start and end of KPARMth field.
      do 4 K=1,KPARM
         I1 = I2
2        I2 = I1
3        I1 = I1+1
         if (CSTR(I1:I1).eq.' ') goto 3
         if (term.eq.' '.and.CSTR(I1:I1).eq.',') then
            term = ','
            goto 2
         end if
         IBLANK = INDEX(CSTR(I1:N),' ')
         ICOMMA = INDEX(CSTR(I1:N),',')
         if (IBLANK.eq.0.and.ICOMMA.eq.0.and.K.lt.KPARM) goto 801
         if (IBLANK.eq.0) IBLANK = N-I1+2
         if (ICOMMA.eq.0) ICOMMA = N-I1+2
         I2 = I1+MIN(IBLANK,ICOMMA)-1
         if (I2.gt.N) then
            term = '.'
         else
            term = CSTR(I2:I2)
         end if
4        continue
! Check for empty field.
      if (I2.eq.I1) goto 802
! Find optional '>'.
      I1 = I1+INDEX(CSTR(I1:I2),'>')
! Get F.
10    read (CSTR(I1:I2-1),90010,err=803) F
90010 format (f15.0)
      ibgn = i1
      iend = i2 - 1
      return
! Errors.
801   ISTAT = 1
      return
802   ISTAT = 2
      return
803   ISTAT = 3
      return
      end
! --------------------------- END RCF ------------------------------



! -------------------------- BEGIN RCI -----------------------
      subroutine RCI (KPARM,CSTR_IN,NCSTR_IN,I,ibgn,iend,ISTAT)
! RCI - [MUELLER.FS.GEN]CSMGENLB
! Read integer I, the KPARMth element in CSTR_IN(1:NCSTR_IN).
!  CSTR_IN contains one-or-more mixed-type fields
!(OLD COMMENT:c  separated by blanks or commas to the right of an optional '='.)
!  KPARMth element contains I to the right of an optional '>'.
! ISTAT= 0 for success;
!      = 1 if CSTR contains fewer than KPARM fields;
!      = 2 if KPARMth field is empty;
!      = 3 if KPARMth field is unreadable;
!      if ISTAT>0, return with I unchanged.

! Dates:  xx/xx/xx - Written by C. Mueller, USGS
!         06/22/98 - Modified by L. Baker
!         11/12/00 - Dave Boore introduced a working array, and a call to 
!                    rmv_tabs (this replaces a tab with a blank, but does not
!                    expand the tabs).  This was done because a file in which 
!                    fields are separated by tabs characters will not be parsed 
!                    correctly; apparently the tab characters (ascii 9) is 
!                    not the same thing as a blank.
!         12/04/00 - added ibgn, iend to rcf, rci, rcc calling arguments
!         06/11/01 - Disable use of "=" to indicate where the parameters
!                    start.  I did this because I sometimes add comments
!                    to the right of the parameters, and if these comments
!                    contain the character "=" then the parameters are not
!                    parsed correctly.  I reasoned that eliminating the check
!                    for "=" is the best solution to the problem (as opposed
!                    to restricting the use of "=" to only indicate 
!                    where the parameters start) because I rarely used
!                    "=" in this way.
!         06/12/01 - Minor modification (i2 = 0 rather than i2 = 1)
!         10/29/02 - Changed "rmv_tabs" to "tabs_rmv"
!         07/25/10 - Increase dimension of cstr
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character CSTR_IN*(*),term*1

      character cstr*500

      if (ncstr_in .gt. 500) then
         write(*,'(a, 1x,i5, a)') '  WARNING: IN RCI, NCSTR_IN = ', 
     :          ncstr_in,
     :         ' WHICH IS GREATER THAN THE DIMENSION OF THE'//
     :         ' WORK ARRAY; QUITTING!!!!!'
         return
      end if

      ncstr = ncstr_in

      cstr = ' '
      cstr(1: ncstr) = cstr_in
      call tabs_rmv(cstr, ncstr)

      ISTAT = 0
! Eliminate trailing white space.
      N = NCSTR+1
1     N = N-1
      if (CSTR(N:N).eq.' '.or.CSTR(N:N).eq.',') goto 1

! Changes on 6/11/01:
!c Find optional '='.
!      I2 = INDEX(CSTR(1:N),'=')
!      term = '='
      i2 = 0         ! DMB, 6/12/01
      term = ' '     ! DMB, 6/11/01
! Changes on 6/11/01:

! Find start and end of KPARMth field.
      do 4 K=1,KPARM
         I1 = I2
2        I2 = I1
3        I1 = I1+1
         if (CSTR(I1:I1).eq.' ') goto 3
         if (term.eq.' '.and.CSTR(I1:I1).eq.',') then
            term = ','
            goto 2
         end if
         IBLANK = INDEX(CSTR(I1:N),' ')
         ICOMMA = INDEX(CSTR(I1:N),',')
         if (IBLANK.eq.0.and.ICOMMA.eq.0.and.K.lt.KPARM) goto 801
         if (IBLANK.eq.0) IBLANK = N-I1+2
         if (ICOMMA.eq.0) ICOMMA = N-I1+2
         I2 = I1+MIN(IBLANK,ICOMMA)-1
         if (I2.gt.N) then
            term = '.'
         else
            term = CSTR(I2:I2)
         end if
4        continue
! Check for empty field.
      if (I2.eq.I1) goto 802
! Find optional '>'.
      I1 = I1+INDEX(CSTR(I1:I2),'>')
! Get I.
10    read (CSTR(I1:I2-1),90010,err=803) I
90010 format (i12)
      ibgn = i1
      iend = i2-1
      return
! Errors.
801   ISTAT = 1
      return
802   ISTAT = 2
      return
803   ISTAT = 3
      return
      end
! -------------------------- END RCI -----------------------



! ---------------------------------------------------------------- Tabs_Rmv
      subroutine tabs_rmv(string, nchar)

! Replaces tabs in the character string with a blank

! Dates: 11/04/00 - Written by D. Boore
!        10/29/02 - Renamed from rmv_tabs.for
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      character string*(*)


      do i = 1, nchar
        if(ichar(string(i:i)) .eq. 9) then
           string(i:i) = ' '
        end if
      end do

      return
      end
! ---------------------------------------------------------------- Tabs_Rmv

! ------------------ BEGIN SORT2 --------------------------
      SUBROUTINE sort2(n,arr,brr)
      
! Sorts arr in ascending order and applies the corresponding order to brr

      INTEGER n,M,NSTACK
      REAL arr(n),brr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,b,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          b=brr(j)
          do 11 i=j-1,l,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
            brr(i+1)=brr(i)
11        continue
          i=l-1
2         arr(i+1)=a
          brr(i+1)=b
12      continue
        if(jstack.eq.0)return
        ir=istack(jstack)
        l=istack(jstack-1)
        jstack=jstack-2
      else
        k=(l+ir)/2
        temp=arr(k)
        arr(k)=arr(l+1)
        arr(l+1)=temp
        temp=brr(k)
        brr(k)=brr(l+1)
        brr(l+1)=temp
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
          temp=brr(l)
          brr(l)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
          temp=brr(l+1)
          brr(l+1)=brr(ir)
          brr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
          temp=brr(l)
          brr(l)=brr(l+1)
          brr(l+1)=temp
        endif
        i=l+1
        j=ir
        a=arr(l+1)
        b=brr(l+1)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        temp=brr(i)
        brr(i)=brr(j)
        brr(j)=temp
        goto 3
5       arr(l+1)=arr(j)
        arr(j)=a
        brr(l+1)=brr(j)
        brr(j)=b
        jstack=jstack+2
        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort2'
        if(ir-i+1.ge.j-l)then
          istack(jstack)=ir
          istack(jstack-1)=i
          ir=j-1
        else
          istack(jstack)=j-1
          istack(jstack-1)=l
          l=i
        endif
      endif
      goto 1
      END
! ------------------ BEGIN SORT2 --------------------------
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
! --------------------- BEGIN UPSTR ----------------------------------
      Subroutine UPSTR ( text )
! Converts character string in TEXT to uppercase
! Dates: 03/12/96 - Written by Larry Baker
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

!
      Implicit   None
!
      Character  text*(*)
!
      Integer    j
      Character  ch
!
      Do 1000 j = 1,LEN(text)
         ch = text(j:j)
         If ( LGE(ch,'a') .and. LLE(ch,'z') ) Then
            text(j:j) = CHAR ( ICHAR(ch) - ICHAR('a') + ICHAR('A') )
         End If
 1000    Continue
!
      Return
      End
! --------------------- END UPSTR ----------------------------------
! --------------------------- BEGIN TRIM_C -----------------------
      subroutine trim_c(cstr, nchar)

! strips leading and trailing blanks from cstr, returning the
! result in cstr, which is now nchar characters in length

! Strip off tabs also.

! Here is a sample use in constructing a column header, filled out with 
! periods:

!* Read idtag:
!        idtag = ' '
!        read(nu_in, '(1x,a)') idtag
!        call trim_c(idtag, nc_id)
!* Set up the column headings:
!        colhead = ' '
!        colhead = idtag(1:nc_id)//'......' ! nc_id + 6 > length of colhead

! Dates: 12/23/97 - written by D. Boore
!        12/08/00 - pad with trailing blanks.  Otherwise some comparisons
!                   of the trimmed character string with other strings
!                   can be in error because the original characters are left
!                   behind after shifting.  For example, here is a string
!                   before and after shifting, using the old version:
!                      col:12345
!                           MTWH  before
!                          MTWHH  after (but nc = 4).
!        03/21/01 - Check for a zero length input string
!        11/09/01 - Change check for zero length string to check for all blanks
!        10/19/09 - Strip off tabs
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

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



!      if(nend .eq. 0) then
!        nchar = 0
!        return
!      end if

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


! --------------------------- BEGIN GET_LUN ----------------
      subroutine get_lun(lun)

! Finds a logical unit number not in use; returns
! -1 if it cannot find one.

! Dates -- 05/19/98 - Written by D. Boore, following
!                     Larry Baker's suggestion
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      logical isopen
      do i = 99,10,-1
        inquire (unit=i, opened=isopen)
        if(.not.isopen) then
          lun = i
          return
        end if
      end do
      lun = -1

      return
      end
! --------------------------- END GET_LUN ----------------
     
! ------------------------------------------------------------------ skipcmnt
      subroutine skipcmnt(nu, comment, ncomments)

! Skip text comments in the file attached to unit nu, but save skipped 
! comments in character array comment.  Skip at least one line, and more as 
! long as the lines are preceded by "|" or "!".

! Dates: 04/16/01 - Written by D. Boore
!        12/07/01 - Added check for eof
!        11/04/03 - Use trim_c to trim possible leading blank
!        02/03/07 - Initialize comments to blank
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

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
!----------------- BEGIN HARMOSCF -----------------------------
      function harmoscf( f, fosc, damp, idva)
! harmonic oscillator displacement frequency response to the oscillator equation
! (for response spectra, harmoscf*A = SD)
!   o_ddot + 2*damp*(2*pi*fosc)*o_dot + (2*pi*fosc)^2*o = a
! where ddot. dot = double and single time differentiation, o = oscillator displacement,
! a = input acceleration.
! idva = 0 for response to displacement
! idva = 2 for response to acceleration
! idva = 1 returns 0 for the response

! Note that this asymptotic values:

! f/fosc -> 0:  harmoscf = 1/omega_osc^2
! f/fosc -> 0:  harmoscf*A = SD = A/omega_osc^2
! where omega_osc = 2*pi*fosc

! Because PSA = omega_osc*2 * SD, then for f/fosc-> 0,
!         PSA = A, which is correct.
 
! Dates: 12/01/83 - Written by D. Boore
!        03/25/89 - a modification
!        07/30/00 - Changed dum for both cases of idva
!                                 (see notes from same date).
!        02/02/11 - Here are some notes in the \smsim\readme.txt file
!                    that are related to the above (I may have hard copy notes elsewhere):
!                      Version 1.90: 07/29/00: Major change to tbldrvrr.  
!                      Obtain input from a control file, eliminate redundancy in code.  
!                      Added computation of twopi to smsim.fi, and corrected an error in 
!                      harmoscf in rvtdsubs when idva = 2.  Luckily, I think harmoscf is only 
!                      called by random vibration routine smsim_rv, and in that routine I 
!                      force idva = 0.   
!        02/02/11 - Added comments about normalization (a pre-07/30/00 version returned unity
!                                 for f/fosc-> 0 when idva = 2, which is not correct).
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)


      pi = 4.0*atan(1.0)
      twopi = 2.0 * pi

      if (idva /= 0 .and. idva /= 2) dum = 0.0
      if (idva == 0) dum = (twopi*f)**2/twopi**2
      if (idva == 2) dum =          1.0/twopi**2

      harmoscf = dum/sqrt( ( fosc*fosc - f*f )**2
     * + ( 2.0*f*fosc*damp )**2 )
     
      return
      end
!----------------- END HARMOSCF -----------------------------
! ---------------------- BEGIN MNMAX ----------------------
      subroutine mnmax(a,nstrt,nstop,ninc,amin,amax) 
!
! Dates: 09/07/84 - Written by D. Boore
!        11/02/12 - replace obsolete if statement 
! 
!
      dimension a(*)
      
      amax = a(nstrt)
      amin=amax 
      
      do i=nstrt,nstop,ninc

        if ( a(i) < amin) then
          amin=a(i)
        else if ( a(i) > amax) then
          amax=a(i) 
        end if
  
      end do
      
      return                  
      end                     
! ---------------------- END MNMAX ----------------------

! ---------------------- BEGIN MNMAXIDX ----------------------
      subroutine mnmaxidx(a,nstrt,nstop,ninc,
     :                    amin,amax,indx_min,indx_max) 

! A rewrite of mnmax, returning the indices of the min and max values
!
! Dates: 10/05/99 - written by D. M. Boore
!        05/16/02 - Replaced "dimension a(1)" with "real a(*)"
!        11/03/12 - Replace if-then with if--then-else

      real a(*)
      amax = a( nstrt)
      indx_max = nstrt
      amin=amax         
      indx_min = nstrt
      do i=nstrt,nstop,ninc
      
        if (a(i) > amax) then
          amax=a(i)
          indx_max = i
        else if (a(i) < amin) then
          amin=a(i)
          indx_min = i
        end if
        
      end do
      
      return                  
      end                     
! ---------------------- END MNMAXIDX ----------------------

! ------------------------ BEGIN AVG_STD ----------------------------
      SUBROUTINE avg_std(data,n,avg,std,itype)
      
! Computes mean and standard deviation for an array of data.
! This is based on the Numerical Recipes program avevar.
! Itype specifies the type of mean:
! itype = 1: arithmetic mean, std = standard deviation
! itype = 2: geometric mean, std = factor by which mean is divided
!                                  and multiplied to yield the
!                                  equivalent of minus and plus
!                                  one standard deviation

! Dates: 02/21/09 - Written by D. Boore, modified from avevar

      INTEGER n
      REAL avg,var,std, data(n), data4avg
      INTEGER j, itype
      REAL s,ep
      
      avg = 0.0
      do j = 1, n
        if (itype .eq. 1) then
          data4avg = data(j)
        else
          data4avg = alog10(data(j))
        end if
        avg = avg + data4avg
      end do
      avg = avg/float(n)
      var = 0.0
      ep = 0.0
      do j = 1, n
        if (itype .eq. 1) then
          data4avg = data(j)
        else
          data4avg = alog10(data(j))
        end if
        s = data4avg - avg   ! eq (14.1.8) in Press et al. (1992)
        ep = ep + s
        var = var + s*s
      end do
      if (n .eq. 1) then
        var = 0.0
      else
        var = (var - ep**2/float(n))/float(n-1)
      end if

      if (itype .eq. 1) then
        std = sqrt(var)
      else
        avg = 10.0**avg
        std = 10.0**sqrt(var)
      end if


      return
      END
! ------------------------ END AVG_STD ----------------------------

! ------------------------ BEGIN DISTAZ ----------------------
      subroutine distaz( wlongsign, alat, along, blat, blong,
     * rdeg, rkm, az, baz)
! 
! compute distances, azimuths using formulas from
! Bruce Julian.
!
! latest modification: 1/27/84
!
!        11/24/07 - Trap for colocated pair in subroutine distaz
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      if (alat .eq. blat .and. along .eq. blong) then
        rdeg = 0.0
        rkm = 0.0
        az = 0.0
        baz = 0.0
        return
      end if

      pi = 4.0 * atan( 1. )
      dtor = pi/ 180.
!
! convert from degrees to radians and correct sign of
! longitude so that east longitude is positive.
!
      alatr = dtor * alat
      alongr = -dtor * along * wlongsign
      blatr = dtor * blat
      blongr = -dtor * blong * wlongsign
!
! compute geocentric latitudes.
!
      alatr = atan( 0.993305 * tan( alatr ) )
      blatr = atan( 0.993305 * tan( blatr ) )
!
! compute latitude dependent quantities
!
      ca = cos( alatr )
      cb = cos( blatr )
      sa = sin( alatr )
      sb = sin( blatr )
!
! now compute other quantities
!
      a = cb * sin( blongr - alongr )
      b = ca * sb - sa * cb * cos( blongr - alongr )
      cd = ca * cb * cos( blongr - alongr ) + sa * sb
      sd = sqrt( a*a + b*b )
!
! compute distances
!
      rdeg = atan2( sd, cd )/ dtor
      rkm = 111.19 * rdeg
!
! compute azimuth (from a to b) and make it positive.
!
      az = atan2( a, b )/ dtor
      if ( az .lt. 0.0 ) az = az + 360.0
!
! compute back azimuth (from b to a) and make it positive.
!
      a = ca * sin( alongr - blongr )
      b = cb * sa - sb * ca * cos( alongr - blongr )
      baz = atan2( a, b)/ dtor
      if ( baz .lt. 0.0 ) baz = baz + 360.0
!
      return
      end
! ------------------------ END DISTAZ ----------------------


! --------------- BEGIN TIME_DIFF ---------------------------------
      subroutine time_diff(time_start, time_stop, time_elapsed)

! Dates: 02/18/09 - Written by D.M. Boore
!        04/28/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)
! To be used with
! Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
!      character datx*8, timx*10
!      call DATE_AND_TIME( datx, timx )
! Date is returned as 'CCYYMMDD'
! Time is returned as 'hhmmss.sss'

      implicit none
      character, intent(in) :: time_start*(*), time_stop*(*)
      real, intent(out) :: time_elapsed
      real ::  secb, sece 
      integer :: ihb, imb, ihe, ime

      read(time_start(1:10),'(i2,i2,f6.3)') 
     :                       ihb, imb, secb
      read(time_stop(1:10),'(i2,i2,f6.3)') 
     :                       ihe, ime, sece
      time_elapsed = 
     :  3600.0*float(ihe-ihb) + 60.0*float(ime-imb) + sece-secb 

      end subroutine time_diff
! --------------- END TIME_DIFF ---------------------------------


! ---------------------------------------------------------- poles_zeros_response
      subroutine poles_zeros_response(f, gain, nz, np, sp, idva, 
     :                              amp, phase)
      
! np is the total number of poles, both real poles and complex poles (which must be 
! conjugates of one another).

! idva: 0=displacement response
!       1=velocity response
!       2=acceleration response

! Dates: 01/16/10 - Written by D. Boore (derived from Wielandt's polzero.g32 program)
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

       
      real f, gain, amp, phase, twopi
      complex work, s, sp(*)
      integer i, nz, np, idva
      
      twopi = 2.0 * 4.0 * atan(1.0)
 
      s = cmplx(0.0, twopi * f)
      
      work = s**(nz-idva)
      DO i = 1, np
        work = work/(s-sp(i))
      END DO
      
      amp = gain*cabs(work)
      
      ampimg = aimag(work)
      ampreal = real(work)
      
      if (ampimg == 0.0 .and. ampreal == 0.0) then
        write(*,*) ' for f= ', f, ' ampreal, ampimg = 0'
        phase = 0.0
      else
        phase = atan2(aimag(work),real(work))
      end if
      
      RETURN
      END
! ---------------------------------------------------------- poles_zeros_response


   
   
  
  
! ----------------------------------------------- poles_zeros_values
      subroutine poles_zeros_values(
     :      instr_type, gain, fgain, nz, np, sp)

! Returns poles and zeros for common instrument types.
! The response is relative to displacement, for 
! velocity and acceleration response reduce nz by 1 and 2, respectively.

! To obtain the specified gain at f-fgain, call poles_zeros_response
! with a gain of 1.0 and f=fgain.  Let the output be response_1.  Then
! call poles_zeros_response again with a gain of gain/response_1.

! instr_type = 'WA', 'WWSSN-SP', 'USGS-SP', 'PIDC-SP', ...

! Dates: 07/07/10 - Written by D. M. Boore, based on acc2seismo_response
 
      complex sp(*) 
      
      character instr_type*(*), work_c*100
      
      work_c = ' '
      work_c = instr_type      
      call upstr(work_c)
      call trim_c(work_c,nc_work_c)
      
      select case(work_c(1:nc_work_c))
      
      case('WA')
      
! gain from Uhrhammer, R.A. and E.R. Collins (1990). Synthesis of Wood-
! Anderson seismograms from broadband digital records, \bssa {\bf 80},
! 702--716. (V = 2080$, T_0 = 0.8 (f_0 = 1/0.8 = 1.25), and \eta = 0.69).
!
! poles and zeros from above values of f_o and eta, using these relations:
!       2*pi*f_0 = sqrt(sp(1)**2 + sp(2)**2)
!       eta = abs(sp(1))/(2*pi*f_0)
      
        gain = 2080.0     
        fgain = 0.01    
        nz = 2
        np = 2
        sp(1) = cmplx( -5.41925, -5.68479)  
        sp(2) = cmplx( -5.41925, +5.68479)  
!        sp(1) = cmplx( -5.49779, -5.60886) ! standard_tab.doc (provided by
!        sp(2) = cmplx( -5.49779, +5.60886) ! Jim Dewey) containing notes from 
                                            ! Charles R. Hutt, 1 December 2006
      case('WWSSN-SP')      

! values from standard_tab.doc (provided by
! Jim Dewey) containing notes from 
! Charles R. Hutt, 1 December 2006

        gain = 1.0
        fgain = 1.0
        nz = 3
        np = 5
        sp(1) = cmplx( -3.72500, -6.22000)
        sp(2) = cmplx( -3.72500, +6.22000)
        sp(3) = cmplx( -5.61200, +0.00000)
        sp(4) = cmplx(-13.24000, +0.00000)
        sp(5) = cmplx(-21.08000, +0.00000)
        
      case('USGS-SP')      

! values from esupplement (Appendix 1) to Granville et al (BSSA 95, 1809-1824)
        gain = 1.0
        fgain = 1.0
        nz = 5  ! Granville et al specify 4 zeros for velocity)
        np = 8
        sp(1) = cmplx( -2.22144, +2.22144)
        sp(2) = cmplx( -2.22144, -2.22144)
        sp(3) = cmplx( -4.66503, +4.66503)
        sp(4) = cmplx( -4.66503, -4.66503)
        sp(5) = cmplx( -11.7736, +11.7736)
        sp(6) = cmplx( -11.7736, -11.7736)
        sp(7) = cmplx( -28.8787, +28.8787)
        sp(8) = cmplx( -28.8787, -28.8787)
         
      case('PIDC-SP')      

! values from esupplement (Appendix 2) to Granville et al (BSSA 95, 1809-1824)
        gain = 1.0
        fgain = 1.0
        nz = 7  ! Granville et al specify 6 zeros for velocity)
        np = 12
        sp(1) = cmplx( -5.02655, +0.00000)
        sp(2) = cmplx( -2.51327, +4.35312)
        sp(3) = cmplx( -2.51327, -4.35312)
        sp(4) = cmplx( -28.2743, +0.00000)
        sp(5) = cmplx( -14.1372, -24.4863)
        sp(6) = cmplx( -14.1372, +24.4863)
! Complete response is acausal, obtained by filtering twice; thus the poles and zeros
! are repeated. BUT NOTE: This does not give zero phase shift!  I need to
! use complex conjugates of something. A simple way to do this is set the
! phase to 0.0 in the calling program.
        sp(7) = cmplx( -5.02655, +0.00000)
        sp(8) = cmplx( -2.51327, +4.35312)
        sp(9) = cmplx( -2.51327, -4.35312)
        sp(10) = cmplx( -28.2743, +0.00000)
        sp(11) = cmplx( -14.1372, -24.4863)
        sp(12) = cmplx( -14.1372, +24.4863)

      case default
      
        write(*,*) ' ERROR: instr_type = '//
     :               work_c(1:nc_work_c)//
     :             ' not recognized; QUITTING!'
        stop
     
      end select

      return
      end
! ----------------------------------------------- poles_zeros_values
! ------------------------------------------------------------------ Get_Avg
      subroutine Get_Avg(a, 
     :  n_start, n_stop, npts, sps, icorr_cumavg, avg)

! Find mean of a between the two indices

! If itype = 1, then half weight is given to the first and last points
! (as it would be if computing velocity when the data before and after the
! indices are 0.0).

! Also consider (no confidence limits, however):

!      SUBROUTINE avg_std(data,n,avg,std,itype)
      
! Computes mean and standard deviation for an array of data.
! This is based on the Numerical Recipes program avevar.
! Itype specifies the type of mean:
! itype = 1: arithmetic mean, std = standard deviation
! itype = 2: geometric mean, std = factor by which mean is divided
!                                  and multiplied to yield the
!                                  equivalent of minus and plus
!                                  one standard deviation

! Or

!      subroutine mean_std_cl(a,npts,mean,std,seom,cl70,cl90,cl95,cl99)
! Computes mean, standard deviation, and confidence limits of the array entries





! Dates: 12/17/02 - Written by D. Boore
!        12/18/02 - Add icorr_cumavg to try to get program smc_pad to give 0.0
!                   velocity at the end of the data segment
!        12/18/02 - Rethinking computation of average when want to
!                   apply it to velocity case (looking at acc2vd.for algorithm)
!        05/01/15 - Replaced comment characters * or C with ! (The Fortran 95 standard)

      real a(*)
      real*8 cum_avg

      avg = 0.0
!      if (t4mean_start .lt. 0.0) t4mean_start = 0.0
!      if (t4mean_stop .gt. tend) t4mean_stop = tend
!      n_start = nint(sps * t4mean_start + 1.1)
!      n_stop = nint(sps * t4mean_stop + 1.1)
      if (n_start .lt. 0)    n_start = 1
      if (n_stop .gt. npts) n_stop = npts
      n_4mean = n_stop - n_start + 1
!      t4mean = float(n_4mean -1)/sps
      cum_avg = 0.0
      do i = n_start, n_stop
        cum_avg = cum_avg + dble(a(i))
      end do

      if (icorr_cumavg .eq. 1) then  ! Effectively ignore icorr_cumavg now
!        cum_avg = cum_avg - 0.5d00*(dble(a(n_start))+dble(a(n_stop)))         
!        avg = sngl(cum_avg/dble(float(n_4mean-1)))
        avg = sngl(cum_avg/dble(float(n_4mean)))
      else
        avg = sngl(cum_avg/dble(float(n_4mean)))
      end if

      return
      end
! ------------------------------------------------------------------ Get_Avg
 
