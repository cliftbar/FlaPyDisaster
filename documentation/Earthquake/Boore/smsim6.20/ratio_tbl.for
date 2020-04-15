      Program Ratio_TBL

*  Reads 2 tables made with TBL_RV_DRVR and forms the ratio.

* Here is a sample control file:
* TBL10P10.TBL TBL20P10.TBL rat_0p10.tbl
* TBL10P20.TBL TBL20P20.TBL rat_0p20.tbl
* TBL10P50.TBL TBL20P50.TBL rat_0p50.tbl
* TBL111P0.TBL TBL211P0.TBL rat_11p0.tbl
* TBL11P00.TBL TBL21P00.TBL rat_1p00.tbl
* TBL12P00.TBL TBL22P00.TBL rat_2p00.tbl
* TBL1_PGA.TBL TBL2_PGA.TBL rat__pga.tbl
* TBL1_PGV.TBL TBL2_PGV.TBL rat__pgv.tbl
* stop ! starting in any column, any mix of upper and lower case

* Dates: 10/31/95 - Written by D.M. Boore
*        02/07/96 - Modified to account for changes in output format
*                   of DRVR4ART and to add magnitude column headings
*        05/22/96 - Obtained rrlow, rrhigh interactively, and placed
*                   setting loops over mag and distance to outside the
*                   loop over file names.
*        12/05/98 - Removed obtaining rrlow, rrhigh interactively, and
*                   modified the format to be that of the current of
*                   tbldrvrr.for.  Get mags, distances from the headers.
*        07/29/00 - Revised to take into account the revised tbldrvrr.for
*        07/30/00 - Use rcc to read control, and make output table same
*                   format as input table (same number of headers).  Also,
*                   ask if entries are log values.
*        12/04/00 - added ibgn, iend to rcf, rci, rcc calling arguments
*        03/12/01 - Improve dealing with end of control file.  Three things
*                   will signal the end: 1) an eof character; 2) a blank
*                   line; 3) "stop" in a line
*        06/14/02 - Suggest name of control file.
*        02/09/03 - Make sure that leading and trailing blanks are removed
*                   from the input file names (because LF95 allows file names
*                   with blanks).
*        08/16/05 - Renamed from "ratiotbl" to "ratio_tbl"
*        11/15/05 - Revise to account for different columns (no log, log)
*                   and modernize
*        05/11/07 - Removed "\smsim\" from include statements
!        08/24/10 - Modernize, account for different table structure from tbl_rv_drvr
!        10/12/10 - Removed dashed line from the output, for ease in importing into CoPlot
!        01/08/11 - Add check for existence of input files
!        04/15/11 - Replaced h_eff with f_ff.

      character f_ctl*80, f_sum*200
      character table1*200, table2*200, table3*200 
      character title*200, title_out*400, buff*165
      character buf_t1h1*100, buf_t2h1*100, buf_h2*60, buf_h3*60
      character answr*1, buf_in*400, buf_upper*400, dash5(40)*5,
     :          buf4*4
     
      character header*100

      real psa1(30), psa2(30), alpsa1(30), alpsa2(30)
      
      integer status

      logical f_ctl_exist, f_exist
      
      DO
        f_ctl = ' '
        write(*, '(a)') 
     :    ' Enter name of control file '//
     :    '(cr = ratio_tbl.ctl; ctl-brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl(1:4) .eq. '    ') f_ctl = 'ratio_tbl.ctl'
        call trim_c(f_ctl,nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_ctl_exist)
        IF (f_ctl_exist) EXIT
        write(*,'(a)') ' ******* CONTROL FILE '//f_ctl(1:nc_f_ctl)//
     :     ' DOES NOT EXIST ******* '
         
      END DO
      call get_lun(nu_ctl)
      open(unit=nu_ctl, file=f_ctl(1:nc_f_ctl), status='unknown')

      write(*,'(a\)') 
     :           ' Enter name of summary file: '
      f_sum = ' '
      write(*, '(a)') 
     :    ' Enter name of summary file '//
     :    '(cr = ratio_tbl.sum; ctl-brk to quit): '
      read(*, '(a)') f_sum
      if (f_sum(1:4) .eq. '    ') f_sum = 'ratio_tbl.sum'
      call trim_c(f_sum,nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum, file=f_sum(1:nc_f_sum), status='unknown')

!      answr = ' '
!      write(*,'(a\)') '  Are table values log values? (cr or y=yes)'
!      read(*,'(a)') answr
!      call upstr(answr)

      loop over files: DO
        buf_in = ' '
      read(nu_ctl,'(a)',iostat=status) buf_in
      IF (status /= 0) THEN
        EXIT loop over files
      END IF
      call trim_c(buf_in, nc_buf_in)
      buf4= ' '
      buf4 = buf_in(1:4)
      call upstr(buf4)
      IF (buf4(1:4) .eq. 'STOP') THEN
        EXIT loop over files
      END IF
      
            
      table1 = ' '
      call rcc(1, buf_in, nc_buf_in, table1, nc1,ibgn,iend)
      table2 = ' '
      call rcc(2, buf_in, nc_buf_in, table2, nc2,ibgn,iend)
      table3 = ' '
      call rcc(3, buf_in, nc_buf_in, table3, nc3,ibgn,iend)

      write(*,'(2a)') '  Table1 = ', table1(1:nc1)
      write(*,'(2a)') '  Table2 = ', table2(1:nc2)
      write(*,'(2a)') '  Table3 = ', table3(1:nc3)

      write(nu_sum,'(a, 3(1x,i2))') 
     :      '  nc1, nc2, nc3 = ', nc1, nc2, nc3
     
      write(nu_sum,'(2a)') '  Table1 = ', table1(1:nc1)
      inquire(file=table1(1:nc1), exist=f_exist)
      IF (f_exist) THEN
        write(nu_sum,'(a)') ' Exists!'
      ELSE
        write(nu_sum,'(a)') ' ******* FILE '//table1(1:nc1)//
     :     ' DOES NOT EXIST; QUIT ******* '
        write(*,'(a)') ' ******* FILE '//table1(1:nc1)//
     :     ' DOES NOT EXIST; QUIT ******* '
        EXIT loop over files
      END IF
      
      write(nu_sum,'(2a)') '  Table2 = ', table2(1:nc2)
      inquire(file=table2(1:nc1), exist=f_exist)
      IF (f_exist) THEN
        write(nu_sum,'(a)') ' Exists!'
      ELSE
        write(nu_sum,'(a)') ' ******* FILE '//table2(1:nc2)//
     :     ' DOES NOT EXIST; QUIT ******* '
        write(*,'(a)') ' ******* FILE '//table2(1:nc2)//
     :     ' DOES NOT EXIST; QUIT ******* '
        EXIT loop over files
      END IF      
      
      write(nu_sum,'(2a)') '  Table3 = ', table3(1:nc3)

      if (table1(1:8) .eq. '        ') then          ! check for blank line
        EXIT loop over files
      END IF
      
      call get_lun(nu_t1)      
      open(unit=nu_t1, file=table1(1:nc1), status='unknown')
      
      inquire(file=table2(1:nc2), exist=f_exist)
      IF (.not. f_exist) THEN
        write(*,'(a)') ' ******* FILE '//table2(1:nc2)//
     :     ' DOES NOT EXIST; QUIT ******* '
        EXIT loop over files
      END IF
      call get_lun(nu_t2)
      open(unit=nu_t2, file=table2(1:nc2), status='unknown')
      
      call get_lun(nu_t3)
      open(unit=nu_t3, file=table3(1:nc3), status='unknown')
      
      write(nu_sum,'(a, 3(1x,i2))') 
     :      '  nu_t1, nu_t2, nu_t3 = ', nu_t1, nu_t2, nu_t3

      write(nu_t3, '(a)') 
     :    ' ratio of ground motions followed by diff of logs' 
     
      nhead2read = 4
      DO i = 1, nhead2read 
        header = ' '
        read(nu_t1,'(a)') header
        call trim_c(header, nc_header)
        write(nu_t3, '(1x,a)') '!'//header(1:nc_header)
      END DO
      
      write(nu_t3,'(1x,a)') '!'
      
      nhead2read = 4
      DO i = 1, nhead2read 
        header = ' '
        read(nu_t2,'(a)') header
        call trim_c(header, nc_header)
        write(nu_t3, '(1x,a)') '!'//header(1:nc_header)
      END DO
      
      title_out = ' '
      title_out = 
     :            table1(1:nc1)//' divided by '//
     :            table2(1:nc2)//' = '//table3(1:nc3)
      call trim_c(title_out, nc_title_out)
      write(nu_t3, '(a)') title_out(1:nc_title_out)

      header = ' '
      read(nu_t1,'(a)') header
      call trim_c(header, nc_header)
      read(nu_t1, *) amaglow, amaghigh, damag, nmag

      write(nu_t3, '(a)') header(1:nc_header)      
      write(nu_t3, '(3(1x,f5.2),1x,i3)')
     :        amaglow, amaghigh, damag, nmag
     
      header = ' '
      read(nu_t1,'(a)') header
      call trim_c(header, nc_header)
      read(nu_t1, *) rlow, rhigh, dlogr, nr

      write(nu_t3, '(a)') header(1:nc_header)      
      write(nu_t3,'(3(1x,f7.2),1x,i3)') rlow, rhigh, dlogr, nr

      buff = ' '
      read(nu_t1, '(a)') buff  ! column headers

      call skip(nu_t2,1)
      read(nu_t2, *) amaglow2, amaghigh2, damag2, nmag2
      if (amaglow2  .ne. amaglow  .or.
     :    amaghigh2 .ne. amaghigh .or.
     :    damag2    .ne. damag         ) then
          write(*,'(a,6es11.3)') 
     :      ' ABORT! amaglow, amaghigh, damag .ne.;  ', 
     :        amaglow2, amaglow,
     :        amaghigh2, amaghigh,
     :        damag2, damag
          EXIT loop over files
      endif

      call skip(nu_t2, 1)
      read(nu_t2, *) rlow2, rhigh2, dlogr2, nr2
      if (rlow2  .ne. rlow  .or.
     :    rhigh2 .ne. rhigh .or.
     :    dlogr2 .ne. dlogr         ) then
          write(*,'(a,6es11.3)') 
     :      ' ABORT! rlow, rhigh, dlogr .ne.;  ', 
     :        rlow2, rlow,
     :        rhigh2, rhigh,
     :        dlogr2, dlogr
          EXIT loop over files
      endif

      call skip(nu_t2, 1)


      write(nu_t3, '(5x,a, 1x,a, 4x,a, 7x,a, 2x,a,
     :               2x,a, 4x,a,
     :               2x,
     :               40(2x,f5.2))')  
     : 'Rxh', 'logRxh', 'h', 'R', 'logR',
     : 'f_ff', 'rmod',
     :  (amaglow+float(i-1)*damag, i = 1, nmag),
     :  (amaglow+float(i-1)*damag, i = 1, nmag)
     
        do i = 1, 40
          dash5(i) = '-----'
        end do

!      write(nu_t3, '(5x,a, 1x,a, 4x,a, 7x,a, 2x,a,
!     :               2x,a, 4x,a,
!     :               2x,
!     :               40(2x,a5))')  
!     : '---', '------', '-', '-', '----',
!     : '-----', '----',
!     :  (dash5(i), i = 1, nmag),
!     :  (dash5(i), i = 1, nmag)
     
      do ir = 1, nr
        read(nu_t1, *) 
     :      rxh1, alogrxh1, h1, r1, alogr1,
     :      f_ff1, rmod1,
     :          (psa1(imag), alpsa1(imag), imag=1,nmag)
        read(nu_t2, *)
     :      rxh2, alogrxh2, h2, r2, alogr2, 
     :      f_ff2, rmod2,
     :          (psa2(imag), alpsa2(imag), imag=1,nmag)
     
        write(nu_t3, '(
     :      1x,f7.2, 2x,f5.2, 
     :      1x,f4.1, 1x,f7.2, 1x,f5.2,
     :      2x,f5.1, 1x,f7.2,
     :      2x,
     :      40(2x,f5.2))') 
     :      rxh1, alogrxh1, 
     :      h1, r1, alogr1, 
     :      f_ff1, rmod1,
     :       (psa1(imag)/psa2(imag), imag=1,nmag),
     :      ((alpsa1(imag)-alpsa2(imag)), imag=1,nmag)
      end do

      close(unit=nu_t1)
      close(unit=nu_t2)
      close(unit=nu_t3)

      write(nu_sum, '(4(1x,a))') 
     :  ' Processed: ', table1, table2, table3

      END DO loop over files

      close(unit=nu_ctl)
      close(unit=nu_sum)

      stop
      end

* --------------------- BEGIN LOCATE -----------------
      SUBROUTINE locate(xx,n,x,j)
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
* --------------------- END LOCATE -----------------

      include 'rv_td_subs.for'
      include 'smsim_util_subs.for'
      

      