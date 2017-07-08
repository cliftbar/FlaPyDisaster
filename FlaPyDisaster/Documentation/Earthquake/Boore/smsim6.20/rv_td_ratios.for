      Program rv_td_ratios
      
*!Control file for program RV_TD_Ratios
*! As many comment lines as desired, each starting with "!"
*! The string "pp:" indicates a new set of processing parameters
*! to be applied to the following smc files.  The parameters are given on the
*! following lines.
*! Revision of program involving a change in the control file on this date:
*   01/30/11
*!Name of summary file:
* rv_td_ratios.sum
*! rv/td (0) or td/rv (/=0)?
*  0
*!List of files
*! This version assumes an output file name, followed by two TD files and then by two RV files, made using
*! the two choices for osc_crrctn.  If the ratio of only two files is desired, just
*! duplicate the entries.
* rv_td_ratios_m5_r005.out
* lc_0.04_osc_crrctn_1.m5.00r005.0_rs.td.col
* box.m5.00r005.0_rs.td.col
* lc_0.04_osc_crrctn_1.m5.00r005.0_rs.rv.col
* lc_0.04_osc_crrctn_2.m5.00r005.0_rs.rv.col
*Stop

! Dates: 01/12/11 - Written by D. Boore
!        01/15/11 - Extract durex from first RV file and use it to write a column of normalized period.
!                   At the time of this revision, gm_rv_drvr, modified to produce durex, has been
!                   run only for the rv1 cases, so I cannot check durex_rv2 against durex_rv1
!                   for equality.  This should not be necessary, however, because I do check that
!                   M and R are equal.
!        01/17/11 - Also extract trms and use it to normalize period
!        01/30/11 - Allow rv/td or td/rv
      
      real dum(20) 
      real per_td1(600), per_td2(600), per_rv1(600), per_rv2(600)  
      real m_td1(600), m_td2(600), m_rv1(600), m_rv2(600)  
      real r_td1(600), r_td2(600), r_rv1(600), r_rv2(600)  
      real sd_td1(600), sd_td2(600), sd_rv1(600), sd_rv2(600) 
      real durex_rv1(600), trms_rv1(600)
      
      character f_td1*200, f_td2*200, f_rv1*200, f_rv2*200, f_out*200
      character cmnts2skip(50)*80, buf*200, buf4*4
      
      integer status
      
      character date_ctl_correct*8, date_ctl_in*30

      character f_ctl*200, f_sum*200 
      logical f_exist 

      f_exist = .false.
      do while (.not. f_exist)
        f_ctl = ' '
        write(*, '(a\)') 
     :    ' Enter name of control file (cr=RV_TD_Ratios.CTL;'//
     :    ' ctl-brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl(1:4) .eq. '    ') f_ctl = 'rv_td_ratios.ctl'
        call trim_c(f_ctl, nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_exist)
        if (.not. f_exist) then
          write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        end if
      end do

      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl(1:nc_f_ctl),status='unknown')

      call skipcmnt(nu_ctl, cmnts2skip,nc_cmnts2skip)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)
      
      date_ctl_correct = ' '
      date_ctl_correct = '01/30/11'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      if (date_ctl_correct(1:nc_date_ctl_correct) .ne. 
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong date; update your '//
     :       'control file and rerun the program!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)

      f_sum = ' '
      read(nu_ctl,'(a)') f_sum
      call trim_c(f_sum,nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')

      write(nu_sum,'(a)') ' Output of program RV_TD_Ratios:'
      write(nu_sum,'(2a)') '   Control file = ', f_ctl(1:nc_f_ctl)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl,*) irv_div_td
      write(nu_sum,'(1x,a, 1x, i2)') 
     :   'irv_div_td (= 0, rv/td; /= 0, td/rv): ', irv_div_td
     
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)

      loop over files: DO
        f_out = ' '
        read(nu_ctl,'(a)',iostat=status) f_out
        if(status /= 0) then
          EXIT loop over files
        end if
        call trim_c(f_out,nc_f_out)
        buf4 = ' '
        buf4 = f_out(1:4)
        call upstr(buf4)
        if(nc_f_out == 0 .or. f_out(1:1) == '!') then
          CYCLE loop over files
        end if
        if(buf4(1:4) == 'STOP') then
          EXIT loop over files
        end if
      
        call get_lun(nu_out)
        open(unit=nu_out,file=f_out(1:nc_f_out),status='unknown')

        write(nu_sum,'(2a)') '   Output file = ', f_out(1:nc_f_out)
      
        if (irv_div_td == 0) then
          write(nu_out,'(
     :     5x,a, 4x,a, 6x,a,
     :     7x,a, 7x,a,
     :     7x,a, 7x,a,
     :     1x,a, 1x,a,
     :     1x,a, 1x,a,
     :     3x,a, 4x,a
     :                     )') 
     :    'per', 'm', 'r', 
     :    'sd_td1', 'sd_td2',
     :    'sd_rv1', 'sd_rv2', 
     :    'rv1_td1', 'rv1_td2',
     :    'rv2_td1', 'rv2_td2',
     :    'per/durex', 'per/trms'
        else
          write(nu_out,'(
     :     5x,a, 4x,a, 6x,a,
     :     7x,a, 7x,a,
     :     7x,a, 7x,a,
     :     1x,a, 1x,a,
     :     1x,a, 1x,a,
     :     3x,a, 4x,a
     :                     )') 
     :    'per', 'm', 'r', 
     :    'sd_td1', 'sd_td2',
     :    'sd_rv1', 'sd_rv2', 
     :    'td1_rv1', 'td1_rv2',
     :    'td2_rv1', 'td2_rv2',
     :    'per/durex', 'per/trms'
        end if        
      

        f_td1 = ' '
        read(nu_ctl,'(a)',iostat=status) f_td1
        if(status /= 0) then
          write(*,*) ' Error reading r_td1, QUITTING'
          EXIT loop over files
        end if
        call trim_c(f_td1,nc_f_td1)
        f_exist = .false.
        inquire(file=f_td1(1:nc_f_td1), exist=f_exist) 
        if (.not. f_exist) then
          write(*,'(a)') ' ****** FILE '//f_td1(1:nc_f_td1)// 
     :          ' DOES NOT EXIST; QUITTING!!! ******* '
          EXIT loop over files
        end if        
        write(*,'(a)') ' f_td1: '//f_td1(1:nc_f_td1)
        write(nu_sum,'(a)') ' f_td1: '//f_td1(1:nc_f_td1)
        call get_lun(nu_td1)
        open(unit=nu_td1,file=f_td1(1:nc_f_td1),status='unknown')        
        call skip(nu_td1,1)
        n_td1 = 0
        loop over TD1 period: DO
          dum = 0.0
          read(nu_td1,*,iostat=status) (dum(i), i=1,14)
          if (status /= 0) then
            close(nu_td1)
            EXIT loop over TD1 period
          end if
          n_td1 = n_td1 + 1
          per_td1(n_td1) = dum(2)
          m_td1(n_td1)   = dum(4)
          r_td1(n_td1)   = dum(5)
          sd_td1(n_td1)  = dum(14)
        END DO loop over TD1 period
  
        f_td2 = ' '
        read(nu_ctl,'(a)',iostat=status) f_td2
        if(status /= 0) then
          write(*,*) ' Error reading r_td2, QUITTING'
          EXIT loop over files
        end if
        call trim_c(f_td2,nc_f_td2)
        f_exist = .false.
        inquire(file=f_td2(1:nc_f_td2), exist=f_exist) 
        if (.not. f_exist) then
          write(*,'(a)') ' ****** FILE '//f_td2(1:nc_f_td2)// 
     :          ' DOES NOT EXIST; QUITTING!!! ******* '
          EXIT loop over files
        end if        
        write(*,'(a)') ' f_td2: '//f_td2(1:nc_f_td2)
        write(nu_sum,'(a)') ' f_td2: '//f_td2(1:nc_f_td2)
        call get_lun(nu_td2)
        open(unit=nu_td2,file=f_td2(1:nc_f_td2),status='unknown')        
        call skip(nu_td2,1)
        n_td2 = 0
        loop over TD2 period: DO
          dum = 0.0
          read(nu_td2,*,iostat=status) (dum(i), i=1,14)
          if (status /= 0) then
            close(nu_td2)
            EXIT loop over TD2 period
          end if
          n_td2 = n_td2 + 1
          per_td2(n_td2) = dum(2)
          m_td2(n_td2)   = dum(4)
          r_td2(n_td2)   = dum(5)
          sd_td2(n_td2)  = dum(14)
        END DO loop over TD2 period
  
        f_rv1 = ' '
        read(nu_ctl,'(a)',iostat=status) f_rv1
        if(status /= 0) then
          write(*,*) ' Error reading r_rv1, QUITTING'
          EXIT loop over files
        end if
        call trim_c(f_rv1, nc_f_rv1)
        f_exist = .false.
        inquire(file=f_rv1(1:nc_f_rv1), exist=f_exist) 
        if (.not. f_exist) then
          write(*,'(a)') ' ****** FILE '//f_rv1(1:nc_f_rv1)// 
     :          ' DOES NOT EXIST; QUITTING!!! ******* '
          EXIT loop over files
        end if        
        write(*,'(a)') ' f_rv1: '//f_rv1(1:nc_f_rv1)
        write(nu_sum,'(a)') ' f_rv1: '//f_rv1(1:nc_f_rv1)
        call get_lun(nu_rv1)
        open(unit=nu_rv1,file=f_rv1(1:nc_f_rv1),status='unknown')
        call skip(nu_rv1,1)
        n_rv1 = 0
        loop over RV1 period: DO
          dum = 0.0
          read(nu_rv1,*,iostat=status) (dum(i), i=1,18)
          if (status /= 0) then
            close(nu_rv1)
            EXIT loop over RV1 period
          end if
          n_rv1 = n_rv1 + 1
          per_rv1(n_rv1)   = dum(2)
          m_rv1(n_rv1)     = dum(4)
          r_rv1(n_rv1)     = dum(5)
          sd_rv1(n_rv1)    = dum(10)
          durex_rv1(n_rv1) = dum(17)
          trms_rv1(n_rv1)  = dum(18)
        END DO loop over RV1 period

        f_rv2 = ' '
        read(nu_ctl,'(a)',iostat=status) f_rv2
        if(status /= 0) then
          write(*,*) ' Error reading r_rv2, QUITTING'
          EXIT loop over files
        end if
        call trim_c(f_rv2, nc_f_rv2)
        f_exist = .false.
        inquire(file=f_rv2(1:nc_f_rv2), exist=f_exist) 
        if (.not. f_exist) then
          write(*,'(a)') ' ****** FILE '//f_rv2(1:nc_f_rv2)// 
     :          ' DOES NOT EXIST; QUITTING!!! ******* '
          EXIT loop over files
        end if        
        write(*,'(a)') ' f_rv2: '//f_rv2(1:nc_f_rv2)
        write(nu_sum,'(a)') ' f_rv2: '//f_rv2(1:nc_f_rv2)
        call get_lun(nu_rv2)
        open(unit=nu_rv2,file=f_rv2(1:nc_f_rv2),status='unknown')
        call skip(nu_rv2,1)
        n_rv2 = 0
        loop over RV2 period: DO
          dum = 0.0
          read(nu_rv2,*,iostat=status) (dum(i), i=1,10)
          if (status /= 0) then
            close(nu_rv2)
            EXIT loop over RV2 period
          end if
          n_rv2 = n_rv2 + 1
          per_rv2(n_rv2) = dum(2)
          m_rv2(n_rv2)   = dum(4)
          r_rv2(n_rv2)   = dum(5)
          sd_rv2(n_rv2)  = dum(10)
        END DO loop over RV2 period

        if (n_td1 /= n_rv1 .or. n_td1 /= n_rv2 .or. n_rv1 /= n_rv2) then
          write(*,*) ' ERROR: n_td1, n_rv1, n_rv2 not equal; QUIT'
          EXIT loop over files
        end if

        if (n_td2 /= n_rv1 .or. n_td2 /= n_rv2 .or. n_rv1 /= n_rv2) then
          write(*,*) ' ERROR: n_td2, n_rv1, n_rv2 not equal; QUIT'
          EXIT loop over files
        end if

        DO i = 1, n_td1
          if ( per_td1(i) /= per_rv1(i) .or. per_td1(i) /= per_rv2 (i) 
     :                        .or. per_rv1(i) /= per_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  per_td1, per_rv1, per_rv2 not equal; QUIT'
            EXIT loop over files
          end if

          if ( m_td1(i) /= m_rv1(i) .or. m_td1(i) /= m_rv2 (i) 
     :                        .or. m_rv1(i) /= m_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  m_td1, m_rv1, m_rv2 not equal; QUIT'
            EXIT loop over files
          end if

          if ( r_td1(i) /= r_rv1(i) .or. r_td1(i) /= r_rv2 (i) 
     :                        .or. r_rv1(i) /= r_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  r_td1, r_rv1, r_rv2 not equal; QUIT'
            EXIT loop over files
          end if

          if ( per_td2(i) /= per_rv1(i) .or. per_td2(i) /= per_rv2 (i) 
     :                        .or. per_rv1(i) /= per_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  per_td2, per_rv1, per_rv2 not equal; QUIT'
            EXIT loop over files
          end if

          if ( m_td2(i) /= m_rv1(i) .or. m_td2(i) /= m_rv2 (i) 
     :                        .or. m_rv1(i) /= m_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  m_td2, m_rv1, m_rv2 not equal; QUIT'
            EXIT loop over files
          end if

          if ( r_td2(i) /= r_rv1(i) .or. r_td2(i) /= r_rv2 (i) 
     :                        .or. r_rv1(i) /= r_rv2(i)) then
            write(*,*) 
     :         ' ERROR:  r_td2, r_rv1, r_rv2 not equal; QUIT'
            EXIT loop over files
          end if

   
        if (irv_div_td == 0) then
          write(nu_out,'(
     :      1x,f7.3, 1x,f4.2, 1x,f6.2,
     :      1x,es12.5, 1x,es12.5, 
     :      1x,es12.5, 1x,es12.5,
     :      2x,f6.3, 2x,f6.3,
     :      2x,f6.3, 2x,f6.3,
     :      1x,es11.4, 1x,es11.4
     :                             )') 
     :      per_td1(i) , m_td1(i), r_td1(i), 
     :      sd_td1(i),  sd_td2(i),
     :      sd_rv1(i), sd_rv2(i), 
     :      sd_rv1(i)/sd_td1(i), sd_rv1(i)/sd_td2(i),
     :      sd_rv2(i)/sd_td1(i), sd_rv2(i)/sd_td2(i),
     :      per_td1(i)/durex_rv1(i), per_td1(i)/trms_rv1(i)
        else
          write(nu_out,'(
     :      1x,f7.3, 1x,f4.2, 1x,f6.2,
     :      1x,es12.5, 1x,es12.5, 
     :      1x,es12.5, 1x,es12.5,
     :      2x,f6.3, 2x,f6.3,
     :      2x,f6.3, 2x,f6.3,
     :      1x,es11.4, 1x,es11.4
     :                             )') 
     :      per_td1(i) , m_td1(i), r_td1(i), 
     :      sd_td1(i),  sd_td2(i),
     :      sd_rv1(i), sd_rv2(i), 
     :      sd_td1(i)/sd_rv1(i), sd_td1(i)/sd_rv2(i),
     :      sd_td2(i)/sd_rv1(i), sd_td2(i)/sd_rv2(i),
     :      per_td1(i)/durex_rv1(i), per_td1(i)/trms_rv1(i)
        end if
        
        END DO
     
     
        close(nu_out)
        
        

      END DO loop over files
      
      close(nu_ctl)
      close(nu_sum)
      
      END
      
      
      include '\forprogs\upstr.for'
      include '\forprogs\skipcmnt.for'
      include '\forprogs\trim_c.for'
      include '\forprogs\get_lun.for'
      include '\forprogs\skip.for'
      

      
      
      
      