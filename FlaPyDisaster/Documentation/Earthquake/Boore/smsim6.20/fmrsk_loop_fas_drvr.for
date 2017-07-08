! ----------------- BEGIN FMRSK_Loop_FAS_Drvr ---------------------------
      Program FMRSK_Loop_FAS_Drvr


!  Obtains parameters (including a list of oscillator periods,
!  magnitudes, and distances) from a file.  The only thing obtained
!  from interactively from the screen is the name of the control file.

!  The control file contains up to 60 comment lines, each line preceeded 
!  by "!", with a line of input parameters following each group of 
!  comment lines.  The processing stops after "Stop" is encountered in a line
!  (the character string "stop" can be any combination of uppercase and
!  lowercase letters).  Here is a sample of the control file:

!! Control file for program fmrsk_loop_fas_drvr
!! Revision of program involving a change in the control file on this date:
!   01/13/16
!!Name of Summary File:
! fmrsk_loop_fas_drvr.sum
!!
!!Comment:
!! (This is written to the summary file,
!! but not the output column file containing
!! the results)
! Test case for Open-File report
!!Name of file with SMSIM input parameters:
!! ***WARNING*** If want the stress parameter to be used,
!! be sure that the source specified
!! in the parameter file uses DeltaSigma as a parameter
!! (of the 11 sources currently built into the smsim
!! programs, only sources 1, 2, and 11 use the DeltaSigma
!! as a free parameter).
!  ofr.params
!!Name of Output Column File:
! fmrsk_loop_fas_drvr.col
!!Character Tag to Add to Column Labels (up to 4 characters):
! fmrs
!!log-spaced frequencies (0) or individual frequencies (1)?
! 1
!!if log-spaced frequencies: nfreq, freq_start, freq_stop:
!!if individual frequencies: nfreq, list of nfreq frequencies:
!! 200 0.01 100.0
! 5 0.01, 0.1, 0.2, 1.0, 2.0
!!linearly spaced magnitudes (0) or individual magnitudes (1)?
! 0
!!if individual magnitudes: nmag, list of nmag magnitudes:
!!if linearly spaced magnitudes: m_start, delta_m, m_stop
!! 3 4.0 6.0 7.0
! 4.0 4 8.0
!!kappa_0's (k0's) from params file (-1), log-spaced k0 (0) or individual k0 (1)
! 0
!!if from params file, still need a line as a placeholder (which will be skipped over):
!!if log-spaced k0: nk0, k0_start, k0_stop:
!!if individual k0: nk0, list of nk0 k0's:
!! Note: if specify k0, then dkappadmag, amagkref will be set to 0.0 (they are used in
!! kappa_f, called by dimin in rv_td_subs.for).  This means that k0 will not be magnitude dependent.
!! This is not a great loss, because I never used this option in the params file.  Note that the kappa 
!! obtained from the line before (except when the value is obtained from the params file)
!! is the same as "akappa" in smsim.fi. kappa = akappa + dkappadmag*(mag-amagkref in the kappa_f function 
!! in rv_td_subs.for. A word on notation: "kappa" should be "k0" for kappa_0, but I wrote my smsim programs before appreciating
!! the need for notation that distinguished kappa measured at some distance from the kappa extrapolated
!! to zero distance, which is what is used in the calculations.
!7 0.0025 0.16 
!! 7  0.0025 0.005 0.01 0.02 0.04 0.08 0.16
!!log-spaced distances (0) or individual distances (1)
! 1
!!if log-spaced distances: nr, r_start, r_stop:
!!if individual distances: nr, list of nr distances:
!! 200 1.0 400.0
! 3 10.0 40.0 80.0
!!
!!Parameters M1, h1, c1, c2, c3, M2, h2 for pseudodepth used to convert loop R values (assumed to be
!!  Rjb unless the pseudodepth = 0.0, as would be given by specifying h1=h2=c0=c1=c2=c3=0.0; or 
!!  by specifying M1=20.0 (or some other number greater than largest magnitude for which the motions are
!!  to be computed) and h1=0.0), in which case it is assumed that the loop R values are Rrup).
!!  The equation is
!!     M <= M1: h = h1
!!     M1< M < M2: h = h1+ c1*(M-M1) + c2*(M-M1)^2 + c3*(M-M1)^3
!!     M >= M2: h = h2
!!   Note that all the parameters are read in one read statement, so that they can be strung together on
!!   one line if desired.  I have separated them into three lines for clarity.
!!
!! Note: for a quadratic with zero slope at h1 and h2:
!! c1 = 0.0
!! c2 = -(3/2)*c3*(M2-M1)
!! c3 = -2*(h2-h1)/(M2-M1)^3
!!
!! For a line joining the values:
!! c1 = (h2-h1)/(M2-M1)
!!
!!No pseudodepth:
!! 3.75  0.0
!! 0.0 0.0 0.0
!! 7.50  0.0 
!!
!!quadratic joining h1 h2 smoothly (zero slope) (h1=8 km):
!! 3.75 8.0
!! 0.0 -1.706666667   0.303407407
!! 7.5 0.0
!!quadratic joining h1 h2 smoothly (zero slope) (h1 = 13 km):
! 3.75 13.0
! 0.0 -2.773333333   0.493037037
! 7.5 0.0
!!
!!stress from params file (-1), log-spaced stresses (0) or individual stress (1)
! 0
!!if log-spaced stresses: nstress, stress_start, stress_stop:
!!if individual stresses: nstress, list of nstress stresss:
!!NOTE: if choose stress from params file, then the input below is not used
!!  This allows using an M-dependent stress option, as specified by the parameters
!!  stressc, dlsdm, and amagc in the params field, for sources for which stress is a free
!!  parameter (such as sources 1, 2, 11, and 12).
! 4 1.0 1000.0

! Notes regarding modification of driver:
!  1. All "include" statements assume all subprograms are in the same folder
!     as this program.
!  2. If values of stress other than those in the input parameter file are
!     to be used, set "stressc" equal to the new value, rather than "stress"
!     (and also set the other parameters if the stress changes with moment).

! Dates: 12/15/15 - Written by D.M. Boore, based on fmrs_loop_fas_drvr and tmrsk_loop_rv_drvr (this version adds kappa_0 as
!                   one of the parameters over which looping is done).
!        01/13/16 - Improve specification of pseudodepth coefficients

      character  ctl_cmmnts(200)*100

      character date_ctl_correct*8, date_ctl_in*30
      
      character loop_order*10
 
      real amag, m_start, dm, m_stop
      
      real m1, h1, c1, c2, c3, m2, h2
      
      real h4rjb2rrup
      
      real r_in(1000), freq(1000), stress_in(1000)  ! cannot use "r" and "stress", because they are 
                                       ! passed through common as a scalar variable
      
      real r, r_start, r_stop      
      integer nr 
      
      real k0_in(1000), k0_start, k0_stop, dlogk0
      integer ik0flag, nk0
      
      real amag_in(500)
      
      real stress_start, stress_stop
      integer dstress 

      character f_ctl*200,
     :  f_params*200, f_sum*200, f_col*200, 
     :  tag4clabel*10, tag_out*4
      character f_params_caps*200, f_sum_caps*200, f_col_caps*200 
      character message*80
      character date*8, time_start*10, time_stop*10,
     :          time_begin*10, time_end*10
     
      character buf*200
      
      character wna_pars_file_name*60, ena_pars_file_name*60

      real smsim_out

      logical tdflag, file_exist, 
     :        log_spaced_frequencies, log_spaced_distances, 
     :        log_spaced_stresses,
     :        use_stress_from_params_file,
     :        use_k0_from_params_file, log_spaced_k0
     
      
      include 'bt12osc.fi'

      include 'smsim.fi'

      pi = 4.0 * atan(1.0)
      twopi = 2.0 * pi

      call banner(6)

      file_exist = .false.

      DO
        f_ctl = ' '
        write(*, '(a)') 
     :    ' Enter name of control file '//
     :    '("Enter" = fmrsk_loop_fas_drvr.ctl; ctl-brk to quit): '
        read(*, '(a)') f_ctl
        call trim_c(f_ctl, nc_f_ctl)
        if (f_ctl(1:4) .eq. '    ') 
     :        f_ctl = 'fmrsk_loop_fas_drvr.ctl'
        call trim_c(f_ctl, nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=file_exist)
        IF (file_exist) EXIT
        write(*,'(a)') ' ******* FILE '//f_ctl(1:nc_f_ctl)//
     :                 ' DOES NOT EXIST, '//
     :                 'ENTER A VALID FILE NAME ******* '
      END DO
      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl(1:nc_f_ctl),
     :     status='unknown')

!Check version of control file
      date_ctl_correct = ' '
      date_ctl_correct = '01/13/16'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)
      
      if (date_ctl_correct(1:nc_date_ctl_correct) /=
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong date ('//
     :       date_ctl_in(1:nc_date_ctl_in)//'); '//
     :      'the proper date is '//date_ctl_correct
        write(*,'(a)') 
     :     ' STOP!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_sum = ' '
      read(nu_ctl, '(a)') f_sum
      call trim_c(f_sum, nc_f_sum)
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      message = ' '
      read(nu_ctl, '(a)') message
      call trim_c(message, nc_message)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_params = ' '
      read(nu_ctl, '(a)') f_params
      call trim_c(f_params, nc_f_params)
        file_exist = .false.
        inquire(file=f_params(1:nc_f_params), exist=file_exist)
        IF (.not. file_exist) then
          write(*,'(a)') ' ******* FILE '//f_params(1:nc_f_params)//
     :        ' DOES NOT EXIST, '//
     :                 'QUITTING ******* '
          stop
        END IF

      f_sum_caps = ' '
      f_sum_caps = f_sum(1:nc_f_sum)
      call trim_c(f_sum_caps, nc_f_sum_caps)
      call upstr(f_sum_caps(1:nc_f_sum_caps))
      f_params_caps = ' '
      f_params_caps = f_params(1:nc_f_params)
      call trim_c(f_params_caps, nc_f_params_caps)
      call upstr(f_params_caps(1:nc_f_params_caps))
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)

      if ( f_sum_caps(1:nc_f_sum_caps) == 
     :    f_params_caps(1:nc_f_params_caps) ) then
        f_sum = f_sum(1:nc_f_sum)//'.sum'
        call trim_c(f_sum, nc_f_sum)
        write(*,'(a)') ' Desired name of summary file is the same as '//
     :     'the params file; '
        write(*,'(a)') ' rename the summary file by appending ".sum" '//
     :     'to the params file name.'
        write(*,*)
      end if
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')

      call banner(nu_sum)
      write(nu_sum, '(2a)') ' summary file: ',
     :   f_sum(1:nc_f_sum)
      write(nu_sum, '(2a)') ' control file: ',
     :   f_ctl(1:nc_f_ctl)
      write(nu_sum, '(2a)') ' parameter file: ',
     :   f_params(1:nc_f_params)
      write(nu_sum, '(a)') message(1:nc_message)



      f_col = ' '
      read(nu_ctl, '(a)') f_col
      call trim_c(f_col, nc_f_col)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      tag4clabel = ' '
      read(nu_ctl, '(1x,a)') tag4clabel
      call trim_c(tag4clabel, nc_tag4clabel)
      f_col_caps = ' '
      f_col_caps = f_col(1:nc_f_col)
      call trim_c(f_col_caps, nc_f_col_caps)
      call upstr(f_col_caps(1:nc_f_col_caps))
      if ( f_col_caps(1:nc_f_col_caps) == 
     :     f_params_caps(1:nc_f_params_caps) ) then
        f_col = f_col(1:nc_f_col)//'.col'
        call trim_c(f_col, nc_f_col)
        write(*,'(a)') ' Desired name of column file is the same as '//
     :     'the params file; '
        write(*,'(a)') ' rename the column file by appending ".col" '//
     :     'to the params file name.'
        write(*,*)
      end if

      
      tag_out = ' '
      tag_out = '____'
      if (nc_tag4clabel .gt. 4) then
        nc_stop = 4
      else
        nc_stop = nc_tag4clabel
      end if
      
      tag_out(1+4-nc_stop:4) = tag4clabel(1:nc_stop)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) ifreqflag

 
      if (ifreqflag == 0) then
        log_spaced_frequencies = .true.
        call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
        read(nu_ctl,*) nfreq, freq_start, freq_stop
        if (freq_start <= 0.0) then
          write(*,*) ' freq_start <= 0.0, which is not allowed'//
     :      ' for log-spaced frequencies. QUITTING'
          close(nu_ctl)
          STOP
        end if        
        if (nfreq == 1) then
          dlogFreq = 0.0
        else
          dlogFreq = alog10(freq_stop/freq_start)/real(nfreq-1)
        end if
        do ifreq=1,nfreq
          freq(ifreq) = freq_start*10**(real(ifreq-1)*dlogFreq)
        end do
      else
        log_spaced_frequencies = .false.
        call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
        read(nu_ctl,*) nfreq, (freq(ifreq), ifreq = 1, nfreq)
      end if        
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) imagflag
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      if (imagflag ==1) then  ! individual magnitudes
        read(nu_ctl,*) nmag, (amag_in(i), i=1, nmag)
      else
        read(nu_ctl,*) m_start, dm, m_stop
        nmag = nint((m_stop - m_start)/dm) + 1
        do i=1, nmag
          amag_in(i) = m_start + real(i-1)*dm
        end do
      end if        

      write(nu_sum,'(a, 1x,i2, 30(1x,f5.2))') ' nmag, amag_in = ', 
     :                               nmag, (amag_in(i),i=1, nmag)
     
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) ik0flag
      write(nu_sum,*)  
      write(nu_sum,'(a,1x,i4)') ' ik0flag = ', ik0flag
      write(nu_sum,*)  

      write(nu_sum,*)
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)      
      if (ik0flag < 0) then
        use_k0_from_params_file = .true.
        nk0 = 1
        call skip(nu_ctl,1)
      else if (ik0flag == 0) then
        log_spaced_k0 = .true.
        read(nu_ctl,*) nk0, k0_start, k0_stop
        write(nu_sum,*)
        write(nu_sum,'(a)') 
     :     ' log_spaced_k0, nk0, k0_start, k0_stop:' 
        write(nu_sum,*) 
     :       log_spaced_k0, nk0, k0_start, k0_stop 
        write(nu_sum,*)
        if (k0_start <= 0.0) then
          write(*,*) ' k0_start <= 0.0, which is not allowed'//
     :               ' ; QUITTING'
          close(nu_ctl)
          STOP
        end if        
        use_k0_from_params_file = .false.
        if (nk0 == 1) then
          dlogk0 = 0.0
        else
          dlogk0 = alog10(k0_stop/k0_start)/real(nk0-1)
        end if
        do ik0=1,nk0
          k0_in(ik0) = k0_start*10**(real(ik0-1)*dlogk0)
        end do
      else
        log_spaced_k0 = .false.
        read(nu_ctl,*) 
     :     nk0, (k0_in(ik0), ik0 = 1, nk0)
        write(nu_sum,*)
        write(nu_sum,'(a)') 
     :       ' nk0, (k0_in(ik0), ik0 = 1, nk0):' 
        write(nu_sum,*) 
     :       nk0, (k0_in(ik0), ik0 = 1, nk0) 
        write(nu_sum,*)
        use_k0_from_params_file = .false.
      end if        

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) idistflag
      if (idistflag == 0) then
        log_spaced_distances = .true.
        call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
        read(nu_ctl,*) nr, r_start, r_stop
        if (r_start <= 0.0) then
          write(*,*) ' r_start = 0.0, which is not allowed; '//
     :               ' use a small nonzero number instead; QUITTING'
          close(nu_ctl)
          STOP
        end if        
        if (nr == 1) then
          dlogr = 0.0
        else
          dlogr = alog10(r_stop/r_start)/real(nr-1)
        end if
        do ir=1,nr
          r_in(ir) = r_start*10**(real(ir-1)*dlogr)
        end do
      else
        log_spaced_distances = .false.
        call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
        read(nu_ctl,*) nr, (r_in(ir), ir = 1, nr)
      end if        
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) m1, h1, c1, c2, c3, m2, h2
      write(nu_sum,*)  
      write(nu_sum,'(a, 7(1x,es10.3))') 
     :  ' m1, h1, c1, c2, c3, m2, h2 = ',
     :    m1, h1, c1, c2, c3, m2, h2
      write(nu_sum,*)  
      
 
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) istressflag
!DEBUG
      write(nu_sum,*)  
      write(nu_sum,'(a,1x,i4)') ' istressflag = ', istressflag
      write(nu_sum,*)  
!DEBUG

      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)      
      if (istressflag < 0) then
        use_stress_from_params_file = .true.
        nstress = 1
      else if (istressflag == 0) then        
        log_spaced_stresses = .true.
        read(nu_ctl,*) nstress, stress_start, stress_stop
        if (stress_start <= 0.0) then
          write(*,*) ' stress_start <= 0.0, which is not allowed'//
     :               ' ; QUITTING'
          close(nu_ctl)
          STOP
        end if        
        use_stress_from_params_file = .false.
      else
        log_spaced_stresses = .false.
        read(nu_ctl,*) 
     :     nstress, (stress_in(istress), istress = 1, nstress)
        use_stress_from_params_file = .false.
      end if        

      close(nu_ctl)

      write(nu_sum, '(a)') 
     :  ' *** Results computed using fmrs_loop_fas_drvr ***'

! Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_begin )
! Date is returned as 'CCYYMMDD' (character date*8) 
! Time is returned as 'hhmmss.sss' (character time*10)
!     character datx*8, time_start*10 
      write(nu_sum, *)
      write(nu_sum, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nu_sum, '(a)') 
     : ' Time Start: '//
     : time_begin(1:2)//':'//time_begin(3:4)//':'//time_begin(5:10)
      write(nu_sum, *)

       

      tdflag = .false.
      call get_params( f_params, tdflag )
      call write_params( nu_sum, tdflag)
      
      if (use_k0_from_params_file) then
        nk0 = 1
        k0_in(nk0) = akappa
        write(nu_sum,'(1x,a, 1x,i3, 1x,f6.4)') 
     :      ' Obtain k0 from params file; nk0, k0 = ', nk0, k0_in(nk0)
      end if

      write(nu_sum,'(a, 1x,l1)') ' use_stress_from_params_file = ', 
     :     use_stress_from_params_file
 
      if (.not. use_stress_from_params_file) then

        if (stressc == 0.0) then  ! This is a result of a source model that
                                ! does not use stress as a parameter
          nstress = 1
          stress_in(nstress) = 0.0
        else if (istressflag == 0) then
          if (nstress == 1) then
            dlogStress = 0.0
          else
            dlogStress = 
     :        alog10(stress_stop/stress_start)/real(nstress-1)
          end if
          do istress = 1, nstress
            stress_in(istress) = 
     :            stress_start*10**(real(istress-1)*dlogStress)
          end do
        else
          write(nu_sum,'(a)') ' stressc /= 0.0, istressflag /= 0, '//
     :        ' so stress_in has been entered from the control file.'
        end if
        write(nu_sum,*)
        write(nu_sum,'(3x,a, 4x,a)') 'i', 'stress_in'
        do istress = 1, nstress
          write(nu_sum,'(1x,i3, 1x,es12.5)') istress, stress_in(istress)
        end do
        write(nu_sum,*)
        
      end if
      
      call get_lun(nu_col)
      open(unit=nu_col,file=f_col(1:nc_f_col),status='unknown')
      write(nu_col,'(a)') '! Parameter file: '//f_params(1:nc_f_params)
      write(nu_col,'(a, 7(1x,es10.3))') 
     :  '! Parameters for Rjb2Rrup conversion: '//
     :                      'm1, h1, c1, c2, c3, m2, h2 = ', 
     :                       m1, h1, c1, c2, c3, m2, h2
      write(nu_col,'(
     :               6x,a, 6x,a,
     :      6x,a, 5x,a, 4x,a, 4x,a, 4x,a, 5x,a,
     :      5x,a,
     :               1x,a,
     :               7x,a,
     :               1x,a,
     :               1x,a
     :                                      )')
     :             'f:'//tag_out(1:4), 'T:'//tag_out(1:4), 
     :    'M', 'Rjb', 'h', 'Rrup', 'F_FF', 'Rps',
     :    'k0',
     :             'numsource',
     :             'S:'//tag_out(1:4),
     :             'fds(cgs):'//tag_out(1:4),
     :             'fas(cgs):'//tag_out(1:4)
  

        DO ik0 = 1, nk0        
          akappa = k0_in(ik0)
          dkappadmag = 0.0
          amagkref = 0.0
          DO istress = 1, nstress      
            if (.not. use_stress_from_params_file) then ! Otherwise use value from params file
              stressc = stress_in(istress)
              dlsdm = 0.0
            end if
            DO imag = 1, nmag      
              amag = amag_in(imag)
              am0 = 10.**(1.5*amag + 16.05)
              DO ir = 1, nr      
                  rjb = r_in(ir)
                  if (amag <= m1) then
                    h4rjb2rrup = h1
                  else if (amag >= m2) then
                    h4rjb2rrup = h2
                  else
                    h4rjb2rrup = h1 +
     :               c1*(amag-m1) + c2*(amag-m1)**2 + c3*(amag-m1)**3
                  end if
                  write(nu_sum, '(a)') 
     :             ' m1, h1, c1, c2, c3, m2, h2, h4rjb2rrup = '
                  write(nu_sum, *) 
     :               m1, h1, c1, c2, c3, m2, h2, h4rjb2rrup     
                  rrup = sqrt(rjb**2+h4rjb2rrup**2)
                  r = rrup
                  rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
                DO ifreq = 1, nfreq            
                  freqosc = freq(ifreq)
                  CALL Compute_Write_FAS(freq(ifreq), nu_col, nu_sum, 
     :                                        rjb, h4rjb2rrup, rrup)
                END DO  
              END DO  
            END DO              
          END DO 
        END DO

 
      write(nu_sum, *)
      call DATE_AND_TIME( date, time_end )
      call time_diff(time_begin, time_end, time_elapsed)
      write(nu_sum, '(a,1x,1pe10.3)') 
     :     ' Total elapsed time (sec): ', time_elapsed

      close(unit=nu_col)
      close(unit=nu_sum)

      stop
      end
! ----------------- END FMRS_LOOP_FAS_DRVR ---------------------------


! ----------------- BEGIN COMPUTE_WRITE_FAS ---------------------------
      SUBROUTINE Compute_Write_FAS(freq, nu_col, nu_sum, 
     :                             rjb, h4rjb2rrup, rrup)  

! Dates: 01/29/15 - Add period to output.

! Note: necessary variables are passed through the common blocks in smsim.fi

      real freq4fas(1), fas, fds(1)  ! need arrays because smsimfas assumes arrays
      integer nfreq4fas
      
      include 'bt12osc.fi'

      include 'smsim.fi'

      print *,' freq, amag, r, f_ff, rmod, stress, k0 = ', 
     :          freq, amag, r, f_ff, rmod, stressc, akappa	
     
      nfreq4fas = 1
      freq4fas(1) = freq
      
      call smsimfas(fds, freq4fas, nfreq4fas)
      
      fas = (twopi*freq)**2*fds(1)

      if (freq == -1.0) then
        per4output = -1.0
      else if (freq == 0.0) then
        per4output = 0.0
      else
        per4output = 1.0/freq
      end if
      
      
      write(nu_col,'(
     :  1x,es11.4, 1x,es11.4,
     :  1x,f6.3, 1x,f7.2, 1x,f4.1, 1x,f7.2, 1x,f7.2, 1x,f7.2, 
     :          1x,f6.4,
     :  8x,i2,
     :  1x,es12.5,  
     :  3x,es11.4,
     :  3x,es11.4
     :             )')
     :  freq, per4output,
     :  amag, rjb, h4rjb2rrup, rrup, f_ff, rmod,
     :          akappa,
     :  numsource,
     :  stressc,
     :        fds(1),
     :        fas 

  
      END
! ----------------- END COMPUTE_WRITE_FAS ---------------------------

!      include 'rv_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'smsim_subs_for_rv_programs.for'
      include 'smsim_util_subs.for'
