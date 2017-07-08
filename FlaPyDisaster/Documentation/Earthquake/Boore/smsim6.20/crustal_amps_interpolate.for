      Program crustal_amps_interpolate
      
!!Control file for crustal_amps_interpolate:
!!
!! Revision of program involving a change in the control file on this date:
!   02/10/15
!!
!!Files with site amps:
!! NOTE: the file must have a "!" at the beginning of any header lines, followed by a line containing
!! the number of amps.
!  crustal_amps_ab06_bc.txt
!!vel, dens used for input site amps
!  3.7 2.8
!!vel, dens for modified site amps
!  3.7 2.8
!!log-spaced frequencies (0) or individual frequencies (1)
! 0
!!if log-spaced frequencies: nfreqs, freqstart, freqstop:
!!if individual frequencies: nfreqs, list of nfreqs frequencies:
!  0.005 100 50
!!  7 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 100.0
!!Type of interpolation:
! 1 = linear f, linear a
! 2 = linear f, log    a
! 3 = log    f, linear a
! 4 = log    f, log    a
!
! 4
!!File with output:
!  crustal_amps_ab06_bc_interpolated.out
 

! Dates: 09/06/10 - Written by D. Boore
!        09/15/10 - Added computation of modified site amps
!                   for different source velocity and density
!        01/28/15 - Renamed from site_amp_interpolate, and minor 
!                   style changes
!                 - Add option for individual or log spaced frequencies
!        02/10/15 - Add option for type of interpolation
!        07/17/15 - Add number of input and output frequencies

      character  ctl_cmmnts(60)*100

      character date_ctl_correct*8, date_ctl_in*30
 
 
      character f_ctl*80, f_crustal_amps*200, f_out*200
      
      real famp(2000), amp(2000), freq_out(10000)
      
      integer interp_type
      
      logical f_ctl_exist, log_spaced_frequencies
      
      DO
        f_ctl = ' '
        write(*, '(a)') 
     :    ' Enter name of control file '//
     :    '(Enter = crustal_amps_interpolate.ctl; ctl-brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl(1:4) .eq. '    ') 
     :    f_ctl = 'crustal_amps_interpolate.ctl'
        call trim_c(f_ctl,nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_ctl_exist)
        IF (f_ctl_exist) EXIT
        write(*,'(a)') ' ******* CONTROL FILE '//f_ctl(1:nc_f_ctl)//
     :     ' DOES NOT EXIST ******* '
         
      END DO
      call get_lun(nu_ctl)
      open(unit=nu_ctl, file=f_ctl(1:nc_f_ctl), status='unknown')
      
!Check version of control file
      date_ctl_correct = ' '
      date_ctl_correct = '02/10/15'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)
      
      if (date_ctl_correct(1:nc_date_ctl_correct) /=
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong date'//
     :    ' (date_ctl_in(1:nc_date_ctl_in))'
        write(*,'(a)') 
     :     ' The correct date is'//
     :    ' (date_ctl_correct(1:nc_date_ctl_correct)); STOP!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_crustal_amps = ' '
      read(nu_ctl, '(a)') f_crustal_amps
      call trim_c(f_crustal_amps, nc_f_crustal_amps)
      
      write(*,*) ' Working on file '//
     :   f_crustal_amps(1:nc_f_crustal_amps)

      call get_lun(nu_amps)
      open(nu_amps,f_crustal_amps(1:nc_f_crustal_amps),status='unknown')
      call skipcmnt(nu_amps,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_amps,*) namps
      write(*,*) '  namps = ', namps
      do i = 1, namps
        read(nu_amps,*) famp(i), amp(i)
      end do
      close(nu_amps)

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) vel_in, dens_in
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) vel_out, dens_out
      
      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) ifreqflag

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      if (ifreqflag == 0) then
        log_spaced_frequencies = .true.
        read(nu_ctl,*) nfreqs, freq_start, freq_stop
        if (freq_start <= 0.0) then
          write(*,*) ' freq_start <= 0.0, which is not allowed'//
     :      ' for log-spaced frequencies. QUITTING'
          close(nu_ctl)
          STOP
        end if        
        if (nfreqs == 1) then
          dlogFreq = 0.0
        else
          dlogFreq = alog10(freq_stop/freq_start)/real(nfreqs-1)
        end if
        do ifreq=1,nfreqs
          freq_out(ifreq) = freq_start*10**(real(ifreq-1)*dlogFreq)
        end do
      else
        log_spaced_frequencies = .false.
        read(nu_ctl,*) nfreqs, (freq_out(ifreq), ifreq = 1, nfreqs)
      end if        

      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      read(nu_ctl,*) interp_type


      call skipcmnt(nu_ctl,ctl_cmmnts, nc_ctl_cmmnts)
      f_out = ' '
      read(nu_ctl, '(a)') f_out
      close(nu_ctl)
      call trim_c(f_out, nc_f_out)
      call get_lun(nu_out)
      open(nu_out,f_out(1:nc_f_out),status='unknown')
      
      write(nu_out,'(a)') 
     :    '! Interpolating crustal amps file: '//
     :    f_crustal_amps(1:nc_f_crustal_amps)
       
      write(nu_out,'(a, 1x,f4.2, 1x,f4.2)') 
     :    '! vel, dens for input amps = ', vel_in, dens_in
      
      write(nu_out,'(a, 1x,f4.2, 1x,f4.2)') 
     :    '! vel, dens for modified amps = ', vel_out, dens_out
      
      SELECT CASE (interp_type)
        CASE(1)
          write(nu_out,'(a, 1x,i1, a)') 
     :    '!  interp_type = ', interp_type, '; linear f, linear A'
        CASE(2)
          write(nu_out,'(a, 1x,i1, a)') 
     :    '!  interp_type = ', interp_type, '; linear f, log A'
        CASE(3)
          write(nu_out,'(a, 1x,i1, a)') 
     :    '!  interp_type = ', interp_type, '; log f, linear A'
        CASE(4)
          write(nu_out,'(a, 1x,i1, a)') 
     :    '!  interp_type = ', interp_type, '; log f, log A'
        CASE DEFAULT
          write(nu_out,'(a, 1x,i1, a)') 
     :    '!  interp_type =', interp_type, '; not valid--QUIT'
          stop
      END SELECT
      
      write(nu_out,'(a, 1x,i4, 1x,i4)') 
     :   '! namps_in, namps_out = ', namps, nfreqs 
     
      write(nu_out,'(11x,a, 8x,a, 9x,a, 1x,a)') 
     :   'f', 'logf', 'amp', 'amp-modified'
      

      do i=1, nfreqs
        f = freq_out(i)
        crustal_amp_in = crustal_amp_factor(f, namps, famp, amp,
     :                                      interp_type)
        write(nu_out,'(1x,es11.4, 1x,es11.4, 1x,es11.4,
     :                 2x,es11.4)') 
     :     f, alog10(f), crustal_amp_in,
     :  sqrt((vel_out*dens_out)/(vel_in*dens_in))*crustal_amp_in
      end do
      
      close(nu_out)
      
      end




!----------------- BEGIN CRUSTAL_AMP_FACTOR -----------------------------
      function crustal_amp_factor(f, namps, famp, amp, interp_type)

! Dates: 06/07/95 - Written by D.M. Boore
!        02/10/15 - Add option to select type of interpolation
!                   Type of interpolation:
!                     inter_type = 1: linear f, linear a
!                     inter_type = 2: linear f, log    a
!                     inter_type = 3: log    f, linear a
!                     inter_type = 4: log    f, log    a


      real famp(*), amp(*), crustal_amp_factor, slope
      
      if ( f .le. famp(1) ) then
        crustal_amp_factor = amp(1)
      else if ( f .ge. famp(namps) ) then
        crustal_amp_factor = amp(namps)
      else
        call locate( famp, namps, f, j)
        SELECT CASE (interp_type)
          CASE(1) ! linear f, linear A
            slope = (amp(j+1)-amp(j))/(famp(j+1)-famp(j))
            crustal_amp_factor = amp(j) + slope * (f - famp(j))
          CASE(2) ! linear f, log A
            slope = alog10(amp(j+1)/amp(j))/(famp(j+1)-famp(j))
            crustal_amp_factor = amp(j)*10.0**(slope*(f - famp(j)))
          CASE(3) ! log f, linear A
            slope = (amp(j+1)-amp(j))/alog10(famp(j+1)/famp(j))
            crustal_amp_factor = amp(j) + slope * alog10(f/famp(j))
          CASE(4) ! log f, log A
            slope = alog10(amp(j+1)/amp(j))/alog10(famp(j+1)/famp(j))
            crustal_amp_factor = amp(j)*(f/famp(j))**slope
        END SELECT
      end if

      return
      end
!----------------- END CRUSTAL_AMP_FACTOR -----------------------------

      include '\forprogs\get_lun.for'
      include '\forprogs\trim_c.for'
      include '\forprogs\skipcmnt.for'
      include '\forprogs\locate.for'
      
