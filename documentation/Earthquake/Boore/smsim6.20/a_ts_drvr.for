! --------------- BEGIN a_ts_drvr ---------------------------------
      Program a_ts_drvr

!  Driver to calculate and save a suite of acceleration time series.
!  This program obtains input parameters, calls the time domain simulation
!  program smsimacc.for, and saves each simulation in smc format.

! Sample control file:
!!Control file for program a_ts_drvr
!!Comment:
! Test case for program a_ts_drvr
!!Name of Summary File:
! a_tsdrvr.sum
!!Name of File with Input Parameters:
! ofr4.dat
!!Stem name for smc files.  
! m7r20_
!! M, R:
! 7.0 20.0
!!Total number of simulations (overrides value in input parameter file):
!!Number of first simulation to save and number of simulations to save:
! 12 2 10

! Notes regarding modification of driver:
!  1. All "include" statements assume all subprograms are in the same folder
!     as this program.
!  2. The logical variable "new_mr" must be explicitly set (= .true. for a
!     new value of magnitude and distance); its value is passed through the
!     block common /misc/ (see SMSIM.FI).
!  3. If values of stress other than those in the input parameter file are
!     to be used, set "stressc" equal to the new value, rather than "stress"
!     (and also set the other parameters if the stress changes with moment).
! Dates: 05/29/02 - Written by D. M. Boore
!        06/13/02 - Changed name from "acc_drvr" to "a_tsdrvr" to indicate that 
!                   acceleration time series are being computed (in contrast
!                   to "gmtddrvr" which computes various ground motion
!                   measures.
!        02/06/03 - removed new_mr from logical statement; it is 
!                   declared in smsim.fi
!        02/09/03 - Make sure that leading and trailing blanks are removed
!                   from the input file names (because LF95 allows file names
!                   with blanks).
!        08/15/05 - No longer any restriction on the length of the stem name,
!                   except restrict number of simulations to <1000
!        01/24/06 - Remove limitation on nsims
!        05/11/07 - Removed "\smsim\" from include statements
!        02/20/09 - ntaper was not being passed out of get_npts and into acc_ts.
!                   I caught this only when I compiled the program using the -chk
!                   switch.  This probably means that all of my time-domain runs
!                   up till now used ntaper = 0, even if a nonzero taper was 
!                   specified (the standard being 0.05).
!        02/26/09 - Updated calls to get date and time.
!        12/04/09 - Change subroutine name Get_Npts to Get_Npts_for_TD_Calcs
!        04/06/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!                 - Call rmod_calc to compute rmod (rmod = rmod_calc(r, m, iflag, c1, c2, f_ff) )
!        06/29/11 - Add name of params file to smc header
!        12/02/11 - Add computation of am0 right after specification of amag (previously, it
!                   was contained in spect_scale, but it was not clear from where that routine was
!                   called--it was in get_npts_for_td_calcs, called from the driver program. 
!        06/04/13 - Add fa, fb, dursource, durpath to headers in SMC file
!        01/06/14 - Increase length of filenames
!        02/07/14 - Add a sorted list of PGAs and mean PGA to summary file
!                   Change "nsims" to "nsims2save"
!                   Change index in output file name to correspond to the actual isim
!        10/13/14 - Allow for f_ff being defined by two lines
!        12/21/14 - Allow for f_ff being defined by a transition curve between two lines; this
!                   required a change to the calling arguments of rmod_calc

      character f_ctl*200, f_params*200, f_sum*200, stem*200
      character f_smc*200, cmmnt_hdrs(20)*80
      character message*80
      character date*8, time_start*10, time_stop*10
      logical tdflag, fparam_exist, f_ctl_exist

      character char_head(11)*80, comments(30)*80
      character precision*20
      integer int_head(48)
      real real_head(50)
      
      real pga(3000), rindex(3000)
 
      real acc(:)
      allocatable :: acc

      include 'smsim.fi'

! pi, twopi now computed in smsim.fi
!      pi = 4.0 * atan(1.0)
!      twopi = 2.0 * pi

      call banner(6)

      f_ctl_exist = .false.
      do while (.not. f_ctl_exist)
        f_ctl = ' '
        write(*, '(a\)') 
     :    ' Enter name of control file '//
     :    '(cr = a_ts_drvr.ctl; ctl_brk to quit): '
        read(*, '(a)') f_ctl
        if (f_ctl .eq. ' ') f_ctl = 'a_ts_drvr.ctl'
        inquire(file=f_ctl, exist=f_ctl_exist)
        if (.not. f_ctl_exist) then
          write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        end if
      end do

      call trim_c(f_ctl, nc_f_ctl)
      call get_lun(nu_ctl)
      open(unit=nu_ctl,file=f_ctl,status='unknown')

      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      message = ' '
      read(nu_ctl,'(a)') message

      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      f_sum = ' '
      read(nu_ctl, '(a)') f_sum
      call trim_c(f_sum, nc_f_sum)
      call get_lun(nu_sum)
      open(unit=nu_sum,file=f_sum(1:nc_f_sum),status='unknown')
      write(nu_sum,'(a)') ' Summary file for program a_ts_drvr'
      write(nu_sum,'(a)') '   Using control file: '//f_ctl
      write(nu_sum, '(a)') 
     :    ' *** Results computed using A_TS_DRVR ***'
      call banner(nu_sum)
      write(nu_sum, '(a)') message

! Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_start )
! Date is returned as 'CCYYMMDD' (character date*8) 
! Time is returned as 'hhmmss.sss' (character time*10)
!     character datx*8, time_start*10 
      write(nu_sum, *)
      write(nu_sum, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nu_sum, '(a)') 
     : ' Time Start: '//
     : time_start(1:2)//':'//time_start(3:4)//':'//time_start(5:10)
      write(nu_sum, *)

      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      f_params = ' '
      read(nu_ctl, '(a)') f_params
      call trim_c(f_params, nc_f_params)
      inquire(file=f_params(1:nc_f_params), exist=fparam_exist)
      if (.not. fparam_exist) then
        write(*,'(a)') ' ******* Parameter file '//
     :                 f_params(1:nc_f_params)//
     :                 ' does not exist; QUITTING!!! ******* '
        close(nu_ctl)
        stop        
      end if

      write(nu_sum, '(2a)') ' file with parameters: ',
     :   f_params(1:nc_f_params)

      tdflag = .true.  ! controls extent of the input file read and written
      call get_params( f_params(1:nc_f_params), tdflag )
      call write_params(nu_sum, tdflag)
      
      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      stem = ' '
      read(nu_ctl, '(a)') stem
      call trim_c(stem, nc_stem)
      write(nu_sum,'(a)') ' Stem = '//stem(1:nc_stem)

      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      read(nu_ctl,*) amag, r
      write(nu_sum, '(a)') ' amag    r ='
      write(nu_sum,'(2x,f4.2, 1x,f7.2)' ) amag, r
      write(*, '(a)') ' amag    r ='
      write(*,'(2x,f4.2, 1x,f7.2)' ) amag, r

      am0 = 10.**(1.5*amag + 16.05)

! replace h_eff with f_ff
      rmod = rmod_calc(r, amag, 
     :                  iflag_f_ff, nlines_f_ff,
     :                  c1_log10_f_ff, c2_log10_f_ff, mh_f_ff, 
     :                  c3_log10_f_ff, c4_log10_f_ff, 
     :                  m1t_f_ff, m2t_f_ff, 
     :                  c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff,
     :                                                          f_ff)       
         
      new_mr = .true.

      call skipcmnt(nu_ctl,cmmnt_hdrs, nc_hc)
      read(nu_ctl,*) nsims_total, nsims_start, nsims2save
      write(nu_sum, '(a)') ' nsims_total, nsims_start, nsims2save = '
      write(nu_sum,*) nsims_total, nsims_start, nsims2save
      
!      if (nsims2save .gt. 999) nsims2save = 999

      close(nu_ctl)

      call Get_Npts_for_TD_Calcs(nstart, nstop, npw2, te, ntaper)  ! included in td_subs
      write(nu_sum,'(a, 1x,i6, 1x,i6)') 
     :      ' nstart, nstop = ', nstart, nstop
      write(nu_sum,'(a, 1x,i6, 1x, f8.2)') ' npw2, te = ', npw2, te

      ndimen = npw2 
      allocate (acc(ndimen))

! Initialize seed:
      iseed = -abs(seed)

! Loop over simulations:      
     
      write(*,*)
      jsim = 0
      loop over simulations: do isim = 1, nsims_total
        write(*,'(3(a,i6),a)') 
     :    '+ Patience! Computing accelerogram # ', isim,
     :    ' of ', nsims_total

        call acc_ts(acc, nstart, nstop, npw2, te, ntaper)
        
        call mnmax(acc,1,npw2,1,amin,amax) 
        
        pga(isim) = max(abs(amin), abs(amax))
        rindex(isim) = real(isim)

!DEBUG
!        write(nu_sum,'(1x,a, 1x,i5, 1x,es10.3)')
!     :   ' index, pga = ', int(rindex(isim)), pga(isim)
!DEBUG

        if (isim .ge. nsims_start .and. jsim .lt. nsims2save) then

          jsim = jsim + 1
          

! Now write it:

          f_smc = ' '
          f_smc(1:nc_stem) = stem(1:nc_stem)
          write(f_smc(nc_stem+1:nc_stem+5),'(i5.5)') isim         
          
          
!          if (nc_stem .lt. 6) then
!            f_smc(1:nc_stem) = stem(1:nc_stem)
!            write(f_smc(nc_stem+1:nc_stem+3),'(i3.3)') jsim         
!          else if (nc_stem .eq. 6) then
!            f_smc(1:6) = stem(1:6)
!            write(f_smc(7:8),'(i2.2)') jsim         
!          else
!            f_smc(1:7) = stem(1:7)
!            write(f_smc(8:8),'(i1)') jsim         
!          end if

          call trim_c(f_smc,nc_f_smc)
          f_smc = f_smc(1:nc_f_smc)//'.smc'
          nc_f_smc = nc_f_smc + 4  

! set default values for int_head = -32768
!                    real_head = 1.7e+38

          do i = 1, 48
            int_head(i) = -32768
          end do

          do i = 1, 50
            real_head(i) = 1.7e+38
          end do

          int_head(17) = npw2
          real_head(2) = 1.0/dt
          write(nu_sum,'(a, 1x,i6, 1x,es12.5, 1x,es12.5)') 
     :         ' jsim, real_head(2), dt = ', jsim, real_head(2), dt


          ncomments = 0
          ncomments = ncomments + 1
          comments(ncomments) = '|'
          ncomments = ncomments + 1
          comments(ncomments) = '| File made using program a_ts_drvr'
          ncomments = ncomments + 1
          comments(ncomments) = 
     :      '|   on '//
     :           date(1:4)//'/'//date(5:6)//'/'//date(7:8)//
     :      ' at time '//
     :    time_start(1:2)//':'//time_start(3:4)//':'//time_start(5:10)
          ncomments = ncomments + 1
          write(comments(ncomments),'(a,1x,i6,a,1x,i6)') 
     :      '|   simulation ', jsim, ' of ', nsims
          ncomments = ncomments + 1
          write(comments(ncomments),'(a,1x,i6,a,1x,i6)') 
     :      '|   starting at simulation ', nsims_start
          ncomments = ncomments + 1
          comments(ncomments) = '|    using control file '//
     :                        f_ctl(1:nc_f_ctl)
          ncomments = ncomments + 1
          comments(ncomments) = '|    and params file '//
     :                        f_params(1:nc_f_params)
          ncomments = ncomments + 1
          write(comments(ncomments),'(a,1x,f4.2,a,1x,f7.2,1x,f7.2)') 
     :      '|   for M = ', amag, ' and R, Rmod = ', r, rmod
          ncomments = ncomments + 1
          write(comments(ncomments),'(a, 1x,es11.4, 1x,es11.4)') 
     :      '|   fa, fb = ', fa, fb 
          ncomments = ncomments + 1
          write(comments(ncomments),'(a, 1x,es11.4, 1x,es11.4)') 
     :      '|   and dursource, durpath = ', 
     :             dursource_calc, durpath_calc 
          ncomments = ncomments + 1
          write(comments(ncomments),'(a, 1x,es11.4)') 
     :      '|   and dursource + durpath = ', 
     :               dursource_calc + durpath_calc  
          ncomments = ncomments + 1
          comments(ncomments) = '|'

          int_head(16) = ncomments

          do i = 1, 11
            char_head(i) = '*'
          end do

          if (fcut .eq. 0.0) then
            itype = 1
          else
            itype = 2
          end if


          precision = 'standard'
          call SMCWrite(f_smc(1:nc_f_smc), precision, acc, 
     :                    char_head, itype, 
     :                    int_head, real_head, comments)

        end if

      end do  loop over simulations

      deallocate (acc)
      
      call sort2(nsims_total,pga,rindex)      
      write(nu_sum,*)
      write(nu_sum,'(1x,a, 7x,a)') 'index', 'pga'
      do i = 1, nsims_total
        write(nu_sum,'(1x,i5, 1x,es10.3)') int(rindex(i)), pga(i)
      end do
      write(nu_sum,*)

      itype = 1   
      call avg_std(pga,nsims_total,arith_mean,std,itype)
      itype = 2   
      call avg_std(pga,nsims_total,geom_mean,std,itype)
      write(nu_sum,'(1x,a, 1x,es10.3)') 
     :         ' arithmetic mean = ', arith_mean
      write(nu_sum,'(1x,a, 1x,es10.3)')
     :         ' geometric mean = ', geom_mean
      write(nu_sum,*)
      
      

      write(nu_sum, *)
      call DATE_AND_TIME( date, time_stop )
      write(nu_sum, '(a)') 
     : ' Time Stop: '//
     : time_stop(1:2)//':'//time_stop(3:4)//':'//time_stop(5:10)
      call time_diff(time_start, time_stop, time_elapsed)
      write(nu_sum, '(a,1x,1pe10.3)') 
     :     ' Elapsed time (sec): ', time_elapsed

      close(unit=nu_sum)

      stop
      end
! --------------- END a_ts_drvr ---------------------------------

!      include 'acc_ts.for'
!      include 'td_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'td_subs.for'
      include 'smsim_util_subs.for'
 
 