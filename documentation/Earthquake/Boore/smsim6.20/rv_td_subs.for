!----------------- BEGIN BANNER -----------------------------
      subroutine banner(nu)
      
! Dates: 05/09/07 - Use a batch program (make_util_subs_file.bat)
!                   to make a file of forprogs utility files,
!                   and use include to bring the subroutines into this
!                   set of subroutines.  I do this to make the routines
!                   self contained.  It would be better to 
!                   use separate include statements, pointing to the
!                   forprogs folder, but people using the program
!                   won't have this folder available. But in order
!                   make sure that the util programs are up to date, I need
!                   to run the batch file above periodically.   The best
!                   way to do that would probably be to include a call to 
!                   that file in the batch files I use for compiling
!                   the smsim programs.

! Write version number to unit nu

      version = 6.0

      write(nu,*)
      write(nu,'(a,f6.3,a)') 
     :   '   ***************** SMSIM, Version ', 
     :    version,
     :   ' ************************'
      write(nu,*)

      return
      end
!----------------- BEGIN BANNER -----------------------------

!----------------- BEGIN GET_PARAMS -----------------------------
      subroutine get_params(f_params, tdflag)

! Dates: 06/07/95 - Written by D.M. Boore
!        10/17/95 - Added tdflag to control if more data need be read for
!                   the time series processing; also added irmvdc as an input
!                   parameter
!        11/14/95 - Read eps_int from data file
!        11/15/95 - changed input of spectral parameters
!        11/16/95 - Read amp_cutoff from data file and determine fup
!        11/27/95 - changed input of spectral parameters again
!        12/06/95 - Get low-cut filter parameters before rv and td params
!        12/07/95 - Get taper with window parameters
!        12/19/95 - Added zup to input parameters
!        02/28/97 - allow kappa to be magnitude dependent
!                   ( kappa = akappa + dkappadmag*(M-amagkref) )
!                   Use amagkref so that akappa is a reasonable value.  
!                   One reason to do so is that in
!                   this version of the program I determine fcut using
!                   a magnitude-independent value for kappa (akappa), and
!                   if the equation for kappa is given with amagkref = 0.0
!                   the value for akappa could well be less than 0.0.
!        01/18/99 - Added osc_crrctn to allow specification of which
!                   correction for oscillator response to make (Boore and Joyner
!                   or Liu and Pezeshk).
!        01/21/99 - Skip over reading rv params if tdflag = .true.
!        02/10/99 - Remove computation of npts for td sims; change name 
!                   "tsimdur" to "dur_fctr"
!        02/13/99 - Use get_lun, changed "fname" to "f", etc.
!        08/03/99 - Added parameter (iran_type) to control type of random 
!                   number (iran_type = 0 for normal, uniform otherwise)
!        06/05/00 - Added velocity parameter c_q to Q parameters
!        06/08/00 - Added r_ref to gsprd parameters and move definition of 
!                   const to const_am0_gsprd
!        01/27/01 - Added new time series window parameters
!        01/27/02 - Allow geom. spreading to be magnitude dependent
!        08/08/02 - Disabled use of remove-mean-from-noise parameter (irmvdc)
!        05/09/07 - Use skipcmnt to read in parameters, allowing up to 60 comment 
!                   lines (in particular, I can list all built-in sources).
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - New parameters and renamed parameters as a result of source 11
!        12/01/09 - Include version date at beginning of params file.  This
!                   is included as a way of checking if the proper params file is being
!                   used, in case the input parameters were changed. 
!                 - Trap for fmax = 0.0, in which case do not use fmax in computing 
!                   the diminution function  
!                 - Redo computation of fup, depending on the values of fmax and kappa
!                 - Read nslope rather than norder for the low-cut filter
!        12/02/09 - Remove computation of fup here, as it is now done in a subroutine
!                   in rv_subs.
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/16/09 - Read coefficients relating
!                   log10(f_ff) to M (this is motivated by Atkinson and Silva,
!                   2000, but may be a way of capturing the effect of a
!                   finite-fault without computing R_eff).  Set iflag_f_ff=1
!                   if want to compute f_ff (it does not work to set
!                   the coefficients = 0, because f_ff = 1 will then be
!                   computed.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/05/11 - Params file changed a bit, allowing the finite flag to have a value of 2
!                   to indicate rmod = r + f_ff.  
!        04/05/11 - I replaced "h_eff" with "f_ff" (because the finite-fault factor is not 
!                   really a pseudo-depth).
!        04/08/11 - Trap for a source that does not use stress as a parameter, and 
!                   set stressc = 0.0 in that case.
!        05/16/11 - Write name of params file if it has the wrong date
!        09/16/11 - Change the error message produced if the wrong
!                   date is found.
!        09/17/11 - Read Boore and Thompson Trms coefficients from the params file
!        09/17/11 - Add read Boore and Thompson Trms coefficients for PGA and PGV
!                   from the params file
!        11/30/11 - Allow for a frequency-dependent geometrical spreading option
!                   (currently the Atkinson November 2011 spreading if option = 1 is
!                   chosen).
!        01/19/12 - Change params file so that allow for two frequency-dependent gsprds.
!        01/23/12 - Get params for the DMB gsprd(f) function
!        03/24/13 - Get params for the generalized additive two-corner source model
!        01/05/14 - Some of the text related to the geometrical spreading was changed, as a 
!                   result of the AB14 ENA atten paper.  The changes were first incorporated into the params
!                   files a few months ago, without changing the revision date of the params files, but I
!                   decide that it was better to change the date so that I am forced to change the text
!                   related to geometrical spreading.
!        07/23/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        07/28/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        10/08/14 - Add possibility of two lines for f_ff
!        12/18/14 - Add possibility of two lines plus a transition for f_ff.  
!                   and compute the coefficients of the transition function if used
!        12/11/15 - Add a high-cut filter

      character  ctl_cmmnts(200)*100
      character date_params_file_in*20, 
     :          date_params_file_correct*20
      character f_params*(*)
      logical tdflag, file_exist

      character wna_trms4osc_pars_file_name*200, 
     :          ena_trms4osc_pars_file_name*200
      common /trms4osc_files/wna_trms4osc_pars_file_name, 
     :                       ena_trms4osc_pars_file_name
     
      include 'bt12osc.fi'
      include 'smsim.fi'

                

      pi = 4.0 * atan(1.0)

      call trim_c(f_params, nc_f_params)
      call get_lun(nin)
      open(unit=nin, file=f_params(1:nc_f_params), status='unknown')
      
      date_params_file_correct = ' '
      date_params_file_correct = '12/11/15'
      call trim_c(date_params_file_correct,
     :   nc_date_params_file_correct)

! check date of parameters file:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      date_params_file_in = ' '
      read(nin,'(a)') date_params_file_in
      call trim_c(date_params_file_in,
     :   nc_date_params_file_in)
      
      if (date_params_file_correct(1:nc_date_params_file_correct)
     :    /= 
     :    date_params_file_in(1:nc_date_params_file_in)) then
        write(*,'(a)') 
     :    ' The program is stopping because you are using an '//
     :    'outdated *.params file.'
        write(*,'(a)') 
     :     ' The parameters file you specified ('//
     :       f_params(1:nc_f_params)//') has the date '//
     :        date_params_file_in(1:nc_date_params_file_in) 
        write(*,'(a)') '  The proper date is '//
     :   date_params_file_correct(1:nc_date_params_file_correct) 
        close(nin)
        stop
      end if      
      
      
! title:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, '(a)') title
 
! rho, beta, prtitn, fs:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) rho, beta, prtitn, rtp, fs

! source shape:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) numsource, pf_a, pd_a, pf_b, pd_b
      
! source scaling: (note: stress is now read in here; to write a driver
! that loops over stress, just assign the desired values of stress
! after calling get_params)
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) stressc, dlsdm, fbdfa, amagc,
     :             c1_fa, c2_fa, amagc4fa,
     :             c1_eps, c2_eps, amagc4eps
     
      if (numsource == 12) then
        if (c2_eps == 0.0) then
          mag4eps1 = -9.9
        else
          mag4eps1 = -c1_eps/c2_eps
        end if
      end if
 
      set stressc: SELECT CASE(numsource)
      CASE(3:10)
        stressc = 0.0
        dlsdm = 0.0
      END SELECT set stressc
      
        
!!
!!finite_fault factor specification:
!!  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0 if a finite-fault factor is not to be used)
!!
!!  Distance for point-source calculation
!!    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))
!!    If iflag_f_ff = 2: rps =  r + f_ff
!!   Use rps in the calculations (this variable is called rmod in the code; it should be changed to rps to
!!   reflect my current preferred terminology.  I do not have time to do this now).
!!  Specification of the finite-fault factor h:
!!    If nlines = 1
!!      log10(f_ff) = c1 + c2*amag  
!!    If nlines = 2
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh
!!      where Mh is determined by the intersection of the two lines
!!      (this is computed in the program)  
!!    If nlines = 3
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2
!!      log10(f_ff) given by a cubic in amag between the two lines (this
!!        produces a smooth transition over the magnitude range DeltaM
!!  *** NOTE: placeholders are needed for c3, c4, and DeltaM, even if not used.
!!
!!  Published finite-fault factors
!!    Author                      applicable_region meaning_of_r  iflag_f_ff nlines         c1      c2   c3    c4  
!!    Atkinson and Silva (2000)                 ACR        r_rup           1    1      -0.0500  0.1500  0.0   0.0
!!    Toro (2002)                               SCR        r_rup           2    1      -1.0506  0.2606  0.0   0.0
!!    Atkinson and Boore (2003)          subduction        r_rup           1    1      -2.1403  0.5070  0.0   0.0
!!    Yenier and Atkinson (2014)                ACR        r_rup           1    1      -1.7200  0.4300  0.0   0.0
!!    Yenier and Atkinson (2015)                ACR        r_rup           1    1      -0.4050  0.2350  0.0   0.0
!!    Yenier and Atkinson (2015),               SCR        r_rup           1    1      -0.5690  0.2350  0.0   0.0
!!    Boore               (2014)                ACR        r_rup           1    2      -1.7200  0.4300 -0.405 0.2350
!!  
!!  Suggested modification for stable continental regions
!!    Assuming that all of the above the above relations except Toro (2002) and Atkinson and Boore (2003)
!!    are for active crustal regions, and that h is proportional to fault radius, then -0.164 should be
!!    added to c1 (and c3 for Boore (2014) to adjust for the smaller fault size expected for stable continental region
!!    earthquakes (this adjustment factor uses radius ~ stress^-1/3, and a stress of 88 bars for ACR (from 
!!    my determination of what stress matches the Atkinson and Silva (2000) high-frequency spectral level--
!!    see What_SCF_stress_param_is_consistent_with_the_AS00_source_model.pdf in the daves notes page of
!!    www.daveboore.com) and 274 bars for SCR, from my inversion of 0.1 s and 0.2 s PSA values for 8 ENA
!!    earthquakes, using the Boatwright and Seekins (2011) attenuation model.  This determination is 
!!    part of ongoing work for the NGA-East project, and will appear in a PEER report in 2015.
!!   1    1      -0.0500  0.1500  0.0   0.0 0.0
!!   1    2      -1.7200  0.4300 -0.405 0.2350 0.0
!!   1    3      -1.7200  0.4300 -0.405 0.2350 1.0
!!   1    1      -1.7200  0.4300  0.0   0.0 0.0
!!   1    1      -0.4050  0.2350  0.0   0.0 0.0
!   0 0 0.0 0.0 0.0 0.0 0.0
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) iflag_f_ff, nlines_f_ff, 
     :                c1_log10_f_ff, c2_log10_f_ff,
     :                c3_log10_f_ff, c4_log10_f_ff,
     :                dm_f_ff
      if (iflag_f_ff == 0) then
        mh_f_ff = 0.0
      else
        if (nlines_f_ff == 1) then
          mh_f_ff = 99.9
        else
          mh_f_ff = (c3_log10_f_ff - c1_log10_f_ff)/
     :              (c2_log10_f_ff - c4_log10_f_ff)
        end if
      end if

      if (nlines_f_ff == 3) then
        m1t_f_ff = mh_f_ff - 0.5*dm_f_ff
        m2t_f_ff = mh_f_ff + 0.5*dm_f_ff
        c1mod = c1_log10_f_ff + c2_log10_f_ff*m1t_f_ff
        c3mod = c3_log10_f_ff + c4_log10_f_ff*m2t_f_ff
        delta_a = c3mod - c1mod
        delta_b = c4_log10_f_ff - c2_log10_f_ff
        
        c0t_f_ff = c1mod
        c1t_f_ff = c2_log10_f_ff
        c2t_f_ff = 2.0*delta_a/dm_f_ff**2.0 - 
     :                  0.5*(3.0*c2_log10_f_ff + c4_log10_f_ff)/dm_f_ff
        c3t_f_ff = 
     :         0.5*(delta_b - 2.0 *(delta_a/dm_f_ff - c2_log10_f_ff))/
     :                          dm_f_ff**2.0
!        c2t_f_ff = (delta_a - c2_log10_f_ff*dm_f_ff)/dm_f_ff**2.0 - 
!     :                        c3t_f_ff*dm_f_ff
!         c2t_f_ff = -0.0975
!         c3t_f_ff = -2.498E-16

!DEBUG
!        print *,' Inside get_params:'
!        print *,' delta_b, c1mod, c3mod, delta_a, '//
!     :          'c2_log10_f_ff, dm_f_ff = ',      
!     :       delta_b, c1mod, c3mod, delta_a, c2_log10_f_ff, dm_f_ff      
!        print *,'c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff=', 
!     :         c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff 
!DEBUG

      else
        m1t_f_ff = 0.0
        m2t_f_ff = 0.0
        c0t_f_ff = 0.0
        c1t_f_ff = 0.0
        c2t_f_ff = 0.0
        c3t_f_ff = 0.0
      end if        
        

! gsprd: 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) i_gsprd_option

      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) r1_dmb_gsprd, 
     :        pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :        ft1_dmb_gsprd, ft2_dmb_gsprd

      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) r_ref
      read(nin, *) nsprd_segs
      do i = 1, nsprd_segs
        read(nin, *) rlow(i), a_s(i), b_s(i), m_s(i)
      end do
 
! Q:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q

! source duration:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) w_fa, w_fb

!        06/01/11 - For source 1, set w_fb = 0.0 so that do not mistakenly compute a longer
!                   source duration than is desired (note that dursource = w_fa/fa + w_fb/fb, 
!                   so before this modification, using duration weights 1.0 1.0 would have
!                   given the wrong duration, since fb = fa in spect_scale).
!        03/25/13 - It is more logical to use weights of 0.5 and 0.5, even for source 1,
!                   since dursource is always = w_fa/fa + w_fb/fb (see function dursource), 
!                   and fb is set equal to 
!                   fa in spect_scale for source 1, and in addition, I now recommend that 
!                   weights of 0.5 and 0.5 be used for the other sources.

!      if (numsource == 1) then
!        w_fb = 0.0
!      end if
      

! path duration:              
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) nknots
      do i = 1, nknots
        read(nin, *) rdur(i), dur(i)
      end do
      read(nin, *) slast

! site amps:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) namps
      do i = 1, namps
        read(nin, *) famp(i), amp(i)
      end do

! site diminution:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fm, akappa, dkappadmag, amagkref

! low-cut filter parameters:
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) fcut, nslope_in
! Force nslope = 4, 8, 12, etc for acausal filters:
      if (nslope_in .lt. 4) then
        nslope = 4
        write(*,'(a)') ' !!! nslope < 4; resetting it to 4'
      else
        nslope = 4 * int(float(nslope_in)/4.0)
        if (nslope .ne. nslope_in) then
          write(*,'(a, 1x,i2)') 
     :    ' !!! nslope not 4, 8, 12, etc; resetting to ', nslope
        end if
      end if
      norder = nslope/2 ! for an acausal (2 passes of an norder Butterworth,
                        ! see Dave's notes titled: "dependence_of_fourier_amplitude_spectra_at_low_frequencies_on_zero_pads.pdf"
                        ! and "properties_of_butterworth_filters.pdf"

! high-cut filter parameters: itype_hcfilt (0=no high-cut filter [but the other parameters are 
!  needed as placeholders], 1=raised half cycle of cosine; 2= quarter cycle of a cosine), 
!  fhc1, fhc2 (low and high frequency limits of filter), eta_hcfilt (power of cosine)
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) itype_hcfilt, fhc1, fhc2, eta_hcfilt
      if (itype_hcfilt /= 0 .and. 
     :    itype_hcfilt /= 1 .and. 
     :    itype_hcfilt /= 2) then 
        write(*,'(a)') ' ERROR: itype_hcfilt /= 0, 1, or 2; QUITTING!!'
        stop
      end if

! parameters for rv integration (read even if tdflag == .T., but values are not used):
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      read(nin, *) zup, eps_int, amp_cutoff, osc_crrctn
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      wna_trms4osc_pars_file_name = ' '
      read(nin, '(a)') wna_trms4osc_pars_file_name
      call trim_c(wna_trms4osc_pars_file_name, 
     :         nc_wna_trms4osc_pars_file_name)
 
      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
      ena_trms4osc_pars_file_name = ' '
      read(nin, '(a)') ena_trms4osc_pars_file_name
      call trim_c(ena_trms4osc_pars_file_name, 
     :         nc_ena_trms4osc_pars_file_name)
 
! Open files and store coefficients if not TDFLAG:

      if ( .not. tdflag .and. 
     :    (osc_crrctn >=3 .and. osc_crrctn<=5) ) then
        file_exist = .false.
        inquire(file=
     :  wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name), 
     :        exist=file_exist)
        IF (.not. file_exist) then
          write(*,'(a)') 
     :    ' ******* WNA Trms4Osc Coefficient file '//
     : wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name)//
     :    ' DOES NOT EXIST, QUIT!!!!!!!'
          stop
        END IF

        call get_lun(nu_pars)
        open(nu_pars,file=
     :   wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name), 
     :   status = 'unknown')
                         
        read(nu_pars,*)
        read(nu_pars,*)
        read(nu_pars,*) nm4bt12wna, nr4bt12wna
        read(nu_pars,*)
        do i = 1, nr4bt12wna
          do j = 1, nm4bt12wna
          
            read (nu_pars,*) m4bt12wna(j), dum_r, 
     :         c1bt12wna(j,i), c2bt12wna(j,i), c3bt12wna(j,i), 
     :         c4bt12wna(j,i), c5bt12wna(j,i), c6bt12wna(j,i),
     :         c7bt12wna(j,i),
     :         td_rv_pga_wna(j,i), td_rv_pgv_wna(j,i)
     
            logr4bt12wna(i) = alog10(dum_r)
            
          end do
        end do
      
        close(nu_pars)        
        
!DEBUG
!        call get_lun(nu_pars)
!        open(nu_pars,file='debug_wna_drms.out',
!     :   status = 'unknown')
!                         
!        write(nu_pars,*)
!        write(nu_pars,*)
!        write(nu_pars,'(i2, 2x,i2)') nm4bt12wna, nr4bt12wna
!        write(nu_pars,*)
!        do i = 1, nr4bt12wna
!          do j = 1, nm4bt12wna
!          
!            write (nu_pars,'(1x,f3.1, 1x,f7.2, 9(1x,es11.4))') 
!     :         m4bt12wna(j), 10.0**logr4bt12wna(i), 
!     :         c1bt12wna(j,i), c2bt12wna(j,i), c3bt12wna(j,i), 
!     :         c4bt12wna(j,i), c5bt12wna(j,i), c6bt12wna(j,i),
!     :         c7bt12wna(j,i),
!     :         td_rv_pga_wna(j,i), td_rv_pgv_wna(j,i)
!            
!          end do
!        end do
!      
!        close(nu_pars)         
!DEBUG  

        file_exist = .false.
        inquire(file=
     :  ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name), 
     :        exist=file_exist)
        IF (.not. file_exist) then
          write(*,'(a)') 
     :    ' ******* ENA Trms4Osc Coefficient file '//
     : ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name)//
     :    ' DOES NOT EXIST, QUIT!!!!!!!'
          stop
        END IF
        
        call get_lun(nu_pars)
        open(nu_pars,file=
     :   ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name), 
     :   status = 'unknown')
                         
        read(nu_pars,*)
        read(nu_pars,*)
        read(nu_pars,*) nm4bt12ena, nr4bt12ena
        read(nu_pars,*)
        do i = 1, nr4bt12ena
          do j = 1, nm4bt12ena
          
            read (nu_pars,*) m4bt12ena(j), dum_r, 
     :         c1bt12ena(j,i), c2bt12ena(j,i), c3bt12ena(j,i), 
     :         c4bt12ena(j,i), c5bt12ena(j,i), c6bt12ena(j,i),
     :         c7bt12ena(j,i),
     :         td_rv_pga_ena(j,i), td_rv_pgv_ena(j,i)
     
            logr4bt12ena(i) = alog10(dum_r)

!DEBUG
!       print *,' Inside rv_td_subs, '//
!     :       'm4bt12ena(j), dum_r, logr4bt12ena(i) =' 
!       print *, 
!     :       m4bt12ena(j), dum_r, logr4bt12ena(i) 
!DEBUG
 
          end do
        end do
      
        close(nu_pars)
        
!DEBUG
!        call get_lun(nu_pars)
!        open(nu_pars,file='debug_ena_drms.out',
!     :   status = 'unknown')
!                         
!        write(nu_pars,*)
!        write(nu_pars,*)
!        write(nu_pars,'(i2, 2x,i2)') nm4bt12ena, nr4bt12ena
!        write(nu_pars,*)
!        do i = 1, nr4bt12ena
!          do j = 1, nm4bt12ena
!          
!            write (nu_pars,'(1x,f3.1, 1x,f7.2, 9(1x,es11.4))') 
!     :         m4bt12ena(j), 10.0**logr4bt12ena(i), 
!     :         c1bt12ena(j,i), c2bt12ena(j,i), c3bt12ena(j,i), 
!     :         c4bt12ena(j,i), c5bt12ena(j,i), c6bt12ena(j,i),
!     :         c7bt12ena(j,i),
!     :         td_rv_pga_ena(j,i), td_rv_pgv_ena(j,i)
!     
!          end do
!        end do
!      
!        close(nu_pars)         
!DEBUG  
        
      end if
 
 
      if (tdflag) then

! Read more if time domain method:

! window parameters:
        call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
        read(nin, *) indxwind, taper, eps_w, eta_w, f_tb2te, f_te_xtnd

! timing stuff:
        call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
        read(nin, *) dur_fctr, dt, tshift, seed, nsims, iran_type

! Flag controlling the removal of the dc from the random series:
!      call skipcmnt(nin,ctl_cmmnts, nc_ctl_cmmnts)
!      read(nin, *) irmvdc
!
!      irmvdc = 0            ! force this value, so mean will not be removed
!

      end if 
      
      close(unit=nin)

      return
      end
!----------------- END GET_PARAMS -----------------------------

!----------------- BEGIN WRITE_PARAMS -----------------------------
      subroutine write_params(nout, tdflag)

! Dates: 08/18/95 - Written by D. Boore
!        10/17/95 - Added tdflag to control if more data need be read for
!                   the time series processing; also added irmvdc as an input
!                   parameter
!        11/14/95 - Write eps_int
!        11/16/95 - Write amp_cutoff, fup
!        12/06/95 - Write low-cut filter params before rv and td params
!        12/07/95 - Write taper with window parameters
!        12/19/95 - Added zup to input parameters
!        01/18/99 - Added osc_crrctn to allow specification of which
!                   correction for oscillator response to make (Boore and Joyner
!                   or Liu and Pezeshk).
!        08/03/99 - Added parameter (iran_type) to control type of random 
!                   number (iran_type = 0 for normal, uniform otherwise)
!        06/05/00 - Added velocity parameter c_q to Q parameters
!        01/27/01 - Added new time series window parameters
!        01/27/02 - Allow geom. spreading to be magnitude dependent
!        02/06/03 _ Use formatted write statements
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - New parameters and renamed parameters as a result of source 11
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/16/09 - Write coefficients relating log10(f_ff) to M.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/05/11 - Params file changed a bit, allowing the finite flag to have a value of 2
!                   to indicate rmod = r + f_ff.  I also replace "h_eff" with "f_ff" (because
!                   the finite-fault factor is not really a pseudo-depth).
!        09/11/11 - Add Trms4osc coefficient file names to argument list, and write them
!        01/23/12 - Write params for the gsprd(f) options
!        03/24/13 - Add source 12 (generalized additive 2-corner source model)
!        03/24/13 - Write params for the generalized additive two-corner source model
!        07/23/14 - Increased length of character variables wna_trms4osc_pars_file_name and 
!                   ena_trms4osc_pars_file_name
!        12/18/14 - Write parameters for possible transition equation for f_ff
!        12/11/15 - Add a high-cut filter

                
      character wna_trms4osc_pars_file_name*200, 
     :          ena_trms4osc_pars_file_name*200
      common /trms4osc_files/wna_trms4osc_pars_file_name, 
     :                       ena_trms4osc_pars_file_name
     
      logical tdflag

      include 'smsim.fi'

      write( nout, '(a)') ' Title:'
      write( nout, '(4x,a)') title

      write( nout, '(a)') ' rho, beta, prtitn, rtp, fs:'
      write( nout, '(2x,f5.2, 1x,f5.2, 1x,f5.3, 1x,f4.2, 1x,f4.2)')   
     :       rho, beta, prtitn, rtp, fs

      write( nout, '(a/a/a/a/a/a)') 
     : ' spectral shape: source number, pf_a, pd_a, pf_b, pd_b',
     :     '(1=Single Corner;2=Joyner;'//
     :      '3=A93;...;8=Jena;9=AS00;10=A05;'//
     :     '11=Generalized multiplicative 2-corner; '//
     :     '12=Generalized additive 2-corner)',
     : ' pf, pd (1-corner spectrum = '//
     :      '1/(1+(f/fc)**pf)**pd; 0.0 otherwise)',
     : ' (usual model: pf=2.0,pd=1.0; Butterworth: pf=4.0,pd=0.5)',
     : ' (Note: power of high freq decay --> pf*pd)',
     : ' NOTE: see a params file for more complete description '//
     :      'of parameters'
      write( nout, '(2x,i2, 4(1x,f4.2))' ) 
     :   numsource, pf_a, pd_a,  pf_b, pd_b

      write( nout, '(a/a/a/a/a)') 
     : ' spectral scaling: stressc, dlsdm, fbdfa, amagc'// 
     :   ' c1_fa, c2_fa, amagc4fa,'//
     :   ' c1_eps, c2_eps, amagc4eps, mag4eps1',
     : ' (stress=stressc*10.0**(dlsdm*(amag-amagc))',
     : ' (fbdfa, amagc for Joyner model, usually 4.0, 7.0)',
     : ' (not used for srce 3, but placeholders still needed)',
     : ' NOTE: see ofr.params for more complete description '//
     :      'of parameters'
      write( nout, '(2x,f7.2, 1x,es10.3, 1x,f5.2, 1x,f4.2,
     :     2(1x,es10.3), 1x,f5.2,
     :     2(1x,es10.3), 1x,f5.2, f5.2
     :                                       )') 
     :     stressc, dlsdm, fbdfa, amagc,
     :     c1_fa, c2_fa, amagc4fa,
     :     c1_eps, c2_eps, amagc4eps, mag4eps1

      write( nout, '(
     :       a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a/a
     :                                    )') 
     : ' !',
     : ' !finite_fault factor specification:',
     : ' !  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0'//
     :                 ' if a finite-fault factor is not to be used)',
     : ' !',
     : ' !  Distance for point-source calculation',
     : ' !    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))',
     : ' !    If iflag_f_ff = 2: rps =  r + f_ff',
     : ' !   Use rps in the calculations (this variable is called '//
     :           'rmod in the code; it should be changed to rps to',
     : ' !   reflect my current preferred terminology.  '//
     :           'I do not have time to do this now).',
     : ' !  Specification of the finite-fault factor h:',
     : ' !    If nlines = 1',
     : ' !      log10(f_ff) = c1 + c2*amag',  
     : ' !    If nlines = 2',
     : ' !      log10(f_ff) = c1 + c2*amag  for amag<Mh',
     : ' !      log10(f_ff) = c3 + c4*amag  for amag>=Mh',
     : ' !      where Mh is determined by the intersection of '//
     :                         'the two lines',
     : ' !      (this is computed in the program)',  
     : ' !    If nlines = 3',
     : ' !      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2',
     : ' !      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2',
     : ' !      log10(f_ff) given by a cubic in amag between '//
     :                'the two lines (this',
     : ' !        produces a smooth transition over the '//
     :                'magnitude range DeltaM'


!      write( nout, '(a/a/a/a/a/a/a/a/a/a)') 
!     : ' !!iflag_f_ff, nlines, c1, c2, c3, c4 (0 0 0 0 0 0 if '//
!     :                                               'not used)',
!     : ' !  If iflag_f_ff = 1:',
!     : ' !    modified distance: rmod = sqrt(r^2 + f_ff^2))',
!     : ' !  If iflag_f_ff = 2:',
!     : ' !    modified distance: rmod =  r + f_ff',
!     : ' !  where log10(f_ff) = c1 + c2*amag for nlines=1, '//
!     :                                         'and for nlines=2',
!     : ' !  the two lines c1+c2*amag for amag<Mh and c3+c4*amag '//
!     :                                     'for amag>=Mh, where Mh is', 
!     : ' !  determined by the intersection of the two lines '//
!     :                           '(this is computed in the program)',  
!     : ' !  *** NOTE: placeholders are needed for c3 and c4, '//
!     :                           'even if not used.',
!     : ' !  Use rmod in the calculations'
!      
      write(nout,*) iflag_f_ff, nlines_f_ff,
     :                c1_log10_f_ff, c2_log10_f_ff, 
     :                c3_log10_f_ff, c4_log10_f_ff, dm_f_ff
      write(nout, '(4x,a, 3(1x,f5.2))') 
     :              ' mh_f_ff, m1t_f_ff, m2t_f_ff = ', 
     :                mh_f_ff, m1t_f_ff, m2t_f_ff
     
      write(nout, '(4x,a, 4(1x,es10.3))') 
     :      ' c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff = ', 
     :        c0t_f_ff, c1t_f_ff, c2t_f_ff, c3t_f_ff

      write( nout, '(1x,a, 1x,i1)') 'i_gsprd_option = ', i_gsprd_option
      write( nout, '(1x,a)') ' r1_dmb_gsprd,'//
     :        ' pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1,'// 
     :        ' ft1_dmb_gsprd, ft2_dmb_gsprd = '
      write( nout, '(1x,f6.2, 
     :               1x,f5.2, 1x,f5.2, 1x,f5.2, 
     :               1x,f5.2, 1x,f5.2)')  
     :          r1_dmb_gsprd, 
     :          pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1,
     :          ft1_dmb_gsprd, ft2_dmb_gsprd

      write( nout, '(2a)') 
     :  ' gsprd: r_ref, nsegs, (rlow(i), a_s, b_s, m_s(i))',
     :                     '  (Usually set r_ref = 1.0 km)'
      write( nout, '(2x,f6.2)' ) r_ref
      write( nout, '(2x,i3)' ) nsprd_segs
      do i = 1, nsprd_segs
        write( nout, '(2x,f7.2, 1x,es10.3, 1x,es10.3, 1x,f4.2)') 
     :    rlow(i), a_s(i), b_s(i), m_s(i)
      end do

      write( nout, '(a)') 
     :   ' q: fr1, Qr1, s1, ft1, ft2, fr2, qr2, s2, c_q'
      write( nout, '(2x,f7.3, 1x,f7.2, 1x,f6.3, 3(1x,f7.3),
     :     1x, f7.2, 1x, f6.3, 1x,f4.2)' ) 
     :       fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q

      write( nout, '(a)') ' source duration: weights of 1/fa, 1/fb'
      write( nout, '(2x,f4.2, 1x, f4.2)' ) w_fa, w_fb

      write( nout, '(2a)') ' path duration: nknots, (rdur(i), dur(i),',
     :                     ' slope of last segment'
      write( nout, '(2x,i3)' ) nknots
      do i = 1, nknots
        write( nout, '(2x,f6.1, 1x,f6.2)' ) rdur(i), dur(i)
      end do
      write( nout, '(2x,es10.3)' ) slast

      write( nout, '(2a)') ' site amplification: namps, (famp(i), ',
     :                     'amp(i))'
      write( nout, '(2x,i3)' ) namps
      do i = 1, namps
        write( nout, '(2x,f7.3, 1x,f6.3)' ) famp(i), amp(i)
      end do

      write( nout, '(a)') 
     :  ' site diminution parameters: fm, akappa, dkappadmag, amagkref'
      write( nout, '(2x,f7.3, 2(1x,es10.3), 1x,f4.2)' ) 
     :    fm, akappa, dkappadmag, amagkref

      write( nout, '(a)') ' low-cut filter parameters: fcut, nslope'
      write( nout, '(2x,f7.3, 1x,i2)' ) fcut, 2*norder

! high-cut filter parameters: itype_hcfilt (0=no high-cut filter (but the other parameters are 
!  needed as placeholders), 1=raised half cycle of cosine; 2= quarter cycle of a cosine), 
!  fhc1, fhc2 (low and high frequency limits of filter), eta_hcfilt (power of cosine)
      write( nout, '(a)') ' high-cut filter parameters: '
      write( nout, '(a)') '   itype_hcfilt'
      write( nout, '(a)') '     0=no high-cut filter (but the other'//
     :                         ' parameters are needed as placeholders)'
      write( nout, '(a)') '     1=raised half cycle of cosine'
      write( nout, '(a)') '     2= quarter cycle of a cosine' 
      write( nout, '(a)') '     fhc1, fhc2 (low and high frequency'
      write( nout, '(a)') '     eta_hcfilt (power of cosine)' 
      write( nout, *) itype_hcfilt, fhc1, fhc2, eta_hcfilt

      if (.not. tdflag) then
        write( nout, '(a)') 
     :  ' parameters for rv calcs: zup, eps_int, amp_cutoff, osc_crrctn'
        write( nout, '(2x,f6.2, 2(1x,es10.3), 1x, i2)' ) 
     :        zup, eps_int, amp_cutoff, osc_crrctn
        if (osc_crrctn >=3 .and. osc_crrctn<=5) then        
          call trim_c(wna_trms4osc_pars_file_name, 
     :        nc_wna_trms4osc_pars_file_name)
          write( nout, '(a)') ' WNA Trms4osc coefficients from file '//
     :    wna_trms4osc_pars_file_name(1:nc_wna_trms4osc_pars_file_name)
          call trim_c(ena_trms4osc_pars_file_name, 
     :        nc_ena_trms4osc_pars_file_name)
          write( nout, '(a)') ' ENA Trms4osc coefficients from file '//
     :    ena_trms4osc_pars_file_name(1:nc_ena_trms4osc_pars_file_name)
        end if
        return
      end if

! Write more if time domain method:

      write( nout, '(2a)') 
     :  ' window params: indxwind(0=box,1=exp), ',
     :  ' taper, eps_w, eta_w, f_tb2te, f_te_xtnd'
      write( nout, '(2x,i1, 1x,f4.2, 1x,f5.2, 1x,f6.3, 1x,f4.1, 
     :               1x, f4.1)' ) 
     :    indxwind, taper, eps_w, eta_w,f_tb2te,f_te_xtnd

      write( nout, '(a)') 
     :  ' timing stuff: dur_fctr, dt, tshift, seed, nsims, iran_type'
      write( nout, '(2x,f4.2, 1x,f6.4, 1x,f6.2, 1x,f6.1, 
     :               1x,i4, 1x,i1)' ) 
     :    dur_fctr, dt, tshift, seed, nsims, iran_type

!      write( nout, '(2a/2a)') 
!     :         ' parameter to control whether dc is',
!     :         ' removed from random series before',
!     :         ' transformation to frequency domain',
!     :         ' is no longer used'
!      write( nout, *) irmvdc

      return
      end
!----------------- END WRITE_PARAMS -----------------------------
  
!----------------- BEGIN SPECT_AMP -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/16/05 - Replace computed go to with if then else
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        03/20/09 - Add instrument type 3 (wwssn short-period)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        01/22/10 - Add more instrument types and changed code to make it 
!                   clearer where the displacement, velocity, or
!                   acceleration response is being computed.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        11/29/11 - Add possibility that gspread is frequency dependent, 
!                   as controlled by i_gsprd_option.
!        02/11/15 - Changed name of "site_amp_factor" to "crustal_amp_factor"
!                 - Added interp_type to argument list, and specified
!                   inter_type = 2 (lin f, log A), based on work done on 02/10/15 in
!                   folder C:\smsim\interpolation_log_or_linear, as described in 
!                   "How should tabulated values of crustal amps be interpolated.v01.docx" in
!                   that folder.
!        12/11/15 - Add a high-cut filter
 
      function spect_amp(f)
      
      real h, pgd_pgv_pga_factor, spect_amp_displacement, spect_amp
      complex sp(20)

      include 'smsim.fi'
      
! NOTE: freq_indep_factor comes from calls to subroutine const_am0_gsprd within programs such as
! gm_rv in rv_subs.for, acc_ts, and SMSIMFAS.  The value of freq_indep_factor is passed
! through the common block /const_params/ in smsim.fi.  

      interp_type = 2 ! lin f, log A
      spect_amp_displacement =   freq_indep_factor * 
     :      buttrlcf(f, fcut, norder) * 
     :      high_cut_filter(f) * 
     :      spect_shape(f, fa, fb, pf_a, pd_a,  pf_b, pd_b, 
     :      am0b_m0, numsource) * 
     :      crustal_amp_factor(f, namps, famp, amp, interp_type) *
     :      dimin(f)
                                 ! Could save some multiplications
                                 ! by removing freq_indep_factor outside the
                                 ! function, but at the cost of possible
                                 ! confusion.
                                 ! freq_indep_factor from subroutine const_am0_gsprd
                                 
! If i_gsprd_option /= 0, then a frequency-dependent geometrical spreading is implied, and
! this must be called from within this subroutine (spect_amp).
      if (i_gsprd_option /= 0) then
        spect_amp_displacement = 
     :    gsprd_freq(f, rmod, r_ref, i_gsprd_option,
     :      r1_dmb_gsprd, 
     :      pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :      ft1_dmb_gsprd, ft2_dmb_gsprd)
     :                                    * spect_amp_displacement
      end if
      
! NOTE: When I was first revising the program to include the possibility of a frequency-dependent
! gsprd, I modified function gsprd to return either the frequency independent or dependent functions,
! and I removed the call to gsprd from const_am0_gsprd.  The problem with this, however, is that if gsprd 
! is independent of frequency, as it usually is, it would be called unnecessarily nfreq times.

!DEBUG
!      write(*,*) ' In spect_amp: freq_indep_factor = ', 
!     :                           freq_indep_factor
!      write(*,*) ' In spect_amp: buttrlcf = ', 
!     :                           buttrlcf(f, fcut, norder)
!      write(*,*) ' In spect_amp: spect_shape = ', 
!     :     spect_shape(f, fa, fb, pf_a, pd_a,  pf_b, pd_b, 
!     :      am0b_m0, numsource)
!      write(*,*) ' In spect_amp: crustal_amp_factor  = ', 
!     :     crustal_amp_factor(f, namps, famp, amp)
!      write(*,*) ' In spect_amp: dimin(f)  = ', 
!     :     dimin(f)
!DEBUG
                                 
      IF (iaorins == 1) THEN
      
        h=1                                      ! no instrument response
        
      ELSE IF (iaorins == 2) THEN              ! psv
                                                 !   converts from displacement to velocity response
        v = twopi * fosc                         
        h = v * harmoscf( f, fosc, damp, idva )
        
      ELSE IF (iaorins == 3) THEN   ! Wood-Anderson from poles and zeros
! gain from Uhrhammer, R.A. and E.R. Collins (1990). Synthesis of Wood-
! Anderson seismograms from broadband digital records, \bssa {\bf 80},
! 702--716. (V = 2080$, T_0 = 0.8 (f_0 = 1/0.8 = 1.25), and \eta = 0.69).
!
! poles and zeros from above values of f_o and eta, using these relations:
!       2*pi*f_0 = sqrt(sp(1)**2 + sp(2)**2)
!       eta = abs(sp(1))/(2*pi*f_0)

        idva = 0
      
        gain = 2080.0
        
        if (idva == 2) then
          fgain = 0.01 
        else if (idva == 0) then
          fgain = 100.0
        else
          write(*,*) ' Invalid value of idva in spect_amp, QUITTING!'
          stop
        end if
        
        nz = 2
        np = 2
        sp(1) = cmplx( -5.41925, -5.68479)  
        sp(2) = cmplx( -5.41925, +5.68479)  
!        sp(1) = cmplx( -5.49779, -5.60886) ! standard_tab.doc (provided by
!        sp(2) = cmplx( -5.49779, +5.60886) ! Jim Dewey) containing notes from 
                                            ! Charles R. Hutt, 1 December 2006
! Determine factor to give proper gain 
        dum = 1.0
        call poles_zeros_response(fgain, dum, nz, np, sp, idva, 
     :                              ampnorm, phase)
        gainfactor = gain/ampnorm

        call poles_zeros_response(f, gainfactor, nz, np, sp, idva, 
     :                              amp_response, phase_response)
        h = amp_response

!DEBUG
!        if (abs(f/1.25-1) < 0.01) then
!          write(*,*) ' For WA: f, h = ', f, h
!        end if
!DEBUG
        
      ELSE IF (iaorins == 4) THEN   ! WWSSN-SP from poles and zeros
! values from standard_tab.doc (provided by
! Jim Dewey) containing notes from 
! Charles R. Hutt, 1 December 2006

        idva = 0
      
        gain = 1.0
        fgain = 1.0
        nz = 3
        np = 5
        sp(1) = cmplx( -3.72500, -6.22000)
        sp(2) = cmplx( -3.72500, +6.22000)
        sp(3) = cmplx( -5.61200, +0.00000)
        sp(4) = cmplx(-13.24000, +0.00000)
        sp(5) = cmplx(-21.08000, +0.00000)
                                             
! Determine factor to give proper gain 
        dum = 1.0
        call poles_zeros_response(fgain, dum, nz, np, sp, idva, 
     :                              ampnorm, phase)
        gainfactor = gain/ampnorm

        call poles_zeros_response(f, gainfactor, nz, np, sp, idva, 
     :                              amp_response, phase_response)
        h = amp_response
        

!DEBUG
!        if (abs(f/1.0-1) < 0.01) then
!          write(*,*) ' For WWSSN-SP, P&Z: f, h = ', f, h
!        end if
!DEBUG
        
!      ELSE IF (iaorins == 5) THEN   ! WWSSN short period
!        pp = 1.0
!        pg = 0.75
!        s = 0.0
!        hp = 0.49
!        hg = 1.00
!        g_spinsf = 17.1
!        h= g_spinsf * spinsf (f,pp,pg,s,hp,hg)
!
!*DEBUG
!        if (abs(f/1.0-1) < 0.01) then
!          write(*,*) ' For WWSSN-SP, Spinsf: f, h = ', f, h
!        end if
!*DEBUG
        
      ELSE
! for customized response (network instruments, etc)
        h=1
      END IF

      pgd_pgv_pga_factor = (twopi*f)**float(idva)
       
      spect_amp = h * pgd_pgv_pga_factor * spect_amp_displacement   ! spect_amp contains the spectral amplitude.

      return
      end
!----------------- END SPECT_AMP -----------------------------


!----------------- BEGIN CONST_AM0_GSPRD -----------------------------
! Dates: 11/14/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments in gsprd
!        06/08/00 - Moved computation of const into this subroutine
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/17/09 - Bring in rmod as a calling argument
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        04/08/11 - I removed numsource from the argument list of gsprd because the
!                   modification for the AS00 source referred to in the 
!                   07/02/99 modification is no longer used, as rmod is 
!                   calculated elsewhere and passed to gsprd through a common
!                   block in smsim.fi.
!        11/30/11 - Allow for the option of a frequency dependent geometrical spreading
!                   (i_gsprd_option /= 0).
!        10/28/14 - Changed call to the function gsprd to the subroutine gsprd_sub in order to
!                   obtain slope, gsprd in smsim.fi, so can print the values if desired (as in
!                   tmrs_loop_rv_drvr) in order to check the values (in particular, the M-dependent
!                   gsprd of Silva used in the NGA-East simulations for the 29-30 Oct. 2014 workshop).

      subroutine const_am0_gsprd()
      
! Note: This subroutine is called from SMSIMFAS      

      include 'smsim.fi'

! Define constant, for r=r_ref(km).Usually set r_ref = 1.0 km.  Be careful
! if a different value or different units are used.  In particular, using 
! different units will require a change in the scaling factor of 1.e-20 below

      const=prtitn*rtp*fs*(1.e-20)/(4.*pi*rho*beta**3*r_ref)

      if (i_gsprd_option == 0) then
        call gsprd_sub(rmod, r_ref, nsprd_segs, rlow, a_s, b_s, m_s, 
     :        amag, slope, gsprd)
      
        freq_indep_factor = const*am0*gsprd
      else
        gsprd = 1.0
        slope = 0.0
        freq_indep_factor = const*am0  ! Note: gsprd_freq is called in spect_amp
                                       ! if i_gsprd_option /= 0.  Separating 
                                       ! freq_indep_factor from gspread in this case
                                       ! avoids multiple calls (for each frequency) to the 
                                       ! frequency-independent computations in this
                                       ! subroutine.
      end if
      
!                         (am0 from Spect_Scale) 

!DEBUG
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: rmod, r_ref, nsprd_segs'
!      write(*,*)  rmod, r_ref, nsprd_segs
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: numsource, amag = ' 
!      write(*,*)  numsource, amag 
!      write(*,*)
!      do i = 1, nsprd_segs
!        write(*,'(4(1x,es10.3))') rlow(i), a_s(i), b_s(i), m_s(i)
!      end do        
!
!       temp_gsprd = gsprd(rmod, r_ref, nsprd_segs, rlow, a_s, b_s, m_s, 
!     :        numsource, amag)
!      write(*,*)
!      write(*,*) ' In const_am0_gsrpd: const, am0, gsprd = ', 
!     :             const, am0, temp_gsprd 
!      write(*,*)
!DEBUG

      return
      end
!----------------- END CONST_AM0_GSPRD -----------------------------

!----------------- BEGIN GSPRD_FUNC -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments
!        06/05/00 - Added some explanation of r
!        06/08/00 - Make gsprd nondimensional through the use of r_ref, which 
!                   now appears in the definition of variable const
!                   in const_am0_gsprd
!        01/27/02 - Following Silva, parameters added to allow magnitude
!                   dependent slope (to capture finite fault effects)
!        11/27/05 - Remove deff for Atkinson (2005) source
!        04/24/07 - Put "rmod = r" in the if statement
!        12/17/09 - Bring in rmod as a calling argument.  numsource is no longer
!                   needed as a calling argument.
!        04/08/11 - I removed numsource from the argument list of gsprd because the
!                   modification for the AS00 source referred to in the 
!                   07/02/99 modification is no longer used, as rmod is 
!                   calculated elsewhere and passed to gsprd through a common
!                   block in smsim.fi.
!        10/28/14 - Changed name to gsprd_func (becauase I will also create a new routine
!                   gsprd_sub based on this one that returns slope and gsprd--see change log
!                   in CONST_AM0_GSPRD

      function gsprd_func(rmod, r_ref, nsprd_segs,  rlow, a_s, b_s, m_s, 
     :               amag)
      real r_ref, rlow(*), a_s(*), b_s(*), m_s(*), geff(10)
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      
!      if (numsource .eq. 9) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if
      
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
      if (rmod <= rlow(1)) then
        j = 1
      else if (rmod >= rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd_func = (geff(j)) * (rmod/rlow(j))**slope

      return
      end
!----------------- END GSPRD_FUNC -----------------------------

!----------------- BEGIN GSPRD_SUB -----------------------------
! Dates: 10/28/14 - Written by D.M. Boore, based on gsprd_func
      subroutine gsprd_sub(rmod, r_ref, nsprd_segs,  rlow, 
     :               a_s, b_s, m_s, 
     :               amag, slope, gsprd)
      real r_ref, rlow(*), a_s(*), b_s(*), m_s(*), geff(10)
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      
!      if (numsource .eq. 9) then ! Atkinson and Silva (2000)                                                         
!        deff = 10.0**(-0.05 + 0.15 * amag)
!        rmod = sqrt(r**2 + deff**2)        
!      else      
!        rmod = r      
!      end if
      
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
      if (rmod <= rlow(1)) then
        j = 1
      else if (rmod >= rlow(nsprd_segs)) then
        j = nsprd_segs
      else
        call locate(rlow, nsprd_segs, rmod, j)
      end if
      slope = a_s(j) + b_s(j)*(amag - m_s(j))

      gsprd = (geff(j)) * (rmod/rlow(j))**slope

      return
      end
!----------------- END GSPRD_SUB -----------------------------

!----------------- BEGIN GSPRD_FREQ -----------------------------
      FUNCTION gsprd_freq(f, r, r_ref, i_gsprd_option,
     :    r1, 
     :    pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :    ft1, ft2)
      
! NOTE: r_ref not used in this version, but it is included as an argument
! in case it is used in a later version.
      
! Dates: 11/30/11 - Written by D.M. Boore, based on gsprd, and implementing
!                   Gail Atkinson's November 2011 frequency-dependent
!                   geometrical spreading for ENA.  This subroutine is
!                   called from spect_amp (see comments in CONST_AM0_GSPRD).
!        01/16/12 - Try a different gspread (r^-0.5 beyond R=50 km for all frequencies;
!                   r^eta for R<=50 km, where eta = -1.0 for f<1 Hz, -1.5 for f > 4 Hz,
!                   with a linear transition (in log f) between).
!        01/19/12 - Allow for either GMA or DMB gsprd
!        01/23/12 - Replace the many options for DMB (with different spreading rates, etc)
!                   with a single option, with variables passed through the
!                   argument list
!        04/05/13 - Add CASE 04 (in GMA & DMB April 2013 atten paper).
!        10/02/13 - Revise CASE 04

      real f, r, r_ref, gsprd_antilog10_gma_nov2011, r1, r2

      real gsprd_pwr_dmb

      integer i_gsprd_option
      
! Note that generally r = hypocentral distance.  For Atkinson and Silva 
! (BSSA 90, 255--274) r is the closest distance to the fault plane ("d" in 
! their paper; below their eq. 4), so that rmod is, in effect, accounting
! source depth twice.  See comments in AS00 section of subroutine
! spect_scale

      SELECT CASE (i_gsprd_option)
       
        CASE(0)
         
          print *,' i_gsprd_option = 0 in call to gsprd_freq, '//
     :            'which is not supposed to occur; QUITTING!' 
          stop
           
        CASE(1)  ! GMA, November, 2011

          r1 = amin1(r, 60.0)
          r2 = amax1(r/60.0, 1.0)

          gsprd_antilog10_gma_nov2011 = -1.6*alog10(r1) 
     :        -0.5*alog10(r2) + delta_gma_nov2011(f, r)  
     
          gsprd_freq = 10.00**gsprd_antilog10_gma_nov2011
        
        CASE(2)  ! DMB, January, 2012
          
          r_ref = 1.0  ! hardwire this
          
          if (r <= r_ref) then
            gsprd_freq = 1/r
          else if (r_ref < r .and. r <= r1) then
            gsprd_freq =  (r/r_ref)**gsprd_pwr_dmb(f, 
     :       r, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2) 
          else
            gsprd_freq = (r1/r_ref)**gsprd_pwr_dmb(f,
     :       r1, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2) 
     :                                   *
     :                       (r/r1)**gsprd_pwr_dmb(f, 
     :       r, r1,
     :       pgsprd_r_le_r1_lf, pgsprd_r_le_r1_hf, pgsprd_r_gt_r1, 
     :       ft1, ft2)
          end if
          
        CASE(3)  ! GMA, September, 2012

          rt = amin1(70.0, (70.0-amin1( 57.2*alog10(f/2),40.0) ))

          if ( r < rt ) then
             gsprd_freq = (r_ref/r)**1.3
          else
             gsprd_freq = (r_ref/rt)**1.3*(rt/r)**0.5
          end if
 
        CASE(4)  ! Atkinson, G.M. and D.M. Boore (2014). The attenuation of Fourier amplitudes 
                 ! for rock sites in eastern North America, Bull. Seismol. Soc. Am. 104, (in press). 

        
          pidiv2 = 1.570796  ! I defined this in SMSIM.FI, but I do not include that file in this subroutine
          

          h4gspread = r1
          r_hinge = 50.0
          
          tc = amax1( 1.0-1.429*alog10( amax1( f, 1.0)), 0.0 )
            
          if ( r <= r_hinge) then
          
            if ( r <= h4gspread) then

              clf = 0.2*cos( pidiv2*(r-h4gspread)/(1.0-h4gspread) )
            
            else  
          
              clf = 0.2*cos( pidiv2*(amin1(r,r_hinge)-h4gspread)/
     :                      (r_hinge-h4gspread) )
          
            end if 
            
            gsprd_freq = 10.0**(tc*clf)*(r_ref/r)**1.3

          else            

            gsprd_freq = (r_ref/r_hinge)**1.3*(r_hinge/r)**0.5

          end if
            
        CASE default
        
          print *,' i_gsprd_option = ', i_gsprd_option,' in call to '// 
     :     'gsprd_freq, for which there is no CASE; QUITTING!' 
          stop
          
      END SELECT
           
      return
      END

      FUNCTION gsprd_pwr_dmb(f, r, r1, 
     :  eta_r_le_r1_lf, eta_r_le_r1_hf, eta_r_gt_r1, ft1, ft2)

        real f, r, r1, 
     :    eta_r_le_r1_lf, eta_r_le_r1_hf, eta_r_gt_r1,
     :    ft1, ft2, gsprd_pwr_dmb
             
        if (r > r1) then
          gsprd_pwr_dmb = eta_r_gt_r1
        else
          if (f <= ft1)                             then
            gsprd_pwr_dmb = eta_r_le_r1_lf
          else if (f > ft1 .and. f <= ft2)          then
            gsprd_pwr_dmb = eta_r_le_r1_lf 
     :        - ((eta_r_le_r1_lf - eta_r_le_r1_hf)/alog(ft2/ft1))*
     :                       alog(f/ft1)
          else           
            gsprd_pwr_dmb = eta_r_le_r1_hf
          end if
        end if           

      return
      END FUNCTION gsprd_pwr_dmb
      
      
      FUNCTION delta_gma_nov2011(f, r)
      real f, r, b
      
      if (f <= 1.0) then
        b = -0.6
      else if (f >= 3.162) then
        b = -0.1
      else
        b = -0.6 + alog10(f)
      end if
      
      delta_gma_nov2011 = 0.7 - amin1(0.7,0.7*exp(b*(r-1.0)))
      
      return
      END FUNCTION delta_gma_nov2011     
!----------------- END GSPRD_FREQ -----------------------------

 
!----------------- BEGIN SPECT_SHAPE -----------------------------
! Source Displacement Spectrum
! Dates: 06/07/95 - Written by D.M. Boore
!        11/15/95 - changed source types
!        12/02/96 - added Atkinson and Silva (model 4) (I did this earlier)
!                   and Haddon (model 5)
!        02/28/97 - Added Atkinson's new version of the source spectra
!                   derived from Atkinson and Silva (this will appear
!                   in Atkinson and Boore... Evaluation of source models...).
!                   (model 6)
!        06/24/97 - added Boatwright and Choy scaling (model 7).
!        07/21/97 - Added Joyner ENA model (model 8; the spectral shape is
!                   the same as his model 2, but I because the corner frequency
!                   relations are new I have to repeat the shape with the new 
!                   model number).
!        09/02/98 - Renamed model 6 to AB98-Ca to be consistent with usage
!                   in Tables 3 and 4 in Atkinson and Boore, BSSA, v. 88, 
!                   p. 917--934.
!        02/16/99 - Added Atkinson and Silva, 1999, as model 9
!        06/05/00 - Changed "AS99" to "AS2000" because the paper was published
!                   in 2000 (BSSA 90, 255--274)
!        07/15/05 - Added new ENA model (from Gail Atkinson)
!        07/16/05 - Replace computed go to with if then else
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        03/24/13 - Add source 12 (added c1_eps, c2_eps as input parameters)

      function spect_shape(f, fa, fb, pf_a, pd_a, pf_b, pd_b, 
     :                     am0b_m0, numsource)
      
      real spect_shape
      
      if (numsource .eq. 1) then  

! Single corner frequency:
        sb = 1.0
        sa = 1.0/( 1.0 + (f/fa)**pf_a )**pd_a 

      else if (numsource .eq. 2) then

! Joyner model
        sb = 1.0/ ( 1.0 + (f/fb)**2 )**0.25
        sa = 1.0/ ( 1.0 + (f/fa)**2 )**0.75

      else if (numsource .eq. 3) then

! Atkinson 1993 model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 4) then

! Atkinson & Silva 1996 (same format as Atkinson 1993) model
       sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 5) then

! Haddon (see 12/02/96 notes; approximate spectra in Fig. 10 of
! Haddon's paper in BSSA, v. 86, p. 1312)
        pda = 1.0/8.0
        pdb = 1.0/8.0
        pfa = 1/pda
        pfb = 1/pdb
        sa = 1.0/( 1.0 + (f/fa)**pfa )**pda
        sb = 1.0/( 1.0 + (f/fb)**pfb )**pdb 

      else if (numsource .eq. 6) then

! AB98-Ca (Atkinson & Boore 1998) (same format as Atkinson 1993) model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 7) then

! Boatwright and Choy (this is the functional form used by 
!  Boore and Atkinson, BSSA 79, p. 1761)
        sa = 1.0
        if (f .ge. fa) sa = fa/f
        sb = 1.0/sqrt( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 8) then

! Joyner model (to be used with his ENA two-corner model)
        sb = 1.0/ ( 1.0 + (f/fb)**2 )**0.25
        sa = 1.0/ ( 1.0 + (f/fa)**2 )**0.75 

      else if (numsource .eq. 9) then

! AS2000 (Atkinson and Silva, 2000, BSSA 90, 255--274) 
! (same format as Atkinson 1993) model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 10) then

! Atkinson 2005 model
        sb = 1.0
        sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**2 )
     :      +      (am0b_m0)/( 1.0 + (f/fb)**2 ) 

      else if (numsource .eq. 11) then
      
! General multiplicative two-corner model      
        sa = 1.0/( 1.0 + (f/fa)**pf_a )**pd_a 
        sb = 1.0/( 1.0 + (f/fb)**pf_b )**pd_b 

      else if (numsource .eq. 12) then
      
! General additive two-corner model      
       sb = 1.0
       sa = (1.0 - am0b_m0)/( 1.0 + (f/fa)**pf_a )**pd_a
     :      +       am0b_m0/( 1.0 + (f/fb)**pf_b )**pd_b 

      else

! Not a legal source number:
        write(*, '(a, i5, a)') ' !!!!!! numsource = ',
     :  numsource, ', .ne. legal value; quitting !!!!!!'
        stop
 
      end if
      
      spect_shape = sa*sb

      return
      end
!----------------- END SPECT_SHAPE -----------------------------

!----------------- BEGIN SPECT_SCALE -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        06/05/96 - Added modified Atkinson & Silva scaling
!        12/02/96 - added Haddon scaling (see spect_shape.for comments)
!        02/28/97 - added Atkinson and Boore scaling 
!                   (see spect_shape.for comments)
!        06/24/97 - added Boatwright and Choy scaling
!        07/10/97 - changed A93, AS96, AS97 scaling to be constant
!                   stress for M < Mc, where Mc is the magnitude for which
!                   am0b_m0 = 1.0.  The single corner model for smaller
!                   magnitudes is determined so that the high frequency
!                   level is matched at M = Mc.
!        07/21/97 - Added Joyner 2-corner model for ENA, as specified 
!                   in his notes prepared for the SSHAC workshop (published 
!                   in SSHAC report, NUREG/CR-6372, p. B-303--B-305, 1997).
!        08/06/97 - I discovered that Joyner erroneously fit vertical spectral
!                   amplitudes.  He provided a revised model, fitting the
!                   horizontal amplitudes.  I changed model 8 accordingly.
!        09/02/98 - Renamed model 6 to AB98-Ca to be consistent with usage
!                   in Tables 3 and 4 in Atkinson and Boore, BSSA, v. 88, 
!                   p. 917--934.
!        02/16/99 - Added Atkinson and Silva (1999)
!        06/05/00 - Changed "AS99" to "AS2000" because the paper was published
!                   in 2000 (BSSA 90, 255--274)
!        07/15/05 - Added new ENA model (from Gail Atkinson)
!        07/16/05 - Replace computed go to with if then else
!        05/11/07 - Removed "\smsim\" from include statements
!        04/10/08 - Add source 11 (as a result, changed pf, pd to pf_a, pd_a)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!                   Explicitly set the stress variables to 0.0 for those source
!                   models that do not use stress as a parameter.
!        03/24/13 - Added source 12

      subroutine spect_scale()


      include 'smsim.fi'

      am0 = 10.**(1.5*amag + 16.05)  ! Now put this into the programs in which amag is specified, to 
                                     ! decrease confusion about where am0 is computed.
      am0b_m0 = 0.0
  
      if (numsource .eq. 1) then  

! Single corner frequency:
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fa = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        fb = fa
 
      else if (numsource .eq. 2) then

! Joyner scaling:
        am0c = 10.0 ** ( 1.5*amagc + 16.05 )
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        rat = stress/am0
        dum = 4.906e+06
        if ( am0 .gt. am0c ) rat = stress/am0c
        fb = ( dum*beta ) * ( fbdfa )**(3./4.) * ( rat )**(1./3.)
        fa = ( dum*beta ) * (stress)**(1./3.) * (am0c)**(1./6.)
     :     * ( fbdfa )**(-0.25) * ( am0 )**(-0.5)
        if ( am0 .lt. am0c ) fa = fb / fbdfa
 
      else if (numsource .eq. 3) then

! Atkinson 93 scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 4.0) then
          fa = 10.0**(2.41 - 0.533 * amag)
          fb = 10.0**(1.43 - 0.188 * amag)      ! fa = fb for M = 2.84
          am0b_m0 = 10.0**(2.52 - 0.637 * amag)
        else
          fb = 10.0**(2.678 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 4) then

! Atkinson and Silva 96 scaling, with am0b_m0 modified by D. Boore on 6/04/96

        stressc = 0.0        
        dlsdm = 0.0
        
        if (amag .gt. 4.6) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(1.778 - 0.302 * amag)   ! fa = fb for M = 2.08
          am0b_m0 = 10.0**(3.440 - 0.746 * amag)  ! DMB's fitting of spctrl ratios
!        am0b_m0 = 10.0**(2.764 - 0.623 * amag) ! in Atkinson & Silva preprint
        else
          fb = 10.0**(2.689 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 5) then

! Haddon scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(0.3 - (1.5/3)*(amag-4.0))
        fb = 10.0**(1.4 - (1.5/3)*(amag-4.0))  ! fa < fb for all M
 
      else if (numsource .eq. 6) then

! AB98-Ca (Atkinson and Boore 98 scaling, based on fitting a model to the 
! Atkinson and Silva 1997 Fourier amplitude spectra for California; see
! Atkinson and Boore, BSSA, v. 88, p. 917--934).

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 4.8) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(1.308 - 0.227 * amag)      ! fa=fb for M = 3.25
          am0b_m0 = 10.0**(3.223 - 0.670 * amag)
        else
          fb = 10.0**(2.617 - 0.5 * amag)
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 7) then

! Boatwright and Choy (this is not from Boore and Atkinson, BSSA 79, p. 1761;
!  it is based on new fits by DMB on 6/24/97 to data in Boat.&Choy, 1992 BSSA.
!  See BC_BSSA.WQ1 in \haddon subdirectory and handwritten notes on 
!  yellow sheets.
!  except set fa=fb=constant stress scaling for M<=5.3)

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(3.409 - 0.681 * amag)
        fb = 10.0**(1.495 - 0.319 * amag)
        if (amag .le. 5.3) then
          fa = 0.634*10.0**(0.5*(5.3 - amag)) ! 0.634= 10^(logfa+logfb)/2 at M5.3
          fb = fa
        end if
 
      else if (numsource .eq. 8) then

! Joyner ENA scaling:

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(2.312 - 0.5 * amag)
        fb = 10.0**(3.609 - 0.5 * amag)
 
      else if (numsource .eq. 9) then

! Atkinson and Silva (2000) (AS00) scaling, based on fitting a point source
! model to finite source calculations, with constraints on various modeling
! studies, with modification for very small magnitude (when eps = 1).
! Note that in AS00 the distance is altered by using a magnitude-dependent
! depth (equation 4 in AS00).  Until the 12/17/09 revision, the distance
! was recomputed in gsprd for numsource = 9, but now I let this be
! done independently of the source number (but this means that for consistency,
! the coefficients defining f_ff in the parameter file should be those used 
! in AS00).  If the distance is modified for the AS00 source, note that 
! comparisons of spectra at close
! distances for input r = 1, 10 (for example) will not differ by about 10 (for
! a geometrical spreading of 1/r).  The application of the effective depth is
! not implemented as precisely as it should be, because this would require
! specifying the depth as an input parameter (or specifying the distance as
! the closest horizontal distance to the surface projection of the fault (rjb)),
! and then using the effective depth for the as00 scaling, or a period
! dependent pseudodepth, such as from bjf97.    This all has to do with
! the modifications needed to approximate finite-fault ground motions.  This
! is material for a future upgrade to the program.

        stressc = 0.0
        dlsdm = 0.0
        
        if (amag .gt. 2.4) then
          fa = 10.0**(2.181 - 0.496 * amag)
          fb = 10.0**(2.41  - 0.408 * amag)      ! fa=fb for M = -2.6
          am0b_m0 = 10.0**(0.605 - 0.255 * amag)
        else
          fb = 10.0**(1.431 - 0.5 * (amag - 2.4))
          fa = fb
          am0b_m0 = 1.0
        endif
 
      else if (numsource .eq. 10) then

! Atkinson 2005 scaling:  

        stressc = 0.0
        dlsdm = 0.0
        
        fa = 10.0**(2.41 - 0.533 * amag)
        fb = 10.0**(2.06 - 0.345 * amag)  
        am0b_m0 = 10.0**(1.2 - 0.3 * amag)
 
      else if (numsource .eq. 11) then

! General two-corner source (see smsim_generalization_of_2-corner_frequency_source_models_v04.pdf):
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fc = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        fa = 10.0**(c1_fa + c2_fa * (amag-amagc4fa))
        fb = (fc**2/fa**(pf_a*pd_a))**(1.0/(pf_b*pd_b))



      else if (numsource .eq. 12) then

! General two-corner source (see smsim_generalization_of_2-corner_frequency_source_models_v04.pdf):
        stress = stressc*10.0**(dlsdm*(amag-amagc))
        fc = (4.906e+06) * beta * (stress/am0)**(1.0/3.0)
        if (amag < mag4eps1) then
          fa = fc
          fb = fc
          am0b_m0 = 1.0
        else
          fa = 10.0**(c1_fa + c2_fa * (amag-amagc4fa))
          eps = 10.0**(c1_eps + c2_eps * (amag-amagc4eps))
          am0b_m0 = eps 
          if (eps < 1.0) then
! Trap for fb undefined
            ds_min = ((sqrt(1.0-eps)/(4.906e+06 * beta))*fa)**3*am0
            if (stress < ds_min) then
              print *,' WARNING: for source 12, stress = ',stress
              print *,' is less than the minimum stress of ',ds_min
              print *,' for fa and eps = ', fa, eps
              print *,' QUITTING because fb is undefined!!!'
              stop
            end if
          end if
          fb = sqrt( (fc**2 - (1-eps)*fa**2)/eps )
        end if

      else

! Not a legal source number:
        write(*, '(a, i5, a)') ' !!!!!! numsource = ',
     :  numsource, ', .ne. legal value; quitting !!!!!!'
        stop

      end if

      return

      end
!----------------- END SPECT_SCALE -----------------------------

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

!----------------- BEGIN DIMIN -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added modification to r required by Atkinson
!                   and Silva (1999)
!        06/05/00 - Substitute c_q for beta in akappaq and add comments
!                   about r
!        05/11/07 - Removed "\smsim\" from include statements
!        12/01/09 - Check for fm = 0.0 or kappa = 0.0; if either is
!                   true, do not include that parameter in the
!                   calculations (only have to check for fm = 0.0,
!                   because kappa = 0.0 is the same as not using
!                   kappa).
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/17/09 - Bring in rmod as a calling argument, not 
!                   calculated only if numsource = 9 (note on 04/08/11:  sometime between
!                   12/17/09 and 04/08/11 I bring rmod in through smsim.fi).
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.

      function dimin(f)
      real dimin, mag
      include 'smsim.fi'

      akappaq = rmod/(c_q*q(f))

      mag = amag    
      
      if (fm .eq. 0.0) then
        dimin = exp( -pi*(kappa_f(mag) + akappaq) * f) 
      else
        dimin = exp( -pi*(kappa_f(mag) + akappaq) * f)/
     :   sqrt( 1. + (f/fm)**8.0 )
      end if
      

      return
      end
!----------------- END DIMIN -----------------------------

!----------------- BEGIN KAPPA_F -----------------------------
! Dates: 02/28/97 - Written by D.M. Boore
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.

      function kappa_f(mag)
      real mag
      include 'smsim.fi'
  
      kappa_f = akappa + dkappadmag*(mag-amagkref)

      return
      end
!----------------- END KAPPA_F -----------------------------
        
!----------------- BEGIN Q -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        12/14/95 - Added check for equal transition frequencies
!        05/11/07 - Removed "\smsim\" from include statements
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        05/16/15 - Some modernization of code

      function q(f) 

      logical, save ::  firstcall_q = .true.
      real(4), save ::  qt1, qt2, st
      
      real, intent(in) :: f
      real(4) :: q      
      
      include 'smsim.fi'

      q = 9999.0
      if (f == 0.0) return
        
      if (firstcall_q) then
        qt1 = qr1*(ft1/fr1)**s1
        qt2 = qr2*(ft2/fr2)**s2
        st = 0.0
        if (ft1 /= ft2) then
          st = alog10(qt2/qt1)/alog10(ft2/ft1)
        end if
        firstcall_q = .false.
      end if
      
      if ( f <= ft1) then
        q = qr1*(f/fr1)**s1
      else if ( f >= ft2) then
        q = qr2*(f/fr2)**s2
      else
        q = qt1*(f/ft1)**st
      end if

      return
      end
!----------------- END Q -----------------------------


!----------------- BEGIN DURSOURCE -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
      function dursource(w_fa, fa, w_fb, fb)
      real dursource
      dursource = w_fa/fa + w_fb/fb
      return
      end
!----------------- END DURSOURCE -----------------------------
      
!----------------- BEGIN DURPATH -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        07/02/99 - Added magnitude-dependent "depth" from Atkinson
!                   and Silva, which required adding some parameters to
!                   the passed arguments of durpath
!        06/05/00 - Changed reference for Atkinson and Silva (1999) to
!                   2000, the year in which their paper was published
!                   and added comments regarding r
!        12/04/09 - Add test for rmod <= rdur(1)
!        12/05/09 - Rearrange right hand side to be dur(j)+dur
!        12/17/09 - Bring in rmod as a calling argument

      function durpath(rmod, nknots, rdur, dur, slast, numsource, amag)
      real rdur(*), dur(*), durpath

      if ( rmod .le. rdur(1) ) then
        durpath = dur(1)
      else if ( rmod .ge. rdur(nknots) ) then
        durpath = dur(nknots) + (rmod -rdur(nknots))*slast 
      else
        call locate(rdur, nknots, rmod, j)
        durpath = dur(j) + (rmod - rdur(j))*( dur(j+1)-dur(j))
     :            /(rdur(j+1)-rdur(j))
      end if

      return
      end
!----------------- END DURPATH -----------------------------

!----------------- BEGIN SPINS -----------------------------
      function  spinsf (f,pp,pg,s,hp,hg)
!   this subroutine computes the frequency response of the WWSSN
!   short period instrument, which is heavily influenced by the
!   transducer inductance.  see reference by Burdick and Mellmann,
!   'The response of the WWSSN short period seismometer';
!   contribution no. 2628, Cal. Tech. Division of Geological and
!   Planetary Sciences.
!   latest version: 11/24/83
!
!   input variables
!
!       f:  frequency
!       pp:  transducer free period
!       pg:  galvanometer free period
!       s:  coupling factor squared
!       hp:  transducer damping
!       hg:  galvanometer damping
!       r1:  resistance of  the transducer branch
!       r2:  resistance of the galvanometer branch
!       r3:  shunt resistance
!       l:  inductance of the transducer
!
!   the values for the WWSSN short period instruments are;
!
!   fmax:  1.7 Hz        pp: 1.0 s         pg: 0.75 s
!      s:  0.0           hp: 0.490         hg: 1.00
!
!   a circuit equivalent to the WWSSN short period instrument
!   in operating conditions is:
!
!         damp trim
!         resistor
!          rb=22. ohms           rc=68. ohms            re=90.ohms
!     ________/\/\/\/\___________/\/\/\/\/\_____________/\/\/\/\/\__
!    1                                           1                  1
!    1                                           1                  1
!    1                                           1                  1
!    ( transducer coil                   shunt   /                  1
!    )  l= 6.8 henries               resistance  \      galvanometer*
!    (  ra=62.5 ohms                rd= 18. ohms /       rf= 80 ohms*
!    )  pp= 1.0 sec                              \       pg= .75 sec*
!    (                                           /                  *
!    1                                           1                  1
!    1                                           1                  1
!    ________________________________________________________________
!
!
!    r1= ra +rb + rc = 152.5 ohms
!    r2 = re + rf = 170 ohms
!    r3 = rd = 18 ohms
!    l = 6.8 henries
!
!
      real l
      r1=152.5
      r2=170.
      r3=18.0
      l=6.8
      twopi = 6.28318531
      fn1 = twopi/pp
      fn2 = twopi/pg
      fk1 = hp*fn1
      fk2 = hg*fn2
      w = twopi*f
      w2 = w**2
!
!
      q2 = r1*r2 + r2*r3 + r3*r1
      u = l*(r2+r3)*w/q2
      fa = 1./sqrt(1.+u**2)
      cplr2 = s*(fa**4)*(1.-u**2)
      cpli2 = s*(fa**4)*2.*u
!
!
      es = fk1*(fa**2)
      eg = fk2*(fa**2)*(1.+(l*w*u)/(r1+r3))
!
!
      ws2 = (fn1**2)+2.*fk1*w*u*(fa**2)
      wg2 = (fn2**2) +2.*fk2*w*(fa**2)*w*l*(r3**2)
     */(q2*(r1+r3))
!
!
      aim = (ws2 - w2)*(wg2-w2)+(cplr2-4.*es*eg)*w2
      are = 2.*es*w*(wg2-w2)+2.*eg*w*(ws2-w2)-cpli2*w2
!
!
      spinsf = (w**3)*fa/sqrt((are**2)+(aim**2))
!      arg = atan2(aim,are)-atan(u)
!  arg: phase response (-3*pi/2 to pi)   
      return
      end
!----------------- END SPINS -----------------------------

!  ------------------- BEGIN BUTTRLCF -------------------------------
! Dates: 06/07/95 - Written by D.M. Boore
      function buttrlcf( f, fcut, norder)
!
! Computes the response of an norder, bidirectional
! high-pass Butterworth filter.  This is the filter
! used by the AGRAM processing (the equation was
! provided by April Converse).

! Modification: 3/27/89 - created by modifying HiPassF

      real buttrlcf
      buttrlcf = 1.0
      if ( fcut.eq.0.0 ) return

      buttrlcf = 0.0

      if ( f .eq. 0.0) return

      buttrlcf = 1.0/ (1.0+(fcut/f)**(2.0*norder))

! Note: Because this filter is intended to simulate the
! two-pass, zero-phase (acausal) Butterworth filter used in
! accelerogram processing, buttrlcf = 1/2 when f = fcut, not 1/sqrt(2) as in
! a single-pass, causal Butterworth filter.

      return
      end
!  ------------------- END BUTTRLCF -------------------------------

!  ------------------- BEGIN HIGH_CUT_FILTER ------------------------ 
!      function high_cut_filter(f, itype_hcfilt, fhc1, fhc2, eta_hcfilt)
      function high_cut_filter(f)

! Dates: 12/11/15 - Written by D. Boore

      real high_cut_filter
      
      include 'smsim.fi'

      high_cut_filter = 1.0
      if ( itype_hcfilt == 0 .or. f <= fhc1) return
      
      if ( f >= fhc2 ) then
        high_cut_filter = 0.0
        return
      end if
      
      if ( itype_hcfilt == 1 ) then  ! raised half-cycle of cosine
        high_cut_filter = 
     :     (0.5*(1.0+cos(pi*(f - fhc1)/(fhc2 - fhc1))))**eta_hcfilt
      else if ( itype_hcfilt == 2 ) then  ! quarter-cycle of cosine
        high_cut_filter = 
     :     (cos(0.5*pi*(f - fhc1)/(fhc2 - fhc1)))**eta_hcfilt
      else
        write(*,'(a)') ' ERROR: itype_hcfilt /= 0, 1, or 2; QUITTING!!'
! A valid value for itype_hcfilt was checked in get_params, so this statement is redundant.
        stop
      end if
      
      return
      end
!  ------------------- END HIGH_CUT_FILTER ------------------------ 




!----------------- BEGIN SMSIMFAS -----------------------------
      subroutine SMSIMFAS(fas, freq, nfreq)
! Dates: 12/16/95 - Written by D.M. Boore, based on SMSIM_RV
!        03/05/99 - Removed include statements from the end (put them into the
!                   driver programs)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        11/29/11 - Add possibility that gspread is frequency dependent; see remarks in  
!                   spect_amp, and const_am0_gsprd.

      real fas(*), freq(*)
      include 'smsim.fi'

      pi = 4.0*atan(1.0)
      twopi = 2.0 * pi

! Set spectral parameters:
      call spect_scale()   ! Be sure to call Get_Params in driver

! Get frequency-independent factor:
      call const_am0_gsprd()

! Be sure to call const_am0 before spect_amp, because spect_amp 
! uses freq_indep_factor, computed in const_am0 and passed into spect_amp
! through the common block /const_params/.

      do i = 1, nfreq
        fas(i) = spect_amp(freq(i))
      end do

      return
      end
!----------------- END SMSIMFAS -----------------------------

! --------------------------- BEGIN BJFV1V2F -------------------          
      function BJFV1V2F(per, V1, V2)

! Returns the correction factor to apply to response spectra with V30 = V1
! to obtain the response spectra for V30 = V2 (i.e., computes PRV(V2)/PRV(V1)).
! The value for per = 0.1 is used for all smaller periods and the per=2.0 value 
! is used for periods longer than 2.0 sec (in an earlier version I used
! per = 1.0).  Note that the latter is
! conservative; we expect the amplifications to reach unity for long enough
! periods (at least for Fourier spectra, what about for response spectra?)

! Dates: 07/17/96 - Written by D. Boore, based on BJFR2S_F.FOR
!        07/18/96 - Changed endpoint period from 1.0 to 2.0
! Dates: 07/24/96 - Added check of V1, V2 equal zero, in which case unit
!                   amplification is returned.
!                   Also, reversed the meaning of V1 and V2.
!                   Added pga amps when per = 0.01
!        10/08/00 - pga when per = 0.0, rather than 0.01

      if (v1 .eq. 0.0 .or. v2 .eq. 0.0) then
         bjfv1v2f = 1.0
         return
      end if

      velfact = alog10( V2/V1 )
      if(per .lt. 0.1) then
         if(per .eq. 0.0) then
           bjfv1v2f = 10.0**(velfact*(-0.371))
         else           
           bjfv1v2f = 10.0**(velfact*cubic(0.1))
         end if
      else if (per .gt. 2.0) then
         bjfv1v2f = 10.0**(velfact*cubic(2.0))
      else
         bjfv1v2f = 10.0**(velfact*cubic(per))
      end if

      return
      end

      function cubic(per)
        c0 = -0.21172
        c1 =  0.06619
        c2 = -1.35085
        c3 =  0.79809
        x = alog10(per/0.1)
        cubic = c0 + c1*x + c2*x**2 + c3*x**3
!        a0 = 0.2102
!        a1 = 0.0726
!        a2 = -0.3142
!        a3 = -0.2403
!        x = alog10(per)
!        cubic = a0 + a1*x + a2*x**2 + a3*x**3
      return
      end
! --------------------------- END BJFV1V2F ------------------- 

! ---------------------------------------------------------- BEGIN RMOD_CAlC
      function rmod_calc(r, m, iflag, nlines, c1, c2, mh, c3, c4,
     :                   m1t, m2t, c0t, c1t, c2t, c3t, f_ff) 
      
!!
!!finite_fault factor specification:
!!  iflag_f_ff, nlines, c1, c2, c3, c4, DeltaM (0 0 0 0 0 0 0 if a finite-fault factor is not to be used)
!!
!!  Distance for point-source calculation
!!    If iflag_f_ff = 1: rps = sqrt(r^2 + f_ff^2))
!!    If iflag_f_ff = 2: rps =  r + f_ff
!!   Use rps in the calculations (this variable is called rmod in the code; it should be changed to rps to
!!   reflect my current preferred terminology.  I do not have time to do this now).
!!  Specification of the finite-fault factor h:
!!    If nlines = 1
!!      log10(f_ff) = c1 + c2*amag  
!!    If nlines = 2
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh
!!      where Mh is determined by the intersection of the two lines
!!      (this is computed in the program)  
!!    If nlines = 3
!!      log10(f_ff) = c1 + c2*amag  for amag<Mh-DeltaM/2
!!      log10(f_ff) = c3 + c4*amag  for amag>=Mh+DeltaM/2
!!      log10(f_ff) given by a cubic in amag between the two lines (this
!!        produces a smooth transition over the magnitude range DeltaM
!!  *** NOTE: placeholders are needed for c3, c4, and DeltaM, even if not used.
!!
!!  Published finite-fault factors
!!    Author                      applicable_region meaning_of_r  iflag_f_ff nlines         c1      c2   c3    c4  
!!    Atkinson and Silva (2000)                 ACR        r_rup           1    1      -0.0500  0.1500  0.0   0.0
!!    Toro (2002)                               SCR        r_rup           2    1      -1.0506  0.2606  0.0   0.0
!!    Atkinson and Boore (2003)          subduction        r_rup           1    1      -2.1403  0.5070  0.0   0.0
!!    Yenier and Atkinson (2014)                ACR        r_rup           1    1      -1.7200  0.4300  0.0   0.0
!!    Yenier and Atkinson (2015)                ACR        r_rup           1    1      -0.4050  0.2350  0.0   0.0
!!    Yenier and Atkinson (2015),               SCR        r_rup           1    1      -0.5690  0.2350  0.0   0.0
!!    Boore and Thompson  (2015) (BT15)        see below
!!  
!!  Input for some of the models, as well as suggested modifications for stable continental regions
!!    Assuming that all of the above the above relations except Toro (2002) and Atkinson and Boore (2003)
!!    are for active crustal regions, and that h is proportional to fault radius, then -0.1644 should be
!!    added to c1 (and c3 for Boore (2014) to adjust for the smaller fault size expected for stable continental region
!!    earthquakes (this adjustment factor uses radius ~ stress^-1/3, and a stress of 88 bars for ACR (from 
!!    my determination of what stress matches the Atkinson and Silva (2000) high-frequency spectral level--
!!    see What_SCF_stress_param_is_consistent_with_the_AS00_source_model.pdf in the daves notes page of
!!    www.daveboore.com) and 274 bars for SCR, from my inversion of 0.1 s and 0.2 s PSA values for 8 ENA
!!    earthquakes, using the Boatwright and Seekins (2011) attenuation model.  This determination is 
!!    part of ongoing work for the NGA-East project, and will appear in a PEER report in 2015.
!!   1    1      -0.0500  0.1500  0.0   0.0 0.0       ! ACR: AS00
!!   1    2      -1.7200  0.4300 -0.405 0.2350 0.0    ! ACR: YA14&YA15, no smoothing
!!   1    3      -1.7200  0.4300 -0.405 0.2350 2.0    ! ACR: BT15 (=YA14&YA15, smooth over 2 magnitude units
!!   1    1      -1.7200  0.4300  0.0   0.0 0.0       ! ACR: YA14
!!   1    1      -0.4050  0.2350  0.0   0.0 0.0       ! ACR: YA15
!!
!!   1    1      -0.2144  0.1500  0.0   0.0 0.0       ! SCR: AS00
!!   1    2      -1.8844  0.4300 -0.5694 0.2350 0.0   ! SCR: YA14&YA15, no smoothing
!!   1    3      -1.8844  0.4300 -0.5694 0.2350 2.0   ! SCR: BT15 (=YA14&YA15, smooth over 2 magnitude units)
!!   1    1      -1.8844  0.4300  0.0   0.0 0.0       ! SCR: YA14
!!   1    1      -0.5694  0.2350  0.0   0.0 0.0       ! SCR: YA15
!   0 0 0.0 0.0 0.0 0.0 0.0                           ! No f_ff

      
! Dates: 04/05/11 - Written by D. Boore
!        10/08/14 - Allow for two lines
!        12/18/14 - Allow for a transition curve between the two lines

      real rmod_calc, r, m, mh, m1t, m2t, m4calc
      
      if (iflag == 0) then
        f_ff = 0.0
        rmod_calc = r
        return
      else 
        if (nlines == 1) then
          f_ff = 10.0**(c1 + c2 * m)
        else if (nlines == 2) then
          if (m <= mh) then
            f_ff = 10.0**(c1 + c2 * m)
          else
            f_ff = 10.0**(c3 + c4 * m)
          end if
        else if (nlines == 3) then
          if (m <= m1t) then
            f_ff = 10.0**(c1 + c2 * m)
          else if (m >= m2t) then
            f_ff = 10.0**(c3 + c4 * m)
          else
            m4calc = m - m1t
            f_ff = 10.0**
     :        (c0t + c1t*m4calc + c2t*m4calc**2.0 + c3t*m4calc**3.0)
          end if        
        else
          print *, ' within rmod_calc, nlines has an invalid value; '//
     :         'nlines = ', nlines
          print *, ' STOP the program!'
          stop
        end if
      end if
      
      if (iflag == 1) then
        rmod_calc = sqrt(r**2 + f_ff**2)
        return
      else if (iflag == 2) then
        rmod_calc = r + f_ff 
        return
      else
        write(*,*) ' In rmod_calc, iflag = ', iflag, 
     :             ' is not valid; QUITTING'
        stop
      end if
      
      end
! ---------------------------------------------------------- END RMOD_CAlC
 
 