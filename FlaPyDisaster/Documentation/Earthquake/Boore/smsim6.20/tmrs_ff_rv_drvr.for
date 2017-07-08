* ----------------- BEGIN TMRS_FF_RV_Drvr ---------------------------
      Program TMRS_FF_RV_Drvr

*  Compute SMSIM simulations for a finite-fault model.
*  Obtains parameters (including a list of oscillator periods,
*  magnitudes, and distances) from a file.  The only thing obtained
*  from interactively from the screen is the name of the control file.

*  The control file contains up to 60 comment lines, each line preceeded 
*  by "!", with a line of input parameters following each group of 
*  comment lines.  The processing stops after "Stop" is encountered in a line
*  (the character string "stop" can be any combination of uppercase and
*  lowercase letters).  Here is a sample of the control file:

*! Control file for program trms_ff_rv_drvr
*! Revision of program involving a change in the control file on this date:
*   02/19/09
*!Name of File with Input Parameters:
*! ***WARNING*** For consistency, the source specified
*! in the parameter file should use DeltaSigma as a parameter
*! (of the 11 sources currently built into the smsim
*! programs, only sources 1, 2, and 11 use the DeltaSigma
*! as a free parameter).  If not, the program writes an error message and quits.
* ofr4.params
*!Output file names stem
*  ff_rv_drvr_m7.0_stress_250_az045_54sites
*!lat and lon of upper edge of fault
*  0.0 0.0  
*!strike,dip, depth of fault
*  0.0 50.0 2.0            	
*!fault type (S=strikeslip; R=reverse; N=normal; U=undifferentiated) 
*! (Only used if Wells and Coppersmith is used to obtain FL and FW).
*  R                               
*!fault length and width, dl, dw, stress_ref
*!Note: Force program to use Wells and Coppersmith for FL and/or FW if either entry = 0.0
*  0.0 0.0 1.5 1.5 70.0 ! used for WC scaling (need an entry as a placeholder even if not used_
*!fq
*   10.0
*!coord flag (1=lat,long; 2=R,Az; 3=N,E)
*   2                      	
*!If "Y" below and strike = 0.0:
*!  if coord flag = 2, move origin of the radial line to the midpoint of
*!                         the top edge of the fault
*!  if coord flag = 3 and coord(1) = 0, redefine coord(1) to be
*!                         the midpoint of the top edge of the fault (so that the
*!                         sites will be along a line normal to the midpoint
*!  if coord flag = 3 and coord(2) = 0, redefine coord(1) to be the
*!                         far end of the fault, so that the sites are along a line
*!                         along the strike of the fault 
* Y
*!Damp PerOsc    M  stress coord(1) coord(2) 
*0.05	-1	7      250     1.00	45	
*0.05	0.0	7      250     1.12	45	
*0.05	0.2	7      250     1.26	45	
*0.05	2.0	7      250     1.41	45	
*  stop
  
* The column headings on output are "psa" to indicate that the output is
* pseudo absolute acceleration response spectra.  If PerOsc < 0.0, however,
* the computed values are actually pgv.  Because the output can mix 
* computations of psa and pgv and only one column heading is used, an earlier 
* version of the program used the generic "Y" for the column heading.  In
* this version I decided that it was best to use
* "psa" to make it clear what response spectral ordinate was being computed
* for the most common case (psa will be computed more often than pgv) at the
* risk of confusion when pgv is computed.  

*  Uses Random Vibration routines.

* Notes regarding modification of driver:
*  1. All "include" statements assume all subprograms are in the same folder
*     as this program.
*  2. If values of stress other than those in the input parameter file are
*     to be used, set "stressc" equal to the new value, rather than "stress"
*     (and also set the other parameters if the stress changes with moment).

* NOTE: The output is psa, not psv!!!

* Dates: 02/19/09 - Written by D.M. Boore; Modified from tmrs_rv_drvr and reff
*        12/01/09 - Redo computation of fup, depending on the values of fmax and kappa
*        12/16/09 - Unlike other revisions made on about this date, do NOT
*                   modify r by using an effective depth; this requires setting
*                   rmod = r, because the computational subprograms now use
*                   rmod rather than r.
*        12/22/09 - removed constraint that numsource = 1
*                 - Write numsource to output file
*        02/14/10 - As a result of feedback from John Douglas, I replaced "cr"
*                   in the screen prompt with "Enter".
!        08/25/10 - Shorten "patience" screen message.
!        04/08/11 - Remove numsource from the subprogram argument lists (see
!                   comments in gsprd_f for the reason).
!        09/21/11 - Write smsim parameter file name in the first line of the output file.
!        12/02/11 - Add computation of am0 right after specification of amag (previously, it
!                   was contained in spect_scale, but it was not clear from where that routine was
!                   called). 
!        01/30/14 - Correct bug: the Q params were being printed in write_Par before they were obtained in the call
!                   to get_params.  The correction is to remove their being printed in write_Par, as they are printed
!                   to the summary file in write_params, after the call to get_params
  
      dimension siteLocation(2)
      character f_stem*120
      character f_write_input_params*120, f_out*120,  
     :          f_smsim_params*120
      character cmnts2skip(50)*80, buf_in*10
      character date*8, time_start*10, time_stop*10,
     :                   time_begin*10, time_end*10
      
     
      logical f_exist, specify_length, specify_width, move_site
 
      character fault_type*10, f_ctl*120

      character buf*80

      real smsim_out

      logical tdflag, fparam_exist

      include 'smsim.fi'

      pi=4.0*atan(1.0)
      twopi = 2.0 * pi
      d2r = pi/180.0

      f_exist = .false.
      do while (.not. f_exist)
        f_ctl = ' '
        write(*, '(a\)') 
     :    ' Enter name of input control file '//
     :    '("Enter" = tmrs_ff_rv_drvr.ctl): '
        read(*, '(a)') f_ctl
        call trim_c(f_ctl,nc_f_ctl)
        if (f_ctl(1:4) .eq. '    ') 
     :                         f_ctl = 'tmrs_ff_rv_drvr.ctl'
        call trim_c(f_ctl, nc_f_ctl)
        inquire(file=f_ctl(1:nc_f_ctl), exist=f_exist)
        if (.not. f_exist) then
          write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        end if
      end do
      call get_lun(nu_ctl)
      open(unit=nu_ctl, file=f_ctl(1:nc_f_ctl), status='unknown')

      call getInputParameters(nu_ctl,
     :            FaultStrike,FaultDip,h,
     :            FaultLat,FaultLon, move_site,
     :            fault_type,
     :            FaultLength, FaultWidth, 
     :            dl, dw, 
     :            specify_length, specify_width, stress_ref, 
     :            fq,
     :            isitecoordflag_in,
     :            f_stem, nc_f_stem, f_smsim_params, nc_f_smsim_params)

      call banner(6)

      f_write_input_params = ' '
      f_write_input_params = f_stem(1:nc_f_stem)//'_summary.out'
      call trim_c(f_write_input_params, nc_f_write_input_params)
      call get_lun(nu_write_params)
      
      open (nu_write_params,
     :   file=f_write_input_params(1:nc_f_write_input_params),
     :                  status='unknown')
 
      call writePar (nu_write_params,
     :   f_smsim_params, nc_f_smsim_params, 
     :   FaultStrike,FaultDip,h,
     :   FaultLat, FaultLon,  
     :   move_site,
     :   fault_type,
     :   FaultLength, FaultWidth, dl, dw,
     :   specify_length, specify_width, stress_ref,
     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s,
     :   fq)

! commented these out on 01/30/14     
!     :   qr1, s1, ft1, ft2, fr2, qr2, s2, c_q, fq,
!     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s)
      
      f_out = ' '
      f_out = f_stem(1:nc_f_stem)//'_psa.out'
      call trim_c(f_out, nc_f_out)
      call get_lun(nu_out)
      open(nu_out, file=f_out(1:nc_f_out), status='unknown')

      write(nu_out,'(a)') ' Parameter file: '//
     :  f_smsim_params(1:nc_f_smsim_params)
    
      write(nu_out,'(
     :                1x,a,
     :                4x,a, 
     :                1x,a, 1x,a, 7x,a, 7x,a, 3x,a, 5x,a,
     :                1x,a, 1x,a,  
     :                1x,a, 1x,a,  
     :                1x,a, 1x,a, 
     :                1x,a, 1x,a,
     :                5x,a, 3x,a, 5x,a, 8x,a,
     :                2x,a, 1x,a, 2x,a,
     :                3x,a, 
     :                5x,a, 
     :                3x,a, 
     :                4x,a, 
     :                1x,a 
     :                                        )')
     :  'index', 
     :  'm', 
     :  'numsource', 'stress', 'fl', 'fw', 'dip', 'h',
     :  'siteloc1_in', 'siteloc2_in', 
     :  'sitecoord(1)', 'sitecoord(2)', 
     :  'isitecoordflag', 'move_site',
     :  'site_lat_degrees', 'site_lon_degrees', 
     :  'd_jb', 'd_cd2f', 'reff', 'r',
     :  'damp', 'perosc', 'freqosc',  
     :  'psa(cgs)', 
     :  'psa(g)',
     :  'psv(cgs)', 
     :  'sd(cgs)', 
     :  'Arias(cgs)' 
      

      call banner(nu_write_params)

      write(nu_write_params, '(2a)') ' parameter file: ',
     :   f_smsim_params(1:nc_f_smsim_params)
      write(nu_write_params, '(2a)') ' control file: ',
     :   f_ctl(1:nc_f_ctl)
      
      write(nu_write_params, '(a)') 
     :  ' *** Results computed using tmrs_ff_rv_drvr ***'

      tdflag = .false.
      call get_params(f_smsim_params(1:nc_f_smsim_params), tdflag )
      
      if (numsource /= 1 .and. numsource /= 2 .and. 
     :    numsource /= 11)                            then
        print *, ' ERROR: numsource /= 1, 2, or 11; QUITTING!!!'
        stop
      end if
      
      call write_params(nu_write_params, tdflag)

* Compute fup.  Because the same code appears in a number of drivers, I've
* consolidated it into a subroutine.

      call get_fup(fm, akappa, amp_cutoff, nu_write_params, fup)

* Standard Fortran 90 intrinsic Subroutine DATE_AND_TIME
      call DATE_AND_TIME( date, time_begin )
* Date is returned as 'CCYYMMDD' (character date*8) 
* Time is returned as 'hhmmss.sss' (character time*10)
*     character datx*8, time_start*10 
      write(nu_write_params, *)
      write(nu_write_params, '(a)') 
     :   ' Date: '//date(1:4)//'/'//date(5:6)//'/'//date(7:8)
      write(nu_write_params, '(a)') 
     : ' Time Begin: '//
     : time_begin(1:2)//':'//time_begin(3:4)//':'//time_begin(5:10)
      write(nu_write_params, *)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)

      n_index = 0
888   continue

      buf = ' '
      read(nu_ctl,'(a)',end=999) buf
      call upstr(buf)
      call trim_c(buf,nc_buf)
      
      if (buf(1:4) .eq. 'STOP') go to 999
      
      call DATE_AND_TIME( date, time_start )

      read(buf(1:nc_buf),*) damp, perosc, amag, stress_in, 
     :                      siteloc1_in, siteloc2_in
      am0 = 10.**(1.5*amag + 16.05)
     

      siteLocation(1) = siteloc1_in
      siteLocation(2) = siteloc2_in
      
      print *,' SiteLocation(1),(2) = ', siteLocation(1),siteLocation(2)
      
      call get_fault_dimensions(
     :    fault_type, amag, stress_in, stress_ref,
     :    dl, dw, nl, nw, nsubs,
     :    specify_length, faultlength, 
     :    specify_width, faultwidth) 

      isitecoordflag = isitecoordflag_in

      call set_location(faultLength,move_site, isitecoordflag, 
     :                        siteLocation)
      call compute_distances(
     :    FaultLat, FaultLon, 
     :    FaultLength, FaultWidth, 
     :    FaultStrike, FaultDip, h, 
     :    siteLocation, isitecoordflag, 
     :    dl, dw, nl, nw,  
     :    fr1, qr1, s1, ft1, ft2,fr2, qr2, s2, c_q, fq,     
     :    r_ref,nsprd_segs,rlow,a_s,b_s,m_s,
     :    amag,
     :    site_lat_degrees,  site_lon_degrees,
     :    d_jb, d_cd2f, reff)

      r = reff
      rmod = r
      
      stressc = stress_in
      dlsdm = 0.0
!      numsource = 1    ! 12/22/09
      
      n_index = n_index + 1

* Arias intensity:

      iaorins = 1
      idva = 2
      call gm_rv(smsim_out)
      arias = arias_rv   ! must rename, because the variable arias_rv
                         ! is assigned a value for PerOsc runs, but in 
                         ! those cases it is not the true Arias intensity
                         ! (it is the zeroth moment of the oscillator response)

      if (perosc .lt. 0.0 ) then
* pgv:
        iaorins = 1
        idva = 1

        print *
        write(*,'(a,1x,f5.2,1x,f6.1,1x,f5.2, 1x,f7.2)') 
     :    '+ Compute pgv for per, r, m, stress = ', 
     :     perosc, r, amag, stress 
        call gm_rv(smsim_out)
        pgv = smsim_out
        write(nu_write_params, '(a, 1p5(1x,e10.3))') 
     :    ' perosc, amag, r, stress, kappa: ',
     :      perosc, amag, r, stress,kappa_f(amag)

      else if (perosc .eq. 0.0 ) then ! pga
* pga:
        iaorins = 1
        idva = 2

        write(*,*)
        write(*,'(a,1x,f5.2,1x,f6.1,1x,f5.2, 1x,f7.2)') 
     :    '+ Compute pga for per, r, m, stress = ', 
     :     perosc, r, amag, stress 
        call gm_rv(smsim_out)
        pga = smsim_out
        write(nu_write_params, '(a, 1p5(1x,e10.3))') 
     :    ' perosc, amag, r, stress, kappa: ',
     :      perosc, amag, r, stress,kappa_f(amag)
 
      else
* psa: (get pga by setting to 0.0 (above) or by specifying suitably 
*      small perosc... e.g., 0.001)
        iaorins = 2
        write(*,*)
        write(*,'(a,1x,f5.2,1x,f6.1,1x,f5.2,1x,f7.2)') 
     :    '+ Compute psa for per, r, m, stress = ', 
     :     perosc, r, amag, stress 
        call gm_rv(smsim_out)
        psv = smsim_out
        write(nu_write_params, '(a, 1p5(1x,e10.3))') 
     :    ' perosc, amag, r, stress, kappa: ',
     :      perosc, amag, r, stress,kappa_f(amag)
 
      end if

* Write to column file:
      if (perosc .lt. 0.0) then
        freqosc = -1.0
        psa = -999.0
        psag = -999.9           
        psv = pgv
        sd = -999.9
      else if (perosc .eq. 0.0) then
        freqosc = 999.9
        psa = pga
        psag = pga/981.0
        psv = -999.9
        sd = -999.9
      else
        freqosc = 1.0/perosc
        psa = (twopi/perosc)*psv     ! convert psv to psa 
        psag = psa/981.0
        psv = (perosc/twopi)*psa  
        sd = (perosc/twopi)*psv      
      end if 
     
      

        write(nu_out, '(
     :    1x,i5,
     :    1x,f4.2, 
     :    8x,i2, 1x,f6.1, 1x,f8.2, 1x,f8.2, 1x,f5.1, 1x,f5.2,
     :    3x,f9.3, 3x,f9.3,  
     :    4x,f9.3, 4x,f9.3,  
     :    14x,i1, 9x,l1,
     :    8x,f9.3, 8x,f9.3, 
     :    1x,f8.2, 1x,f8.2, 1x,f8.2, 1x,f8.2,
     :    1x,f5.2, 1x,f7.3, 1x,f7.3,
     :    1p, 1x,e10.3, 
     :        1x,e10.3, 
     :        1x,e10.3,
     :        1x,e10.3,
     :        1x,e10.3
     :                                         )') 
     :    n_index, 
     :    amag, 
     :    numsource, stress, faultlength, faultwidth, faultdip, h,
     :    siteloc1_in, siteloc2_in,
     :    siteLocation(1), siteLocation(2), 
     :    isitecoordflag, move_site,
     :    site_lat_degrees,  site_lon_degrees,
     :    d_jb, d_cd2f, reff, r,
     :    damp, perosc, freqosc, 
     :    psa,
     :       psag, 
     :        psv, 
     :         sd, 
     :      arias

      call DATE_AND_TIME( date, time_stop )
      call time_diff(time_start, time_stop, time_elapsed)
      write(nu_write_params, '(a,1x,1pe10.3)') 
     :     ' Elapsed time (sec): ', time_elapsed
      write(nu_write_params, *)          
 
* Loop back for another set of PerOsc, Amag, R:
      go to 888
999   continue

      call DATE_AND_TIME( date, time_end )
      write(nu_write_params, '(a)') 
     : ' Time End: '//
     : time_end(1:2)//':'//time_end(3:4)//':'//time_end(5:10)
      call time_diff(time_begin, time_end, time_elapsed)
      write(nu_write_params, '(a,1x,1pe10.3)') 
     :     ' Total elapsed time (sec): ', time_elapsed

      print *,' '
      print *, ' Elapsed time (sec): ', time_elapsed


      close(unit=nu_out)
      close(unit=nu_write_params)
      close(unit=nu_ctl)
      
      stop
      end
* ----------------- END TMRS_FF_RV_DRVR ---------------------------

!------------------- SUBPROGRAMS -----------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine findDistanceAndAzimuth (FaultLat,FaultLon,SiteLat,
     *                                 SiteLon,epi,azi,isitecoordflag)
c       calculates distance and azimuth between two
c       points using their latitudes and longitudes

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

* Dates: 02/17/09 - renamed some variables to avoid possible confusion
*                   between angles in degrees and radians.

* The input and output angles are in degrees

        pi = 4.0*atan(1.0)
        d2r = pi/180.0
        
        if (isitecoordflag .eq. 1) then  ! lat,long

          re=6371.
          alat=(FaultLat+SiteLat)/2.
          alat_radians=d2r*alat
          dlat=SiteLat-FaultLat  ! degrees
          dlon=SiteLon-FaultLon  ! degrees
          r1 = d2r*re*(SiteLat-FaultLat)
          epi= d2r*re*sqrt(dlat**2.+cos(alat_radians)**2.*dlon**2.)
          if(r1.ge.epi)then
             azi_radians=pi
          else
             azi_radians=acos(r1/epi)
          endif

          if (dlon.le.0.) azi_radians=2.0*pi-azi_radians
          azi= azi_radians/d2r

        else if (isitecoordflag .eq. 2) then  ! R,Az
        
          epi = sitelat
          azi = sitelon
          
        else  ! assume cartesian coordinates
        
          xn = sitelat
          xe = sitelon
          
          epi = sqrt(xn**2 + xe**2)
          
          azi = 180.0*atan2(xe, xn)/pi
          if (azi .lt. 0.0) then
            azi = 360.0 + azi
          end if
          
        end if
          
          

        return
        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        real function findSubfaultDistance (R,h,FaultStrike,
     *                  fi2,FaultDip,dl,dw,i,j)
c       computes distance subfaultDistance from center of subfault (i,j)
c       to observation point using formula (1) (Figure 1)

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

* Note confusion in what dip means in the figure 1 of Beresnev and Atkinson.
* What they label as "delta" is what was used in the original version of
* this subroutine as "dip".  Their figure has delta_1 as the true 
* fault dip, probably added as a result of a review. I have
* replaced "dip" by 90_faultdip here to eliminate confusion (D. Boore, 17Feb09).

        pi = 4.0*atan(1.0)
        d2r = pi/180.0
        
        a90_faultdip_radians = d2r*(90.0-FaultDip)
        phi2_strike_radians = d2r*(fi2-FaultStrike)
        t1=R*cos(phi2_strike_radians)-(2.*i-1)*dl/2.
        t2=R*sin(phi2_strike_radians)-(2.*j-1)*dw/2.*
     :       sin(a90_faultdip_radians)
        t3=-h-(2.*j-1)*dw/2.*cos(a90_faultdip_radians)
        findSubfaultDistance=sqrt(t1**2.+t2**2.+t3**2.)
        return
        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine getInputParameters(nu_ctl,
     :            FaultStrike,FaultDip,h,
     :            FaultLat,FaultLon, move_site,
     :            fault_type,
     :            FaultLength, FaultWidth, 
     :            dl, dw, 
     :            specify_length, specify_width, stress_ref, 
     :            fq,
     :            isitecoordflag,
     :            f_stem, nc_f_stem, f_smsim_params, nc_f_smsim_params)

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

      character f_stem*(*), f_smsim_params*(*), fault_type*(*)
      character date_ctl_correct*8, date_ctl_in*30
      character cmnts2skip(50)*80, buf_in*10
      
      logical f_exist, specify_length, specify_width,
     :        move_site
      
      pi = 4.0*atan(1.0)
      d2r = pi/180.0

      date_ctl_correct = ' '
      date_ctl_correct = '02/19/09'
      call trim_c(date_ctl_correct,nc_date_ctl_correct)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      date_ctl_in = ' '
      read(nu_ctl,'(a)') date_ctl_in
      call trim_c(date_ctl_in,nc_date_ctl_in)

      if (date_ctl_correct(1:nc_date_ctl_correct) .ne. 
     :    date_ctl_in(1:nc_date_ctl_in)) then
        write(*,*) 
     :     ' The control file has the wrong date; STOP!'
        write(*,*)
     :     ' The date in the control file is '//
     :      date_ctl_in(1:nc_date_ctl_in)
        write(*,*)
     :     ' The proper date is '//
     :      date_ctl_correct(1:nc_date_ctl_correct)
        close(nu_ctl)
        stop
      end if
     
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      f_smsim_params = ' '
      read (nu_ctl,'(a)') f_smsim_params
      call trim_c(f_smsim_params, nc_f_smsim_params)
  
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      f_stem = ' '
      read (nu_ctl,'(a)') f_stem
      call trim_c(f_stem, nc_f_stem)
  
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*) FaultLat, FaultLon
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*) FaultStrike, FaultDip, h
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      fault_type = ' '
      read(nu_ctl,'(a)') fault_type
      call trim_c(fault_type, nc_fault_type)
      call upstr(fault_type(1:1))
     
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*) FaultLength, FaultWidth, dl, dw, stress_ref
      
      if (faultlength .eq. 0.0) then
        specify_length = .false.
      else
        specify_length = .true.
      end if

      if (faultwidth .eq. 0.0) then
        specify_width = .false.
      else
        specify_width = .true.
      end if

* fq:
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl, *) fq
      
* Site info
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*) isitecoordflag
      print *, ' Type of coordinates (1=lat,long; 2=R,Az; 3=N,E):', 
     :           isitecoordflag
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      buf_in = ' '
      read (nu_ctl,'(a)') buf_in
      call upstr(buf_in)
      call trim_c(buf_in, nc_buf_in)
      if (buf_in(1:1) .eq. 'Y') then
         move_site = .true.
      else
         move_site = .false.
      end if
      
      if (move_site .and. FaultStrike .ne. 0.0) then
        print *,' ERROR: Cannot request move site if'//
     :          ' the fault strike .ne. 0.0; QUITTING'
        stop
      end if
      
      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

*----------------- BEGIN SET_LOCATION -----------------------------
      subroutine set_location(faultLength,move_site, isitecoordflag, 
     :                        siteLocation)
      
      logical move_site
      real sitelocation(2)
      
      pi = 4.0*atan(1.0)
      d2r = pi/180.0
      
      isitecoordflag_tmp  = isitecoordflag
      if (move_site .and. isitecoordflag_tmp .eq. 1 ) then
        dum = 0.0
          ! eventually might do something with this case
      else if (move_site .and. isitecoordflag_tmp .eq. 2 ) then 
        isitecoordflag = 3
        r = siteLocation(1)
        az = siteLocation(2)
        xn = r*cos(d2r*az)
        xe = r*sin(d2r*az)
        siteLocation(1) = faultLength/2.0 + xn
        siteLocation(2) = xe
      else if (move_site .and. isitecoordflag_tmp .eq. 3) then
          if (sitelocation(1) .eq. 0.0) then  ! move to midpoint
            siteLocation(1) = faultLength/2.0
          end if
          if (siteLocation(2) .eq. 0.0) then  ! move to end
            siteLocation(1) = faultLength + siteLocation(1)
          end if
      end if
 
      return
      end
*----------------- END SET_LOCATION -----------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine site_coords_in_degrees(
     :         FaultLat, FaultLon, 
     :         SiteLat, SiteLon, isitecoordflag, 
     :         site_lat_degrees, site_lon_degrees ) 
     
* Note: site_lat_degrees, site_lon_degrees are relative
* to the origin of the fault coordinates

        pi = 4.0*atan(1.0)
        dtor = pi/180.0
     
! If site coords not in degrees, convert      
        if (isitecoordflag .eq. 1) then  ! degrees
          site_lat_degrees = sitelat
          site_lon_degrees = sitelon         
        else if (isitecoordflag .eq. 2) then  ! R,Az
          r = sitelat
          az = sitelon
          xn = r*cos(dtor*az)
          xe = r*sin(dtor*az)
          call km2deg_f( xn, xe, FaultLat, FaultLon,  
     :            site_lat_degrees, site_lon_degrees )          
        else  ! assume cartesian coordinates
          xn = sitelat
          xe = sitelon
          call km2deg_f( xn, xe, FaultLat, FaultLon,  
     :            site_lat_degrees, site_lon_degrees )          
        end if          
          
          
      
        return
        end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine writePar (ioPar,
     :   f_smsim_params, nc_f_smsim_params, 
     :   FaultStrike,FaultDip,h,
     :   FaultLat, FaultLon,  
     :   move_site,
     :   fault_type,
     :   FaultLength, FaultWidth, dl, dw,
     :   specify_length, specify_width, stress_ref,
     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s,
     :   fq)
     
!     :   qr1, s1, ft1, ft2, fr2, qr2, s2, c_q, fq, 
!     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s)
 
c     writes modeling parameters to specified ascii file

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

! Dates: 01/30/14 - Removed Q params from input arguments and no longer write them to
!                   ioPar.  But do write fq, as it comes from the tmrs_ff_rv_drvr control
!                   file and not from the smsim params file (so it will not be written
!                   to the summary file by write_params).
!                   

      logical specify_length, specify_width, move_site
      
      character fault_type*(*), f_smsim_params*(*)
      
 

      real amag, r_ref, rlow(*), a_s(*), b_s(*), m_s(*)
       
      write(ioPar,'("         modeling parameters     ")' )
      write(ioPar,'(a)') ' File with smsim parameters: '//
     :                   f_smsim_params(1:nc_f_smsim_params)
      write(ioPar,'("Fault Strike              = ",f8.2)')FaultStrike
      write(ioPar,'("Fault dip                 = ",f8.2)')FaultDip
      write(ioPar,'("Fault depth to upper edge = ",f8.2)')h
      
      write(ioPar,'(a,1x,l1)') ' move_site = ', move_site 

      write(ioPar,'(a,1x,l1)') ' specify_length = ', specify_length 

      if(.not. specify_length) then
        write(ioPar,'(a,1x, f6.1)') 
     :    ' Fault length from Wells and Coppersmith for fault type '//
     :      fault_type(1:1)//', using a reference stress of ', 
     :                             stress_ref
      end if     
      write(ioPar,'("Fault Length  in            = ",f8.2)')FaultLength
      
      write(ioPar,'(a,1x,l1)') ' specify_width = ', specify_width 

      if(.not. specify_width) then
        write(ioPar,'(a,1x, f6.1)') 
     :    ' Fault width from Wells and Coppersmith for fault type '//
     :      fault_type(1:1)//', using a reference stress of ', 
     :                             stress_ref
      end if     
      write(ioPar,'("Fault Width   in            = ",f8.2)')FaultWidth
      
      write(ioPar,'("FaultLat                  = ",f8.2)')FaultLat
      write(ioPar,'("FaultLon                  = ",f8.2)')FaultLon

      write(ioPar,'("subfault length in          = ",f8.2)')dl
      write(ioPar,'("subfault width in           = ",f8.2)')dw
      write(ioPar,'("-----------------------------------------------")')
!      write(ioPar,'(a)') ' qr1, s1, ft1, ft2, fr2, qr2, s2, c_q = '
!      write(ioPar,*) qr1, s1, ft1, ft2, fr2, qr2, s2, c_q 
      write(ioPar,'(a)') ' fq = '
      write(ioPar,*) fq 
      write(ioPar,'("-----------------------------------------------")')

      write(ioPar,'(a)') ' i, nsprd_segs, r_ref, rlow, a_s, b_s, m_s'
      do i = 1, nsprd_segs
        write(ioPar,*) 
     :    i, nsprd_segs, r_ref, rlow(i), a_s(i), b_s(i), m_s(i)
      end do


      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

* --------------- BEGIN COMPUTE_DISTANCES ---------------------------------
      subroutine compute_distances(
     :  FaultLat, FaultLon, 
     :  FaultLength, FaultWidth, 
     :  FaultStrike, FaultDip, h, 
     :  SiteCoords, isitecoordflag, 
     :  dl, dw, nl, nw,  
     :  fr1, qr1, s1, ft1, ft2,
     :  fr2, qr2, s2, c_q, fq,     
     :  r_ref,nsprd_segs,rlow,a_s,b_s,m_s,
     :  amag,
     :  site_lat_degrees,  site_lon_degrees,
     :  d_jb, d_cd2f, reff)
     
* Dates: 12/22/09 - Removed constraint that numsource = 1     
!        04/08/11 - I modified gsprd_f to remove the dependence on numsource
      
      real amag, r_ref, rlow(10), a_s(10), b_s(10), m_s(10)
      real sitecoords(*), 
     :     subfaultDistance(200,200), gsprdq(200,200)
      
 
      external gsprd_q_avg_f
      
* Initialize q_f and compute q for f=fq

      dummy = q_f_setup(fr1, qr1, s1, ft1, ft2,
     :                  fr2, qr2, s2, c_q)

* Specify fq, then compute q for this fq.  For no q, let fq = 0.0
* (in this case, q = 9999.0)
* Then write r gsprd(1/q=0) gsprd(q)

      q_fq = q_f(fq)

      inv_q = 1.0/q_fq

* Initialize gsprd_q_f:

!      numsource = 1  ! 12/22/09
      dummy =  gsprd_q_f_setup(r_ref,nsprd_segs,rlow,
     :                     a_s, b_s, m_s,
     :                     amag,
     :                     q_fq, c_q, fq)
     
     
     
* Compute some distances: 

      sitelat = sitecoords(1)
      sitelon = sitecoords(2)

      call site_coords_in_degrees(
     :         FaultLat, FaultLon, 
     :         SiteLat, SiteLon, isitecoordflag, 
     :         site_lat_degrees, site_lon_degrees ) 

        h_min_c = 3.0 ! Campbell depth to seismogenic region
        fw1 = 0.0
        fw2 = faultwidth
        fl1 = 0.0
        fl2 = faultlength
     
        call dist_3df(
     :   site_lat_degrees, site_lon_degrees, 
     :   FaultLat, FaultLon, h, FaultStrike, FaultDip,
     :   fw1, fw2, fl1, fl2, 
     :   h_min_c, d_jb, az_jb, d_cd2f, az_cd2f, d_c, az_c,
     :   d_sta_n, d_sta_e, irgn_cd2f, irgn_c, irgn_jb)
 
        fi2=0
        call findDistanceAndAzimuth(FaultLat,FaultLon,SiteLat,
     :                           SiteLon,R,fi2,isitecoordflag)
c     Note that R, azm fi2 are the distance, azm. w.r.t. origin (not epi),
c     Note also I must input faultstrike in degrees and it's converted to rad.
 
        cum_gsprdq = 0.0  ! need this for computation of reff

        nsubfaults = 0
          
        DO i=1,nl
          DO j=1,nw
            
            nsubfaults = nsubfaults + 1
              
            subfaultDistance(i,j)=
     :                 findSubfaultDistance(R,h,FaultStrike,
     :                           fi2,FaultDip,dl,dw,i,j)
     
* Compute gsprd*q for this subfault and station:
            gsprdq(i,j) = gsprd_q_f(subfaultDistance(i,j))


            cum_gsprdq = cum_gsprdq + gsprdq(i,j)**2.0
 
          END DO ! loop over nw to calculate quantities not dependent on nsims
        END DO ! loop over nl to calculate quantities not dependent on nsims

        gsprdq_avg = sqrt(cum_gsprdq/float(nsubfaults))

        dummy =  gsprd_q_avg_f_setup(r_ref, nsprd_segs, rlow,
     :                     a_s, b_s, m_s,
     :                     amag,
     :                     q_fq, c_q, fq,
     :                     gsprdq_avg)     
        x1 = 1.0
        x2 = 2000.0
        tol = 0.001
        reff = zbrent(gsprd_q_avg_f,x1,x2,tol)

        dummy =  gsprd_q_avg_f_deallocate() 

        dummy =  gsprd_q_f_deallocate() 
        
        return
        end
* --------------- END COMPUTE_DISTANCES ---------------------------------

*----------------- BEGIN GET_FAULT_DIMENSIONS -----------------------------
      subroutine get_fault_dimensions(
     :    fault_type, amag, stress, stress_ref,
     :    dl, dw, nl, nw, nsubs,
     :    specify_length, faultlength, 
     :    specify_width, faultwidth) 

      character fault_type*1
      
      logical specify_length, specify_width
      
      stress_factor = (stress_ref/stress)**(1.0/3.0)
 
      if (.not. specify_length) then
        if(fault_type(1:1) .eq. 'S') then
          faultlength = 10.0**(-2.57+0.62*amag)
        else if(fault_type(1:1) .eq. 'R') then
          faultlength = 10.0**(-2.42+0.58*amag)
        else if(fault_type(1:1) .eq. 'N') then
          faultlength = 10.0**(-1.88+0.50*amag)
        else
          faultlength = 10.0**(-2.44+0.59*amag)
        end if
        
        faultlength = stress_factor * faultlength
        
      end if
      if (.not. specify_width) then
        specify_width  = .false.
        if(fault_type(1:1) .eq. 'S') then
          faultwidth = 10.0**(-0.76+0.27*amag)
        else if(fault_type(1:1) .eq. 'R') then
          faultwidth = 10.0**(-1.61+0.41*amag)
        else if(fault_type(1:1) .eq. 'N') then
          faultwidth = 10.0**(-1.14+0.35*amag)
        else
          faultwidth = 10.0**(-1.01+0.32*amag)
        end if
        
        faultwidth = stress_factor * faultwidth
       
      end if
      
      nl=FaultLength/dl
      if (nl .lt. 1) nl = 1
      nw=FaultWidth/dw
      if (nw .lt. 1) nw = 1
 
      nsubs = nl * nw
        
      dl = FaultLength/float(nl)  ! Need to reset dl and dw because nl and nw are integers      
      dw = FaultWidth/float(nw)
      
      return
      end
*----------------- END GET_FAULT_DIMENSIONS -----------------------------



!      include 'rv_subs.for'
!      include 'rv_td_subs.for'
!      include 'recipes.for'
      include 'smsim_subs_for_rv_programs.for'
      include 'smsim_util_subs.for'
      include 'ff_subprograms.for'
      

      
