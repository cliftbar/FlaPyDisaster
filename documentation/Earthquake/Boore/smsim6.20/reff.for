      program Compute_Reff
 
*!Control file for program reff
*! Revision of program involving a change in the control file on this date:
*   02/17/09
*!MW, Stress 
*  7.0 140.0       	
*!lat and lon of upper edge of fault
*  0.0 0.0  
*!strike,dip, depth of fault
*  0.0 90.0 0.0            	
*!fault type (S=strikeslip; R=reverse; N=normal; U=undifferentiated) 
*! (Only used if Wells and Coppersmith is used to obtain FL and FW).
*  R                               
*!fault length and width, dl, dw, stress_ref
*!Note: Force program to use Wells and Coppersmith for FL and/or FW if either entry = 0.0
*  0.0 0.0 1.5 1.5 70.0 ! used for WC scaling (need an entry as a placeholder even if not used_
*!gsprd: r_ref, nsegs, (rlow(i), a_s, b_s, m_s(i))  (Usually set r_ref = 1.0 km)
*    1.0
*    3
*      1.0 -1.3 0.0 6.5
*     70.0 +0.2 0.0 6.5
*    140.0 -0.5 0.0 6.5
*!q: fr1, Qr1, s1, ft1, ft2, fr2, qr2, s2, c_q
*   1.0 1000.0 0.0 1.4242 1.4242 1.0 893.0 0.32 3.7   
*!fq
*   10.0
*!Output file names stem
*  m7.0_fl_fw_stress_140_test_reff
* !Number of Sites, coord flag (1=lat,long; 2=R,Az; 3=N,E)
*  2 2                      	
*!Coordinates of each site
*  2.5   0.0                      
*  2.5   90.0                       
*!Move the sites to the midpoint and end of the surface projection 
*!of the upper edge of the fault if isitecoordflag = 3 and 
*!siteLocation(1) = 0 (center) and siteLocation(2) = 0 (end), respectively
*!(this only makes sense if the strike of the fault = 0.0)?
* Y
 

* Dates: 02/15/09 - Written by D. Boore, patterned after Reff_DurEff.
*                   This is a bit of a kludge, with some code related
*                   to the envelope and duration calculation remaining.  The
*                   main thing I did was to skip writing most of the output files.
*        02/16/09 - Simplified input and put as much of the reff stuff as possible into
*                   a subprogram.
*        02/17/09 - Read in dl, dw rather than nl, nw (which need to be changed for
*                   each M).
*                 - Revised to eliminate possible confusion of dip (actually 90-Faultdip),
*                   angles in degrees vs radians.  
*                 - Read in dl, dw rather than nl, nw (which need to be changed for
*                   each M).
*                 - Collect as much of the code as possible into a subprogram
!        04/08/11 - Removed using AS00 deff to compute rmod, because the 
!                   application is for small size faults for which the
!                   finite-fault effect approximated by deff is not relevant
!                   (of course, amag will be small, so this would reduce 
!                   the impact of using deff).
!                 - Removed numsource from argument list.


 
      dimension siteLocation(300,2)
      character f_stem*120
      character fpar*120, f_dist*120
      

      logical f_exist, specify_length, specify_width, move_site
 
      character fault_type*10, InputFileName*80

      real amag, r_ref, rlow(10), a_s(10), b_s(10), m_s(10)
 
      nu_ctl = 99
      ioPar = nu_ctl - 1
      nu_dist = nu_ctl - 20
      

      pi=4.0*atan(1.0)
      twopi = 2.0 * pi
      d2r = pi/180.0

      f_exist = .false.
      do while (.not. f_exist)
        InputFileName = ' '
        write(*, '(a\)') 
     :    ' Enter name of input parameter file '//
     :    '(cr = reff.ctl): '
        read(*, '(a)') InputFileName        
        if (InputFileName(1:4) .eq. '    ') 
     :                         InputFileName = 'reff.ctl'
        call trim_c(InputFileName, nc_f_in)
        inquire(file=InputFileName(1:nc_f_in), exist=f_exist)
        if (.not. f_exist) then
          write(*,'(a)') ' ******* FILE DOES NOT EXIST ******* '
        end if
      end do
      open(unit=nu_ctl, file=InputFileName(1:nc_f_in), status='unknown')

      call getInputParameters(nu_ctl,
     :            FaultStrike,FaultDip,h,
     :            FaultLat,FaultLon, move_site,
     :            siteLocation,numberOfSites,isitecoordflag,
     :            fault_type,
     :            FaultLength, FaultWidth, 
     :            dl, dw, nl, nw, nsubs, 
     :            specify_length, specify_width, stress_ref, 
     :            amag, stress, 
     :            fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q,
     :            fq,
     :            r_ref, nsprd_segs, rlow, a_s, b_s, m_s,
     :            f_stem)

      close(nu_ctl)
     
      call trim_c(f_stem, nc_f_stem)
      
      fpar = ' '
      fpar = f_stem(1:nc_f_stem)//'_parameters.out'
      call trim_c(fpar, nc_fpar)
      open (ioPar,file=fpar(1:nc_fpar),status='unknown')
 
      call writePar (ioPar,
     :   FaultStrike,FaultDip,h,
     :   FaultLat, FaultLon,  
     :   siteLocation, numberOfSites, move_site,
     :   fault_type,
     :   FaultLength, FaultWidth, nl, nw, dl, dw,
     :   specify_length, specify_width, stress_ref,
     :   amag, stress, 
     :   qr1, s1, ft1, ft2, fr2, qr2, s2, c_q, fq,
     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s)
      
      close(ioPar) 

      f_dist = ' '
      f_dist = f_stem(1:nc_f_stem)//'_distances.out'
      call trim_c(f_dist, nc_f_dist)
      open(nu_dist, file=f_dist(1:nc_f_dist), status='unknown')
    
      write(nu_dist,'(
     :                4x,a, 1x,a, 7x,a, 7x,a, 3x,a, 5x,a,
     :                2x,a, 2x,a,
     :                1x,a, 
     :                1x,a, 1x,a, 1x,a, 
     :                1x,a, 1x,a, 
     :                5x,a, 3x,a, 5x,a)')
     :  'm', 'stress', 'fl', 'fw', 'dip', 'h',
     :  'nl', 'nw',
     :  'isite',
     :  'sitecoord(1)', 'sitecoord(2)', 'isitecoordflag',
     :  'site_lat_degrees', 'site_lon_degrees', 
     :  'd_jb', 'd_cd2f', 'reff'
      


      DO isite=1,numberOfSites   ! Loop on Sites around the Fault

      
        print *, "Working on Site #",isite,
     :       siteLocation(isite,1), siteLocation(isite,2)
        
        call compute_distances(
     :    FaultLat, FaultLon, 
     :    FaultLength, FaultWidth, 
     :    FaultStrike, FaultDip, h, 
     :    siteLocation(isite,1), siteLocation(isite,2), isitecoordflag, 
     :    dl, dw, nl, nw,  
     :    fr1, qr1, s1, ft1, ft2,fr2, qr2, s2, c_q, fq,     
     :    r_ref,nsprd_segs,rlow,a_s,b_s,m_s,
     :    amag,
     :    site_lat_degrees,  site_lon_degrees,
     :    d_jb, d_cd2f, reff)

        write(nu_dist, '(
     :    1x,f4.2, 1x,f6.1, 1x,f8.2, 1x,f8.2, 1x,f5.1, 1x,f5.2,
     :    1x,i3, 1x,i3,
     :    1x,i5,
     :    4x,f9.3, 4x,f9.3, 14x,i1,
     :    8x,f9.3, 8x,f9.3, 
     :    1x,f8.2, 1x,f8.2, 1x,f8.2)') 
     :    amag, stress, faultlength, faultwidth, faultdip, h,
     :    nl, nw,
     :    isite, 
     :    siteLocation(isite,1), siteLocation(isite,2), isitecoordflag, 
     :    site_lat_degrees,  site_lon_degrees,
     :     d_jb, d_cd2f, reff


      END DO  ! End loop over sites

       
      close(nu_dist)
 
 
      STOP
      END
cccccccccccccccccccccccc   End of Main Program    cccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!------------------- SUBPROGRAMS -----------------------------------------

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        subroutine findDistanceAndAzimuth (FaultLat,FaultLon,SiteLat,
     *                                 SiteLon,epi,azi,isitecoordflag)
c       calculates distance and azimuth between two
c       points using their latitudes and longitudes

!As called in compute_distances.for:
!        fi2=0
!        call findDistanceAndAzimuth(FaultLat,FaultLon,SiteLat,
!     :                           SiteLon,R,fi2,isitecoordflag)


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

* Note confusion in what dip means in fault1.ps, included in the FINSIM
! package distibuted by Igor Beresnev (my version is dated late 1990s, early 2000s).
* What is labeled as "delta" in that figure was referred to in FINSIM (from which
! this subroutine was apparently derived by Motazedian in writing EXSIM)
* as "dip".  The FINSIM figure has delta_1 as the true 
* fault dip (also this is explicitly stated in the program comments). I have
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
     :            siteLocation,numberOfSites,isitecoordflag,
     :            fault_type,
     :            FaultLength, FaultWidth, 
     :            dl, dw, nl, nw, nsubs, 
     :            specify_length, specify_width, stress_ref, 
     :            amag, stress, 
     :            fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q,
     :            fq,
     :            r_ref, nsprd_segs, rlow, a_s, b_s, m_s,
     :            f_stem)

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

* Dates: 11/30/08 - Move computation of random slipweights 
*                   if Islipweight .eq. 1.0 from main program to here
*                   (a more logical place).
*        12/02/08 - Get i_rise_time, to determine what type of risetime is used 
     
      real rlow(*), a_s(*), b_s(*), m_s(*)
      dimension siteLocation(300,2)
      character f_stem*(*), fault_type*(*),
     :          version_ctl*8, version_in*30
      character cmnts2skip(50)*80, buf_in*10
      
      logical f_exist, specify_length, specify_width,
     :        move_site
      
      pi = 4.0*atan(1.0)
      d2r = pi/180.0

      version_ctl = ' '
      version_ctl = '02/17/09'
      call trim_c(version_ctl,nc_version_ctl)

      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      version_in = ' '
      read(nu_ctl,'(a)') version_in
      call trim_c(version_in,nc_version_in)

      if (version_ctl(1:nc_version_ctl) .ne. 
     :    version_in(1:nc_version_in)) then
        write(*,'(a)') 
     :     ' The control file has the wrong version number; STOP!'
        close(nu_ctl)
        stop
      end if
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*) amag,stress 
       
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
      
      stress_factor = (stress_ref/stress)**(1.0/3.0)
 
      specify_length = .true.
      specify_width  = .true.


      if (faultlength .eq. 0.0) then
        specify_length = .false.
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
      if (faultwidth .eq. 0.0) then
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

* gsprd: 
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl, *) r_ref
      read(nu_ctl, *) nsprd_segs
      do i = 1, nsprd_segs
        read(nu_ctl, *) rlow(i), a_s(i), b_s(i), m_s(i)
      end do
 
* Q:
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl, *) fr1, qr1, s1, ft1, ft2, fr2, qr2, s2, c_q
      
* fq:
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read(nu_ctl, *) fq
      
     
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      f_stem = ' '
      read (nu_ctl,'(a)') f_stem
      call trim_c(f_stem, nc_f_stem)
  
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      read (nu_ctl,*)numberOfSites, isitecoordflag
      print *, ' Type of coordinates (1=lat,long; 2=R,Az; 3=N,E):', 
     :           isitecoordflag
      
      call skipcmnt(nu_ctl,cmnts2skip,nc_cmnts2skip)
      do i=1,numberOfSites
            read (nu_ctl,*)siteLocation(i,1),siteLocation(i,2)
            print *,' SiteLat,SiteLon for Site #',
     :               i,' = ', siteLocation(i,1),siteLocation(i,2)
      enddo

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


      isitecoordflag_in = isitecoordflag
      Do i=1,numberOfSites
        if (move_site .and. isitecoordflag_in .eq. 1 ) then
          dum = 0.0
          ! eventually might do something with this case
        else if (move_site .and. isitecoordflag_in .eq. 2 ) then 
          isitecoordflag = 3
          r = siteLocation(i,1)
          az = siteLocation(i,2)
          xn = r*cos(d2r*az)
          xe = r*sin(d2r*az)
          siteLocation(i,1) = faultLength/2.0 + xn
          siteLocation(i,2) = xe
        else if (move_site .and. isitecoordflag_in .eq. 3) then
          if (sitelocation(i,1) .eq. 0.0) then  ! move to midpoint
            siteLocation(i,1) = faultLength/2.0
          end if
          if (siteLocation(i,2) .eq. 0.0) then  ! move to end
            siteLocation(i,1) = faultLength + siteLocation(i,1)
          end if
        end if
      enddo

      return
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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
     :   FaultStrike,FaultDip,h,
     :   FaultLat, FaultLon,  
     :   siteLocation, numberOfSites, move_site,
     :   fault_type,
     :   FaultLength, FaultWidth, nl, nw, dl, dw,
     :   specify_length, specify_width, stress_ref,
     :   amag, stress, 
     :   qr1, s1, ft1, ft2, fr2, qr2, s2, c_q, fq, 
     :   r_ref, nsprd_segs, rlow, a_s, b_s, m_s)
 
c     writes modeling parameters to specified ascii file

* Modified from a subroutine in Motazedian and Atkinson's 
* EXSIM (release version 1.0, October 10, 2005)

      logical specify_length, specify_width, move_site
      
      character fault_type*(*)
      
      dimension siteLocation(300,2)


      real amag, r_ref, rlow(*), a_s(*), b_s(*), m_s(*)
       
      write(ioPar,'("         modeling parameters     ")' )
      write(ioPar,'("Fault Strike              = ",f8.2)')FaultStrike
      write(ioPar,'("Fault dip                 = ",f8.2)')FaultDip
      write(ioPar,'("Fault depth to upper edge = ",f8.2)')h
      
      if(.not. specify_length) then
        write(ioPar,'(a,1x, f6.1)') 
     :    ' Fault length from Wells and Coppersmith for fault type '//
     :      fault_type(1:1)//', using a reference stress of ', 
     :                             stress_ref
      end if     
      write(ioPar,'("Fault Length              = ",f8.2)')FaultLength
      
      if(.not. specify_width) then
        write(ioPar,'(a,1x, f6.1)') 
     :    ' Fault width from Wells and Coppersmith for fault type '//
     :      fault_type(1:1)//', using a reference stress of ', 
     :                             stress_ref
      end if     
      write(ioPar,'("Fault Width               = ",f8.2)')FaultWidth
      
      write(ioPar,'("FaultLat                  = ",f8.2)')FaultLat
      write(ioPar,'("FaultLon                  = ",f8.2)')FaultLon

      do i = 1, numberOfSites
        write(ioPar,'(a, 1x,i3, 1x,a, 1x,f8.2, 1x, f8.2)')
     :   'For site', i, 'siteLocation coordinates 1&2 = ', 
     :                siteLocation(i,1),  siteLocation(i,2)
      end do
      if (move_site) then
        write(ioPar,'(a)')
     : 'Site was moved to midpoint or end of surface projection '//
     : 'of upper edge of fault' 
      end if

      write(ioPar,'("No.of subs along strike   = ",i8)')nl
      write(ioPar,'("No.of subs along dip      = ",i8)')nw
      write(ioPar,'("subfault length           = ",f8.2)')dl
      write(ioPar,'("subfault width            = ",f8.2)')dw
      write(ioPar,'("-----------------------------------------------")')
      write(ioPar,'("Mag.                      = ",f8.2)')amag
      write(ioPar,'("-----------------------------------------------")')
      write(ioPar,'("stress parameter (bars)   = ",f8.2)')stress
      write(ioPar,'("-----------------------------------------------")')
      write(ioPar,'(a)') ' qr1, s1, ft1, ft2, fr2, qr2, s2, c_q = '
      write(ioPar,*) qr1, s1, ft1, ft2, fr2, qr2, s2, c_q 
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
     :  SiteLat, SiteLon, isitecoordflag, 
     :  dl, dw, nl, nw,  
     :  fr1, qr1, s1, ft1, ft2,
     :  fr2, qr2, s2, c_q, fq,     
     :  r_ref,nsprd_segs,rlow,a_s,b_s,m_s,
     :  amag,
     :  site_lat_degrees,  site_lon_degrees,
     :  d_jb, d_cd2f, reff)
      
!        04/08/11 - Removed using AS00 deff to compute rmod, because the 
!                   application is for small size faults for which the
!                   finite-fault effect approximated by deff is not relevant
!                   (of course, amag will be small, so this would reduce 
!                   the impact of using deff).
!                 - Removed numsource from argument list.

      real amag, r_ref, rlow(10), a_s(10), b_s(10), m_s(10)
      real subfaultDistance(200,200), gsprdq(200,200)
      
 
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

      dummy =  gsprd_q_f_setup(r_ref,nsprd_segs,rlow,
     :                     a_s, b_s, m_s,
     :                     amag,
     :                     q_fq, c_q, fq)
     
     
     
* Compute some distances: 

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

        include 'reff_included_subprograms.for'
        
 


  