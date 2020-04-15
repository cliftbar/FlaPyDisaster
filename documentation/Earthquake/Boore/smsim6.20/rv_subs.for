

!NOTE:  On 06/23/14 I started to create a batch file that would make rv_subs, adding
! the necessary subroutines (as I do for td_subs.for).  But on reflection, I realized 
! that this would not be convenient for changing the routines in the future.  
! I should only extract those subroutines that might be used by other programs.  For this
! reason I extracted the cl68 subroutines, bringing them in via include statements

! See 12/08/14 note below

! Note that rv_td_subs is NOT created using a batch file.  td_subs contains a number of files from
! \forprogs, and since these are probably used by other programs, it makes sense to use a batch
! file to create this file. The batch file is called when compilng the td drivers.


!----------------- BEGIN gm_rv -----------------------------
      subroutine gm_rv(gmsim)

! Compute various measures of ground motion, as specified from the
! variables iaorins and idva (passed through a common block specified
! in smsim.fi), using random-vibration stochastic model

! Note that gmsim is the dk80 value.  To use other values in programs that call gm_rv,
! the values passed through the rv common block in smsim.fi should be used.

! Dates: 06/07/95 - Written by D.M. Boore
!        06/08/95 - Pass amag, r through common block in smsim.fi
!        11/14/95 - Combined smsim.fi and rv.fi
!        12/14/95 - Pass freq20, durex through common block in smsim.fi
!        05/10/95 - Written by D. M. Boore, based on RVIB
!        07/28/96 - Added path to include statements
!        03/05/99 - Removed include rvtdsubs, recipes statements at end (they 
!                   must be placed in the driver programs) 
!        07/02/99 - Added arguments to durpath to incorporate Atkinson and 
!                   Silva (1999) magnitude-dependent modification to distance.
!        01/27/02 - Reset value of zup if needed in cl68_numrcl_int
!        06/13/02 - Changed name from "smsim_rv" to "gm_rv".
!        02/05/03 - Changed units of g from 980 cm/s/s to 981 cm/s/s.
!        02/09/03 - Removed pi, twopi computation (now in smsim.fi)
!        05/11/07 - Removed "\smsim\" from include statements
!        03/20/09 - iaorins = 3 for the WWSSN short period instrument (PerOsc .eq. -2.0, so
!                   cannot use fosc = 1/perosc)
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        06/27/11 - Changed name of bt11osc.fi file
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.
!        09/20/11 - Apply BT12 TD/RV factors for PGA and PGV 
!        12/08/14 - Try once again to replace this and the included subroutines in one file, made using
!                   make_smsim_subs_for_rv_programs.bat.  This will make it much easier to distribute
!                   the program.
!        11/14/15 - Use the DK80 values for gmsim

      include 'bt12osc.fi'

      include 'smsim.fi'

! Set spectral parameters:
      call spect_scale()   ! Be sure to call Get_Params in driver

! Get frequency-independent factor:
      call const_am0_gsprd()  ! rmod is used in this call; see rv_td_subs
!DEBUG
!      write(*,*) ' In gm_rv, after const_am0_gsprd():'//
!     :             'freq_indep_factor, rmod = ', freq_indep_factor, rmod
!DEBUG
! Set parameters related to type of output:
!
! iaorins == 1:  ground motion (acc, vel, or dis, as specified by idva)
!    idva == 0: displacement
!    idva == 1: velocity
!    idva == 2: acceleration
! iaorins == 2:  sdof harmonic oscillator
! iaorins == 3:  WWSSN-SP, using spins

      fosc = 1.0
      if (iaorins == 2) then
        fosc = 1.0/perosc
      else if (iaorins == 3) then  ! WWSSN-SP, using spins
        pp = 1.0
        pg = 0.75
        fosc = 0.5*(1.0/pp+1.0/pg)  ! I'm not sure what value to use for fosc
! DEBUG
!      print *,' in gm_rv: iaorins, fosc = ', iaorins, fosc
! DEBUG
      end if
      
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
! Assume that instrument response is relative to ground
! displacement.  In this case idva = 0, and to make sure
! that this is so, I include the following statement:

      if (iaorins > 1) idva = 0
!
!^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      call get_motion(gmsim)  

! DEBUG
!      print *,' in gm_rv: iaorins, gmsim = ', iaorins, gmsim
! DEBUG
      return
      end
!----------------- END gm_rv -----------------------------

!----------------- BEGIN GET_MOTION -----------------------------
      subroutine get_motion(gmsim)

! Dates: 06/06/95 - written by D.M. Boore, adapted from main.for, which see for
!                   code for printing out much more info.
!        06/07/95 - Changed to two term asymptotic expression for output.  The
!                   'exact' causes problems.
!        06/08/95 - Changed anz to have a minimum value of 1.33 (as mentioned
!                   by Toro in section 3, vol. 3, appendices B & C, Seismic
!                   Hazard Methodology for Nuclear Facilities in the Eastern
!                   United States, EPRI, April 30, 1985, eq. B-35, p. B-80.)
!        06/08/95 - Changed names of variables such as 'anclee' to more
!                   meaningful names, and also put the sqrt 2 into the proper
!                   place rather than carrying it around with rms (in 
!                   'afact' in subroutine main on the VAX).
!        06/09/95 - Changed method of computing 'exact' solution to a numerical
!                   integration.
!        10/17/95 - Eliminated exact series evaluation, as well as the switch
!                   from asymptotic to "exact"; I also eliminated the commented
!                   statements that included Toro's "clumping" correction...
!                   the source code for that has been retained in smsmclmp.for.
!        12/14/95 - Pass freq20, durex through common block in smsim.fi
!        12/19/95 - Added zup to exact_numrcl (as of 1/3/96, cl68_numrcl_int)
!        12/25/95 - Add Herrmann's integration of C&L-H eq. 6.4
!        12/26/95 - Pass eps_rv and ane through smsim.fi common rather than
!                   through the parameter list
!        12/28/95 - Changed variable names to indicate that equation 6.8 of
!                   Cartwright and Longuet-Higgins is being used.
!        12/30/95 - Remove Herrmann's integration of C&L-H eq. 6.4
!        01/14/99 - Add calculation of Arias intensity
!        01/17/99 - Added a variable "osc_crrctn" that controls the way that
!                   duration is calculated in computing response spectra:
!                   osc_crrctn = 0: original (Boore, 1983); no correction; 
!                                   no longer used (supplanted by 
!                                   osc_crrctn = 1), included here for
!                                   completeness
!                   osc_crrctn = 1: Boore and Joyner (1984); used up to 
!                                   now in smsim
!                   osc_crrctn = 2: Liu&Pezeshk (1996)'s empirical model in
!                                   which L. J. Liu by replaced alpha with 
!                                   the spectral shape factor k and n = 2 
!                                   instead n= 3 in Boore and Joyner (1984)
!        03/13/99 - Rearranged "if then" statements for osc_crrctn
!                   such that osc_crrctn appears in the order 0, 1, 2
!        02/05/00 - Used Chuck Mueller's suggestion to change code in 
!                   cl68_integrand to avoid possible numerical problems.
!        08/14/01 - Prompted by Bob Herrmann's suggestion, I changed the code
!                   dealing with small values of arg, in order to eliminate 
!                   sqrt of a negative number.   I probably should use a 
!                   double precision version of odeint (called by amom_rv), but
!                   the results are not too sensitive to the exact value of arg
!                   when it is small (as it is for a very narrow band response).
!                   The variables deltay and eps_rv are affected by the
!                   sqrt(arg) code; eps_rv and deltay is used only in the
!                   Liu & Pezeshk oscillator correction.  According to Bob 
!                   Herrmann, by eliminating the sqrt .le. 0.0 problem, a larger
!                   eps (e.g., 0.001 rather than 0.00001) can be used with 
!                   little loss of accuracy and a significant increase in 
!                   computational speed.
!        01/27/02 - Reset value of zup if needed in cl68_numrcl_int
!        02/09/03 - Place declarations before smsim.fi
!        03/20/09 - iaorins = 3 for the WWSSN short period instrument (PerOsc .eq. -2.0, so
!                   cannot use fosc = 1/perosc); apply oscillator correction now for any
!                   iaorins >= 2, but think about this in the future---it could be that
!                   I might not want to use the correction for other responses. 
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        12/04/09 - Save calculation of durpath and dursource so can pass
!                   them through a revised smsim.fi and write them if desired.
!        12/17/09 - Bring in rmod as a calling argument and use in call to durpath.
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        01/18/11 - Include Der Kiureghian and Asfura variables.
!        01/21/11 - Introduce a variable dur4n for computing the number of zero crossings
!                   and extrema so that I can experiment with setting it to durex and to trms.
!        02/06/11 - Add comment about amom_rv for an oscillator (to clear up
!                   possible confusion about what is being computed---when I
!                   first wrote the program PSV seemed to be the intensity measure
!                   of interest, but thathas now shifted to PSA and SD).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.
!        06/07/11 - Slight changes in code (trms region) for clarity
!        06/19/11 - Incorporate the Thompson-Boore modifications to the BJ84 oscillator correction
!                   for computing Trms.  The type of correction is specified in the params file, as follows:
!                   osc_crrctn(0=no correction;1=b&j;2=l&p; 3=Thompson&Boore WNA; 4=Thompson&Boore ENA)
!                 - Trap for sqrt(<0) in computation of pk_rms_dk and replace quantities with -9.99 in this case
!        06/27/11 - Changed name of bt12osc.fi file
!        07/02/11 - Allow for use of BT12 ena, wna, or both oscillator corrections
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.
!        09/20/11 - Apply BT12 TD/RV factors for PGA and PGV 
!        08/07/13 - Add ynull specification before calls to bt12.
!        06/23/14 - Add computations using the differentiation and integration of eq. (2) in Der Kiureghian
!                 - Change "arg" to "arg1" and "arg2".  It is not good style to use the same variable for
!                   different things.
!                 - Place the TD/RV adjustment section before the computation of ground motions
!                 - Reorder CL and DK computations (CL first)
!                 - Use a hardwired flag to skip the dur4n computations (these were included for a special 
!                   purpose--see 01/21/11 note above).
!        06/25/14 - Use version of DK80eq2 that uses analytic equations for the pdf
!        07/28/14 - For pga or pgv, return ynull if call to bt12_td_rv_for_pga_or_pgv_f returns ynull
!                 - Add pk_dk80_eq2 = ynull to return of oscillator if there is no Drms adjustment
!                 - Include ynull as a parameter
!        11/14/15 - Use the DK80 values for gmsim
!                 - Note:  I should rename BT12 to BT (the factors were updated in 2015).
!                   The params file specifies a file of adjustment coefficients
!                   to be used, but sometime in the future this might be updated from those in 
!                   Boore and Thompson (2015).  But many variables and subprograms include "12" in the name
!                   so I am reluctant to do this now.
 

      character e_or_a*1
      
      real, parameter :: ynull=-9.99

      real amom0,amom1,amom2,amom4,anz,ane

      include 'bt12osc.fi'

      include 'smsim.fi'


!
! compute moments and frequencies, bandwidth factors, rms.
!
!
      amom0=amom_rv(0)	  ! For an oscillator (iaorins == 2), these are the
      amom1=amom_rv(1)	  ! moments of 2*pi*fosc*SD (where SD
      amom2=amom_rv(2)	  ! is the displacement response of the oscillator).
      amom4=amom_rv(4)	  ! See spect_amp in rv_td_subs for this assignment.

      arg1=1.0-amom1*amom1/amom0/amom2
!Old version      if(abs(arg1) .lt. 1.0e-20) arg1=1.0e-20
      if(arg1 < 0.0) then
! Include some write statements to screen until have experience with this;
! delete in a later version
        write(*,'(a,1x,1p,e10.3,a)') 
     :   ' !!! arg1 in deltay calc = ', arg1, 
     :   '; .lt. 0, so setting = 0.0'
        arg1 = 0.0 ! presumes arg1 < 0 only if it is small
      end if
       
      deltay=sqrt(arg1)                    ! used in V-M or T&U calcs
                                          ! This is "q" in Der Kiureghian (1980, 
                                          ! two lines below his eq. 2)
      xi = amom2/sqrt(amom0*amom4)

      arg2=(1.-xi*xi)
!Old version      if(abs(arg2) .lt. 1.0e-20) arg2=1.0e-20
      if(arg2 < 0.0) then
! Include some write statements to screen until have experience with this;
! delete in a later version
        write(*,'(a,1x,1p,e10.3,a)') 
     :   ' !!! arg2 in eps_rv calc = ', arg2, 
     :   '; .lt. 0, so setting = 0.0'
        arg2 = 0.0 ! presumes arg2 < 0 only if it is small
      end if
       
      eps_rv=sqrt(arg2)
      freq20=sqrt(amom2/amom0)/(2.0*pi)
      freq42=sqrt(amom4/amom2)/(2.0*pi)

! because the acceleration is a transient, the durations for
! computing rms and for determining n may be different.
! durex Duration of excitation) is used for computing N. 
! The rms is computed using
! trms, which is determined as durex for
! regular time series and durex + tosc * ( rf/(rf+avib) ) for the
! oscillator output, where tosc is the time for an oscillator
! to decay to 1/e and rf = (fosc * durex)**3. avib is an adjustable
! parameter, currently set to 1/3. The reason for using the oscillator
! duration is that the random vibration theory gives the ratio
! of peak to rms. If the spectral energy is spread over a number of
! cycles as in an oscillator, the "local" rms is smaller than
! if contained just within durex. 
! In effect, we are forced to apply a fixup in an
! attempt to get around this basic limitation.

! DETERMINE DURATION OF EXCITATION:

      dursource_calc = dursource(w_fa, fa, w_fb, fb)
      durpath_calc = durpath(rmod, nknots, rdur, dur, slast, 
     :                       numsource, amag)
      durex = dursource_calc + durpath_calc 

! START DETERMINE DURATION OF RMS:
!
      IF (iaorins >= 2 ) THEN   ! I'm not sure if the oscillator correction
                                ! should be used for the WWSSN short-period response
        avib_bj84 = 1./3.
        avib_lp99 = deltay * sqrt(twopi)
        
        tosc = 1.0/(twopi*damp*fosc)        
                                                              
                                                      
        IF      (osc_crrctn == 0) THEN
        
! no correction
          avib = 1.
          rf = 0.0
          trms = durex
          
        ELSE IF (osc_crrctn == 1) THEN
        
! use BJ correction
          avib = avib_bj84
          rf = (fosc * durex)**3
          trms = durex + tosc * ( rf/(rf+avib) )
          
        ELSE IF (osc_crrctn == 2) THEN
        
! avib, rf are modified by Liu & Pezeshk (in paper submitted to BSSA in
! 1999) as follows:
          avib = avib_lp99
          rf = (fosc * durex)**2
          trms = durex + tosc * ( rf/(rf+avib) )
          
        ELSE IF (osc_crrctn == 3) THEN   ! WNA BT12 osc corr        
          
          avib = 1.
          rf = 0.0
!          ynull = -9.99
          per_dex = perosc/durex
          drms_dex_wna = drms_dex_f(per_dex, amag, rmod, 
     :               damp, twopi, ynull,
     :               nm4bt12wna, nr4bt12wna,
     :               m4bt12wna, logr4bt12wna,
     :               c1bt12wna, c2bt12wna, c3bt12wna,
     :               c4bt12wna, c5bt12wna, c6bt12wna,
     :               c7bt12wna )
          
          if (drms_dex_wna == ynull) then
            pk_dk80_eq2 = ynull
            gmsim = pk_dk80_eq2
            return
          end if
          
          trms = durex * drms_dex_wna
        
        ELSE IF (osc_crrctn == 4) THEN   ! ENA BT12 osc corr        
          
          avib = 1.
          rf = 0.0
!          ynull = -9.99
          per_dex = perosc/durex
          drms_dex_ena = drms_dex_f(per_dex, amag, rmod, 
     :               damp, twopi, ynull,
     :               nm4bt12ena, nr4bt12ena,
     :               m4bt12ena, logr4bt12ena,
     :               c1bt12ena, c2bt12ena, c3bt12ena,
     :               c4bt12ena, c5bt12ena, c6bt12ena,
     :               c7bt12ena )
     
          if (drms_dex_ena == ynull) then
            pk_dk80_eq2 = ynull
            gmsim = pk_dk80_eq2
            return
          end if
        
          trms = durex * drms_dex_ena
        
        ELSE IF (osc_crrctn == 5) THEN   ! Average of ENA and WNA BT12 osc corr        
          
          avib = 1.
          rf = 0.0
!          ynull = -9.99
          per_dex = perosc/durex
          drms_dex_ena = drms_dex_f(per_dex, amag, rmod, 
     :               damp, twopi, ynull,
     :               nm4bt12ena, nr4bt12ena,
     :               m4bt12ena, logr4bt12ena,
     :               c1bt12ena, c2bt12ena, c3bt12ena,
     :               c4bt12ena, c5bt12ena, c6bt12ena,
     :               c7bt12ena )
     
          if (drms_dex_ena == ynull) then
            pk_dk80_eq2 = ynull
            gmsim = pk_dk80_eq2
            return
          end if
          
          drms_dex_wna = drms_dex_f(per_dex, amag, rmod, 
     :               damp, twopi, ynull,
     :               nm4bt12wna, nr4bt12wna,
     :               m4bt12wna, logr4bt12wna,
     :               c1bt12wna, c2bt12wna, c3bt12wna,
     :               c4bt12wna, c5bt12wna, c6bt12wna,
     :               c7bt12wna )
     
          if (drms_dex_wna == ynull) then
            pk_dk80_eq2 = ynull
            gmsim = pk_dk80_eq2
            return
          end if
     
          drms_dex = 0.5*(drms_dex_ena + drms_dex_wna)          
        
          trms = durex * drms_dex
        
        END IF
        
!        trms = trms + tosc * ( rf/(rf+avib) )       ! commented out 06/07/11
        
      ELSE  ! iaorins < 2; not an oscillator
      
        trms = durex
        
      END IF
!
! FINISH DETERMINATION OF TRMS

      rms=sqrt(amom0/trms)
      rms_from_get_motion = rms
      
      ane = 2.0*freq42 * durex
      anz = 2.0*freq20 * durex
! factor of 2.0 because consider positive maxima
! and negative minima.
! ane is an estimate of the total number of extrema.
! anz ( also=ane*sqrt(1.0-eps_rv*eps_rv) ) is an estimate of the
! number of positive and negative zero crossings.
      if (ane <= 1.0) ane = 1.002
      if (anz <= 1.33) anz = 1.33

! COMPUTE ARIAS INTENSITY (ONLY MEANINGFUL IF PGA: IAORINS = 1 AND IDVA = 2):

      if (iaorins == 1 .and. idva == 2) then
        g = 981.0    ! acceleration of gravity, assuming acc units of cm/s^2
        arias_fctr = pi/(2.0*g)
        arias_rv = arias_fctr * amom0
      else
        arias_fctr = 0.0
        arias_rv = arias_fctr * amom0
      end if

! START OBTAIN TD/RV MODIFICATION, USING BT12 COEFFICIENTS, IF NEEDED:
!
!*** NOTE:  This is not needed for oscillator response, because the modification
!           is included in the durations (Drms, Dex)
!
      td_rv_mod = 1.0  ! default value
!      ynull = -9.99
 
      if (iaorins == 1) then                                   ! displacement, velocity, or acceleration
        IF (osc_crrctn == 3) THEN   ! WNA BT12 modification
          if (idva == 0) then  ! PGD
            td_rv_mod = 1.0  ! No BT adjustments were derived for PGD, but continue
          else if (idva == 1) then  ! PGV
            td_rv_mod =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12wna, nr4bt12wna,
     :                      m4bt12wna, logr4bt12wna,
     :                      td_rv_pgv_wna )
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else if (idva == 2) then  ! PGA
            td_rv_mod =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12wna, nr4bt12wna,
     :                      m4bt12wna, logr4bt12wna,
     :                      td_rv_pga_wna )
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else
            print *, ' ERROR: iaorins=1, but idva != 0, 1, or 2; QUIT!'
            stop
          end if
          
        ELSE IF (osc_crrctn == 4) THEN   ! ENA BT12 modification
          if (idva == 0) then  ! PGD
            td_rv_mod = 1.0  ! No BT adjustments were derived for PGD, but continue
          else if (idva == 1) then  ! PGV
            td_rv_mod =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12ena, nr4bt12ena,
     :                      m4bt12ena, logr4bt12ena,
     :                      td_rv_pgv_ena )
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else if (idva == 2) then  ! PGA
            td_rv_mod =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12ena, nr4bt12ena,
     :                      m4bt12ena, logr4bt12ena,
     :                      td_rv_pga_ena )
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else
            print *, ' ERROR: iaorins=1, but idva != 0, 1, or 2; QUIT!'
            stop
          end if
          
        ELSE IF (osc_crrctn == 5) THEN   ! Average of ENA and WNA BT12 osc corr 
          if (idva == 0) then  ! PGD
            td_rv_mod = 1.0  ! No BT adjustments were derived for PGD, but continue
          else if (idva == 1) then  ! PGV
            td_rv_mod_wna =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12wna, nr4bt12wna,
     :                      m4bt12wna, logr4bt12wna,
     :                      td_rv_pgv_wna )
            td_rv_mod_ena =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12ena, nr4bt12ena,
     :                      m4bt12ena, logr4bt12ena,
     :                      td_rv_pgv_ena )
            td_rv_mod = 0.5*(td_rv_mod_ena + td_rv_mod_wna)
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else if (idva == 2) then  ! PGA
            td_rv_mod_wna =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12wna, nr4bt12wna,
     :                      m4bt12wna, logr4bt12wna,
     :                      td_rv_pga_wna )
            td_rv_mod_ena =  bt12_td_rv_for_pga_or_pgv_f(
     :                      amag, rmod, ynull,
     :                      nm4bt12ena, nr4bt12ena,
     :                      m4bt12ena, logr4bt12ena,
     :                      td_rv_pga_ena )
            td_rv_mod = 0.5*(td_rv_mod_ena + td_rv_mod_wna)
            if (td_rv_mod == ynull) then
              pk_dk80_eq2 = ynull
              gmsim = pk_dk80_eq2
              return
            end if
          else
            print *, ' ERROR: iaorins=1, but idva != 0, 1, or 2; QUIT!'
            stop
          end if
        ELSE
          td_rv_mod = 1.0
        END IF
        
      end if
!      
! FINISH OBTAIN TD/RV MODIFICATION, USING BT12 COEFFICIENTS, IF NEEDED:



! COMPUTE GROUND MOTIONS:

!   USING CARTWRIGHT AND LONGUET-HIGGINS:
!

!DEBUG
!      write(*,*) ' amom2, amom0, freq20, durex, dursource_calc, '// 
!     :            'durpath_calc, anz = '
!      write(*,*)   amom2, amom0, freq20, durex, dursource_calc,  
!     :             durpath_calc, anz
!DEBUG

      pk_rms_cl_1 = sqrt(2.0*alog(anz))              ! 'cl' = Cart. & L-H
      pk_rms_cl_2 = pk_rms_cl_1+0.5772/pk_rms_cl_1

      zup_used = zup
      pk_rms_cl_eq68 = cl68_numrcl_int( ane, xi, zup_used) ! zup_used may be 
                                                           ! reset in the 
                                                           ! function

      e_or_a = 'e'                  ! might be used in a print statement

      pk_cl_eq68 = td_rv_mod * rms * pk_rms_cl_eq68    ! eq. 6.8 of C&L-H
      pk_cl_1 = td_rv_mod * rms * pk_rms_cl_1          ! 1 term asymptotic
      pk_cl_2 = td_rv_mod * rms * pk_rms_cl_2          ! 2 term asymptotic

!      gmsim_before_td_rv_mod = pk_cl_eq68 
!      gmsim = pk_cl_eq68
! NOTE: gmsim is returned to gm_rv as the argument to get_motion, after applying 
! any td-rv adjustments below.  It is used in gm_rv_drvr, although I may revise this
! to be in keeping with more recent programs, such as tmrs_loop_rv_drvr, which obtains
! the ground motions from rv common block in smsim.fi


!   USING DER KIUREGHIAN AND ASFURA ASYMPTOTIC RMS-TO-PEAK FACTORS:

      if (iaorins == 1) then   ! not an oscillator
        deltay_prime = deltay
      else
        deltay_prime = amax1(deltay, damp)
      end if
      
      if (deltay_prime <= 0.10) then
        anz_prime = amax1(2.1, 2*deltay_prime*anz)
      else if (deltay_prime<= 0.69) then
        anz_prime = (1.63*deltay_prime**0.45 - 0.38)*anz
      else
        anz_prime = anz
      end if
      
      if (anz_prime > 1.0) then
        pk_rms_dk_1 = sqrt(2.0*alog(anz_prime))              ! 'dk' = Der Kiureghian
        pk_rms_dk_2 = pk_rms_dk_1+0.5772/pk_rms_dk_1
        pk_dk_2 = td_rv_mod * rms * pk_rms_dk_2
      else
        pk_rms_dk_1 = ynull              ! 'dk' = Der Kiureghian
        pk_rms_dk_2 = ynull
        pk_dk_2     = ynull
      end if
      
!   USING EQUATION (2) IN DER KIUREGHIAN (1980) TO COMPUTE THE RMS-TO-PEAK FACTOR:

!      eps_int_dk80_eq2 = 0.0001  
                                   ! at first I used eps_int from the params file, which is also used in the 
                                   ! computation spectral moments in odeint, called from amom_rv.
                                   ! I thought I could use the same value for both.  But I found some chatter
                                   ! in the sims vs period, which I am trying to track down.
      eps_int_dk80_eq2 = eps_int  
      call PF_DK80eq2PDF(anz, deltay, eps_int_dk80_eq2, 
     :                na_dk80_eq2, pk_rms_dk80_eq2)  ! na is an output variable.
                                   ! The problem was not with eps_int, but with calling
                                   ! the DK80_eq2 subroutines with an integer nz.
      
      gmsim_before_td_rv_mod = rms * pk_rms_dk80_eq2 
      gmsim = td_rv_mod * gmsim_before_td_rv_mod
      pk_dk80_eq2 = gmsim  !##### CAREFUL---It seems logical that pk_dk80_eq2 is the value
                              !before applying td_rv_mod, but in programs such as tmrs_loop_rv_drvr,
                              !it is assumed that pk_dk80_eq2 (obtained from a common block in smsim.fi)
                              !is the motion after applying the td_rv_mod adjustment.   I should 
                              !redo the code to make things more consistent, but I am reluctant 
                              !to do this because of the potential for not making changes in all
                              !needed place--such is is the curse of "spaghetti code" (note added 14nov15) 

! NOTE: gmsim is returned to gm_rv as the argument to get_motion, after applying 
! any td-rv adjustments below.  It is used in gm_rv_drvr, although I may revise this
! to be in keeping with more recent programs, such as tmrs_loop_rv_drvr, which obtains
! the ground motions from rv common block in smsim.fi
      

!DEBUG
!      write(*,*) ' For amag, rmod, idva, iaorins, osc_crrctn = ', 
!     :                 amag, rmod, idva, iaorins, osc_crrctn          
!      write(*,*) ' rms, pk_rms_dk80_eq2, gmsim_before_td_rv_mod, td_rv_mod, pk_dk80_eq2, gmsim = ', 
!     :                 td_rv_mod, gmsim_before_td_rv_mod, gmsim          
!DEBUG
 


!***************************************************************************************************
!
! Added section, using dur4n = trms:

      iflag = 0  ! Use to skip the calculations
      
      IF (IFLAG == 0) THEN
      
        dur4n = trms
        ane_dur4n_trms = ynull
        anz_dur4n_trms = ynull
        anz_prime_dur4n_trms = ynull
        pk_rms_dk_1_dur4n_trms = ynull
        pk_rms_dk_2_dur4n_trms = ynull
        pk_dk_2_dur4n_trms = ynull
        zup_used_dur4n_trms = ynull
        pk_rms_cl_eq68_dur4n_trms = ynull
        pk_cl_eq68_dur4n_trms = ynull
         
      ELSE
         
         
        dur4n = trms
        
        ane_dur4n_trms = 2.0*freq42 * dur4n   ! added 06/07/11
        anz_dur4n_trms = 2.0*freq20 * dur4n   ! added 06/07/11
        
!        ane_dur4n_trms = 2.0*freq42 * trms  ! commented out 06/07/11
!        anz_dur4n_trms = 2.0*freq20 * trms  ! commented out 06/07/11
        
        if (ane_dur4n_trms <= 1.0) ane_dur4n_trms = 1.002
        if (anz_dur4n_trms <= 1.33) anz_dur4n_trms = 1.33
        
! compute Der Kiureghian and Asfura pk_rms factor

        if (deltay_prime <= 0.10) then
          anz_prime_dur4n_trms = 
     :            amax1(2.1, 2*deltay_prime*anz_dur4n_trms)
        else if (deltay_prime<= 0.69) then
          anz_prime_dur4n_trms = 
     :      (1.63*deltay_prime**0.45 - 0.38)*anz_dur4n_trms
        else
          anz_prime_dur4n_trms = anz_dur4n_trms
        end if
      
        pk_rms_dk_1_dur4n_trms = sqrt(2.0*alog(anz_prime_dur4n_trms))              ! 'dk' = Der Kiureghian
        pk_rms_dk_2_dur4n_trms = 
     :         pk_rms_dk_1_dur4n_trms+0.5772/pk_rms_dk_1_dur4n_trms
      
        pk_dk_2_dur4n_trms = rms * pk_rms_dk_2_dur4n_trms

! compute Cartwright and Longuet-Higgins estimates of peak/rms:

        zup_used_dur4n_trms = zup
        pk_rms_cl_eq68_dur4n_trms = 
     :    cl68_numrcl_int( ane_dur4n_trms, xi, zup_used_dur4n_trms) ! zup_used may be 
                                                                    ! reset in the 
                                                                    ! function

        pk_cl_eq68_dur4n_trms = rms * pk_rms_cl_eq68_dur4n_trms    ! eq. 6.8 of C&L-H
        
      END IF
      
!
!***************************************************************************************************
 
!
! that is all
!
      return
      end
!----------------- END GET_MOTION -----------------------------

!----------------- BEGIN AMOM_RV -----------------------------
      function amom_rv(i)
! Dates: 06/06/95 - Written by D.M. Boore, patterned after AMOMI, which 
!                   see for more detailed history.
!        11/14/95 - Obtain eps_int from get_params and pass through common
!        02/09/03 - Declare variable 'result' as an array in odeint, as
!                   expected by odeint
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).
!        02/06/11 - Add comment about amom_rv for an oscillator (to clear up
!                   possible confusion about what is being computed---when I
!                   first wrote the program PSV seemed to be the intensity measure
!                   of interest, but thathas now shifted to PSA and SD).
!        04/02/11 - For portability, remove "\smsim\" from  "include '\smsim\smsim.fi'"
!                   To ensure that all "included" files are found, they all should be placed
!                   in the same folder or copied into the folder at compile time.

      external derivs

      real result(1)
      include 'smsim.fi'



      h1 = 0.1
      hmin = 0.0
      imom = i       ! keep param i in parameter list rather than imom;
                     ! imom is passed through the rv common block

      result(1) = 0.0
!DEBUG
!      write(*,*) ' Within amom_rv, before call to odeint: '//
!     :   'i, fup, eps_int, h1, hmin = '
!      write(*,*)  
!     :    i, fup, eps_int, h1, hmin
!DEBUG
      call odeint(result, 1, 0.0, fup, eps_int, h1, hmin,
     :            nok, nbad, derivs)
!DEBUG
!      write(*,*) ' Within amom_rv, after call to odeint: '//
!     :   'i, fup, eps_int, h1, hmin, nok, nbad, result(1) = '
!      write(*,*)  
!     :    i, fup, eps_int, h1, hmin, nok, nbad, result(1)
!DEBUG


      amom_rv = 2.0 * result(1)  ! For an oscillator (iaorins == 2), this is the
                                 ! moment of 2*pi*fosc*SD (where SD
                                 ! is the displacement response of the oscillator).
                                 ! See spect_amp in rv_td_subs for this assignment.

      return
      end
!----------------- END AMOM_RV -----------------------------

!----------------- BEGIN DERIVS -----------------------------
! Dates: 06/07/95 - Written by D.M. Boore
!        02/09/03 - Made y, dydf arrays, as expected in calling routine.
!        12/02/09 - Remove "\smsim\" from  "include '\smsim\smsim.fi'"
!        02/17/10 - Put "\smsim\" back in "include 'smsim.fi'", (see comment after 
!                   "include smsim.fi" in program \forprogs\stress_param_from_psa.for" for
!                   a discussion of where programs expect to find smsim and a solution
!                   involving a copy statement in the compile and link batch file).

      subroutine derivs(freq, y, dydf)

      parameter (nmax=50)
      real y(nmax), dydf(nmax)

      include 'smsim.fi'

      f = freq
      if (freq .eq. 0.0) f = 0.001
      w = twopi * f
      
      a = spect_amp(f)
!DEBUG
!      write(*,*) ' In derivs: f,a, iaorins, fosc, damp, idva = ', 
!     :                        f,a, iaorins, fosc, damp, idva
!DEBUG

      if(imom .eq. 0) then
        dydf(1)=a*a
      else
        dydf(1)=a*a*w**imom
      end if

! Note added years after I first wrote this routine:
! What about specifying "y(1)"?  Setting y(1) = 0.0 gives
! erroneous results.  I should look in my notes; I probably knew what
! I was doing when I first wrote the routine.

      return
      end
!----------------- END DERIVS -----------------------------

!----------------- BEGIN GET_FUP -----------------------------
      subroutine get_fup(fm, akappa, amp_cutoff, nu, fup)
      
! Dates: 12/02/09 - Added by D. Boore
!        02/14/10 - Removed print-to-screen portions; accept computed
!                   value of fup.
      
      real fm, akappa, amp_cutoff, fup, pi
      integer nu
      character rply*10
      
      pi = 4.0 * atan(1.0)
      
!DEBUG
!      print *,' fm, akappa, amp_cutoff, nu, pi = ', 
!     :          fm, akappa, amp_cutoff, nu, pi
!DEBUG
! compute fup parameter for rv integration:
      if( akappa .eq. 0.0) then  
        fup = fm/amp_cutoff**0.25
      else 
        if (fm .eq. 0.0) then
          fup = -alog(amp_cutoff)/(pi*akappa)          
        else
          fup = 
     :     amin1(fm/amp_cutoff**0.25, -alog(amp_cutoff)/(pi*akappa))
        end if
      end if
      write(nu, '(a,1pe10.3)') ' fup calculated in driver = ', fup

!      write(*, '(a,1pe10.3)') ' fup calculated in driver = ', fup
!      rply = ' '
!      write(*, '(a)') '   Override this value? ("Enter"=no): '
!      read(*, '(a)') rply
!      call trim_c(rply,nc_rply)
!      call upstr(rply)
!      if (rply(1:1) .eq. 'Y') then
!        write(*, '(a)') '     Enter new value for fup: '
!        read(*, *) fup
!        write(*, '(a, 1x, f8.3)') '     You entered fup = ', fup
!        write(nu, '(a,1pe10.3)') 
!     :      ' calculated fup overridden; fup from user = ', fup
!      end if
      
      return
      end
!----------------- END GET_FUP -----------------------------

!      include 'cl68_numrcl_int.for'
!      include 'cl68_integrand.for'
      
!      include 'PF_DK80eq2PDF.for'
!      include 'DK80eq2PDF.for'
 
!      include 'bt12osc_eval_eq.for'
!      include 'drms_dex_f.for'

!      include 'bt12_td_rv_for_pga_or_pgv_f.for'

