

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

!----------------- BEGIN CL68_NUMRCL_INT -----------------------------
      function cl68_numrcl_int( an_in, xi_in, zup)
! Numerical integration of eq. 6.8 in Cartwright and Longuet-Higgins
! Dates: 06/09/95 - Written by D.M. Boore, and tested using CHK_INT.
!                   I also plotted the integrand for typical values
!                   of an, xi, and found that it decays strongly to zero
!                   by a value of 5 for the variable.  I use 10 as an upper
!                   limit, which should be much more than enough.  The 
!                   integration routines are such, however, that I could 
!                   probably use a much larger m=number with little extra
!                   time.
!        12/19/95 - Added zup to exact_numrcl
!        12/28/95 - Name changed from exact_numrcl to cl68_numrcl
!        01/03/96 - Name changed from cl68_numrcl to cl68_numrcl_int
!        03/13/99 - On the advice of R. Herrmann, substituted qromb for
!                   qmidpnt.  Numerical tests indicate that both give the
!                   same answers, but qromb evaluates the function at the 
!                   endpoints (zlow and zup), whereas qmidpnt does not (and
!                   is appropriate for an improper integral for which
!                   the integrand cannot be evaluated right at the endpoints;
!                   this is not the case here).
!        01/27/02 - Reset zup so that the integrand is not equal to 0.0 
!                   (except the last point).  I did this because I was being 
!                   plagued with small differences in the pk_rms factor for
!                   some values of D, M, and zup.  After a bunch of testing,
!                   using embedded write statements and the program chk_int,
!                   I'm pretty sure that the problem is related to the attempt
!                   to numerically integrate the function when it has been set
!                   to 0.0 by the compiler (because of underflow).  The 
!                   integrand decreases rapidly, so that zup=10 is generally
!                   much larger than needed.  Note that I also eliminated the
!                   "glitch" in pk_rms by setting eps in qromb to 1.0e-8, but I
!                   think the current fix is better.

      external cl68_integrand

      common /clint/ xi, an

      an = an_in
      xi = xi_in      
      zlow = 0.0 ! where is zup? It is read in as a parameter in the input file.

! Reset zup if need be:
      zup_step = 0.1
      do while (cl68_integrand(zup) .eq. 0.0) 
        zup = zup - zup_step
      end do
      zup = zup + zup_step
     
      call qromb(cl68_integrand,zlow,zup,result)

      cl68_numrcl_int = result/sqrt(2.0)

      return
      end
!----------------- END CL68_NUMRCL_INT -----------------------------

!----------------- BEGIN CL68_INTEGRAND -----------------------------
      function cl68_integrand(z)
! Dates: 06/09/95 - Written by D.M. Boore.  See 7/11/82 notes for
!                   stochastic model, with 6/9/95 addition that uses
!                   a variable transformation to remove the sqrt
!                   singularity at the origin.
!        01/03/95 - Name changed from cl_int to cl68_integrand
!        02/05/00 - Made changes suggested by C. Mueller to avoid
!                   possible numerical problem when "an" is large

      common /clint/ xi, an
!      real*8 y, dum

!      dum = 1.0d0-xi*dexp(-z**2)
!      y = an * dlog(dum)   ! Mueller modification
!      if (y .lt. -10.0d0**(-10)) then !based on machine epsilon for dble precsn
!        cl68_integrand = 0.0  ! or could let compiler deal with underflow
!      else
!        cl68_integrand = 2.0*(1.0d0-dexp(y))  ! Mueller modification
!      end if

      y = an * alog(1.0-xi*exp(-z**2))   ! Mueller modification
      cl68_integrand = 2.0*(1.0-exp(y))  ! Mueller modification

!      cl68_integrand = 2.0*(1.0-(1.0-xi*exp(-z**2))**an)  ! original

      return
      end
!----------------- END CL68_INTEGRAND -----------------------------
! ------------------------------------------------------------------ PF_DK80eq2PDF
      SUBROUTINE PF_DK80eq2PDF(anz,q,tol,na,pf)
      
!Dates: 06/20/14 - Written by D> Boore, patterned after a program by E. Rathje.
!       06/24/14 - Do not require that nz is an integer.  This leads to small jumps in the peak 
!                  factor with period.  I replaced nz with anz here, in keeping with
!                  the usage in SMSIM
!       06/24/14 - Written by D> Boore: use analytic PDF

      REAL q,da, amax, pf, tol, anz
      REAL a(10000),pdf(10000)  ! should make these allocatible
      INTEGER i, nint
      
      amax = 6 ! Based on runs of evaluate_dkeq2.for; see plots of output in
               ! Evaluate_DKeq2.out.draw
       
      na = 10 ! To initiate computations
      da = amax/real(na)
        
      DO i = 1,na
        a(i) = real(i) * da
      ENDDO
      
      pf = 0

      nint = na-2

      DO i = 2,na-2
        pf = pf + a(i)*DK80eq2PDF(a(i),q, anz)
      END DO
      
      pf1 = pf * da
      
      DO
      
        na = 2*na
        da = amax/real(na)
          
        DO i = 1,na
          a(i) = real(i) * da
        ENDDO
        
        pf = 0

        nint = na-2
        
        DO i = 2,na-2
          pf = pf + a(i)*DK80eq2PDF(a(i),q, anz)
        END DO
        
        pf = pf * da
        
        if ((pf-pf1)/pf1 < tol) EXIT
        
        pf1 = pf
        
      END DO       
 
      END
! ------------------------------------------------------------------ PF_DK80eq2PDF
! ------------------------------------------------------------------ DK80eq2PDF
      FUNCTION DK80eq2PDF(pf,q,anz)

! The PDF corresponding to DK80 eq. 2, using analytic derivatives      
! Dates: 06/24/14 - Written by D. Boore, DK80eq2     

      REAL pf, q, sqrtpidiv2, qe, anz
      
      sqrtpidiv2 = sqrt(2.0*atan(1.0))
      
      qe = q**1.2     ! l. 2 below eq. 2 in DK(1980)
      
      e1 = exp(-0.5*pf**2)
      e2 = exp(-sqrtpidiv2*qe*pf)
      
      A = (1.0-e1)
      B = (1.0-e2)
      C = A/e1
      E = exp(-anz*B/C)
      
      dAda = pf * e1
      dBda = sqrtpidiv2*qe*e2
      dCda = pf/e1
      
      dEda = -anz*E*(C*dBda - B*dCda)/C**2
      
      DK80eq2PDF = A*dEda + E*dAda
       
      
      END
! ------------------------------------------------------------------ DK80eq2PDF
      function bt12osc_eval_eq(per_dex, damp, 
     :         c1, c2, c3, c4, c5, c6, c7, twopi)
      
! Eric's params equivalences:

!  c1 = A
!  c2 = B
!  c3 = 2.0
!  c4 = eps
!  c5 = alpha
!  c6 = n
!  c7 = delta
  
      
! Dates: 06/18/11 - Written by D. Boore
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.

      eta = per_dex

      F = c1+c2*(1.0-eta**c3)/(1.0+eta**c3)
      
      bt12osc = 
     :      1.0 + c4*(1.0/(twopi*damp))*(eta/(1.0+c5*eta**c6))**c7
      
      bt12osc_eval_eq = F * bt12osc
      
      return
      
      end
      
            function drms_dex_f(per_dex, m, r, damp, twopi, rnull,
     :  nm4bt12osc, nr4bt12osc,
     :  m4bt12osc, logr4bt12osc,
     :  c1bt12osc, c2bt12osc, c3bt12osc,
     :  c4bt12osc, c5bt12osc, c6bt12osc,
     :  c7bt12osc )
      
! Find Drms/Dex for specified M and R by interpolating log Drms/Dex evaluated for
! tabulated values of M and R surrounding the desired M and R

! Eric's params equivalences:

!  c1 = A
!  c2 = B
!  c3 = 2.0
!  c4 = eps
!  c5 = alpha
!  c6 = n
!  c7 = delta
  
      
! Dates: 06/18/11 - Written by D. Boore
!        07/02/11 - Allow for use of BT12 ena, wna, or both oscillator corrections.
!                   I think it is best if the necessary files are passed 
!                   via argument lists rather than a common block
!        08/02/11 - Changed "BT11" to "BT12", in anticipation of the paper being published in 2012.
!        07/23/14 - Temp change for the new BT14 factors
!        07/28/14 - Return rnull if c1 = c2 = 0
!        11/17/14 - Changed above statement to "Return rnull if m, r is out of range".
!                 - Comment out "stop" in debug statements

      real per_dex, m, r, logr, damp, twopi, rnull
      
      real y_interp, u, t, y1,y2,y3,y4
      
      integer, parameter :: nmparam=13, nrparam=15  ! Added 07/23/14
      integer nm4bt12osc, nr4bt12osc
      real m4bt12osc(nmparam), logr4bt12osc(nrparam)
      real c1bt12osc(nmparam,nrparam), c2bt12osc(nmparam,nrparam), 
     :     c3bt12osc(nmparam,nrparam)
      real c4bt12osc(nmparam,nrparam), c5bt12osc(nmparam,nrparam), 
     :     c6bt12osc(nmparam,nrparam)
      real c7bt12osc(nmparam,nrparam) 

!      include 'bt12osc.fi'

! Find j:

      call locate(m4bt12osc,nm4bt12osc,m,j)

      if (j == 0 .or. j == nm4bt12osc) then ! out of range
!DEBUG
       print *,' In drms_dex_f, m is out of range: m, j,  m4bt12osc = '   
       print *, m, j,  m4bt12osc  
!       stop
!DEBUG
        drms_dex_f = rnull
        return
      end if
      
! Find k:

      logr = alog10(r)

      call locate(logr4bt12osc,nr4bt12osc,logr,k)

      if (k == 0 .or. k == nr4bt12osc) then ! out of range
!DEBUG
       print *,' In drms_dex_f, logr is out of range:'//
     :           ' r, logr, k,  logr4bt12osc = '   
       print *,    r, logr, k,  logr4bt12osc  
!       stop
!DEBUG
        drms_dex_f = rnull
        return
      end if
      
      if (c1bt12osc(j,k) == 0.0 .and. c2bt12osc(j,k) == 0.0) then
        drms_dex_f = rnull
        return
      end if


! Evaluate log(drms_dex) at the four corners:

      y1 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j,k), c2bt12osc(j,k), c3bt12osc(j,k), 
     :      c4bt12osc(j,k), c5bt12osc(j,k), c6bt12osc(j,k), 
     :      c7bt12osc(j,k), twopi)
     
      y2 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j+1,k), c2bt12osc(j+1,k), c3bt12osc(j+1,k), 
     :      c4bt12osc(j+1,k), c5bt12osc(j+1,k), c6bt12osc(j+1,k), 
     :      c7bt12osc(j+1,k), twopi)

      y3 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j+1,k+1), c2bt12osc(j+1,k+1), c3bt12osc(j+1,k+1), 
     :      c4bt12osc(j+1,k+1), c5bt12osc(j+1,k+1), c6bt12osc(j+1,k+1), 
     :      c7bt12osc(j+1,k+1), twopi)

      y4 = bt12osc_eval_eq(per_dex, damp, 
     :      c1bt12osc(j,k+1), c2bt12osc(j,k+1), c3bt12osc(j,k+1), 
     :      c4bt12osc(j,k+1), c5bt12osc(j,k+1), c6bt12osc(j,k+1), 
     :      c7bt12osc(j,k+1), twopi)
     
      y1 = alog10(y1)
      y2 = alog10(y2)
      y3 = alog10(y3)
      y4 = alog10(y4)


      t = (m -m4bt12osc(j))/(m4bt12osc(j+1) - m4bt12osc(j))
      u = (logr -logr4bt12osc(k))/(logr4bt12osc(k+1) - logr4bt12osc(k))
      
       
      y_interp = (1.0-t)*(1.0-u)*y1 + t*(1.0-u)*y2 + 
     :           t*u*y3 + (1.0-t)*u*y4
     
      drms_dex_f = 10.0**y_interp
      
        
      return
      
      end
      
            function bt12_td_rv_for_pga_or_pgv_f(m, r, ynull,
     :  nm4bt12, nr4bt12,
     :  m4bt12, logr4bt12,
     :  bt12_td_rv )
      
! Find td/rv for specified M and R by interpolating td/rv evaluated for
! tabulated values of M and R surrounding the desired M and R

! The input arrays are read in subroutine get_params, contained in rv_td_subs, and
! are passed through common blocks in bt12osc.fi.

! Dates: 09/20/11 - Written by D. Boore, based on drms_dex_f.for
!        07/23/14 - Temp change for the new BT14 factors
!        07/28/14 - Set to ynull if bt12_td_rv = 0
!        11/17/14 - Changed above statement to "Set to ynull if m, r is out of range".

      real m, r, logr, ynull
      
      real y_interp, u, t, y1,y2,y3,y4
      
      integer, parameter :: nmparam=13, nrparam=15  ! Added 07/23/14
      integer nm4bt12, nr4bt12
      real m4bt12(nmparam), logr4bt12(nrparam)
      real bt12_td_rv(nmparam,nrparam)


! Find j:

      call locate(m4bt12,nm4bt12,m,j)

      if (j == 0 .or. j == nm4bt12) then ! out of range
        bt12_td_rv_for_pga_or_pgv_f = ynull
        return
      end if
      
! Find k:

      logr = alog10(r)

      call locate(logr4bt12,nr4bt12,logr,k)

      if (k == 0 .or. k == nr4bt12) then ! out of range
        bt12_td_rv_for_pga_or_pgv_f = ynull
        return
      end if
      
      if (bt12_td_rv(j,k) == 0.0) then
        bt12_td_rv_for_pga_or_pgv_f = ynull
!DEBUG
      print *,' For M, R, j, k, bt12_td_rv(j,k), '//
     :         'bt12_td_rv_for_pga_or_pgv_f = '
      print *,  M, R, j, k, bt12_td_rv(j,k),  
     :         bt12_td_rv_for_pga_or_pgv_f
!DEBUG
        return
      end if
        

! Evaluate TD/RV at the four corners (I made plots of TD/RV and log TD/RV as a function
! of log R for the M's; I see no reason to interpolate log TD/RV rather than TD/RV):

      y1 = bt12_td_rv(j,k) 
     
      y2 = bt12_td_rv(j+1,k)

      y3 = bt12_td_rv(j+1,k+1) 

      y4 = bt12_td_rv(j,k+1) 
 
      t = (m -m4bt12(j))/(m4bt12(j+1) - m4bt12(j))
      u = (logr -logr4bt12(k))/(logr4bt12(k+1) - logr4bt12(k))
      
       
      y_interp = (1.0-t)*(1.0-u)*y1 + t*(1.0-u)*y2 + 
     :           t*u*y3 + (1.0-t)*u*y4
     
      bt12_td_rv_for_pga_or_pgv_f = y_interp
      
        
      return
      
      end
      
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
 
 ! ---------------------- BEGIN QROMB SUITE ------------------------
      SUBROUTINE qromb(func,a,b,ss)
      INTEGER JMAX,JMAXP,K,KM
      REAL a,b,func,ss,EPS
      EXTERNAL func
      PARAMETER (EPS=1.e-6, JMAX=20, JMAXP=JMAX+1, K=5, KM=K-1)
!U    USES polint,trapzd
      INTEGER j
      REAL dss,h(JMAXP),s(JMAXP)
      h(1)=1.
      do 11 j=1,JMAX
        call trapzd(func,a,b,s(j),j)
        if (j.ge.K) then
          call polint(h(j-KM),s(j-KM),K,0.,ss,dss)
          if (abs(dss).le.EPS*abs(ss)) return
        endif
        s(j+1)=s(j)
        h(j+1)=0.25*h(j)
11    continue
      call error(' Too many steps in qromb') ! mod. by RBH
      END

      SUBROUTINE trapzd(func,a,b,s,n)
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL del,sum,tnm,x
      if (n.eq.1) then
        s=0.5*(b-a)*(func(a)+func(b))
      else
        it=2**(n-2)
        tnm=it
        del=(b-a)/tnm
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+del
11      continue
        s=0.5*(s+(b-a)*sum/tnm)
      endif
      return
      END

      SUBROUTINE polint(xa,ya,n,x,y,dy)
      INTEGER n,NMAX
      REAL dy,x,y,xa(n),ya(n)
      PARAMETER (NMAX=10)
      INTEGER i,m,ns
      REAL den,dif,dift,ho,hp,w,c(NMAX),d(NMAX)
      ns=1
      dif=abs(x-xa(1))
      do 11 i=1,n
        dift=abs(x-xa(i))
        if (dift.lt.dif) then
          ns=i
          dif=dift
        endif
        c(i)=ya(i)
        d(i)=ya(i)
11    continue
      y=ya(ns)
      ns=ns-1
      do 13 m=1,n-1
        do 12 i=1,n-m
          ho=xa(i)-x
          hp=xa(i+m)-x
          w=c(i+1)-d(i)
          den=ho-hp
          if(den.eq.0.)call error(' denominator = 0 in polint') ! mod. by RBH
          den=w/den
          d(i)=hp*den
          c(i)=ho*den
12      continue
        if (2*ns.lt.n-m)then
          dy=c(ns+1)
        else
          dy=d(ns)
          ns=ns-1
        endif
        y=y+dy
13    continue
      return
      END

      subroutine error(str)
!-----
!	RBH code (3/99)
!-----
      character str*(*)
      write(6,*)str
      stop
      end
! ----------------------- END QROMB SUITE ---------------

!----------------- BEGIN QMIDPNT -----------------------------
      SUBROUTINE qmidpnt(func,a,b,s)
!Dates: 02/09/00 - latest version of qtrap, with midpnt substituted for trapzd
      INTEGER JMAX
      REAL a,b,func,s,EPS
      EXTERNAL func
      PARAMETER (EPS=1.e-6, JMAX=12)
!U    USES midpnt
      INTEGER j
      REAL olds
      olds=-1.e30
      do j=1,JMAX
        call midpnt(func,a,b,s,j)
        if (j.gt.5) then
          if (abs(s-olds).lt.EPS*abs(olds).or.(s.eq.0..and.olds.eq.0.)) 
     *          return
        endif
        olds=s
      end do
      call error(' too many steps in qmidpnt')
      END
!----------------- END QMIDPNT -----------------------------

!----------------- BEGIN MIDPNT -----------------------------
      SUBROUTINE midpnt(func,a,b,s,n)
      INTEGER n
      REAL a,b,s,func
      EXTERNAL func
      INTEGER it,j
      REAL ddel,del,sum,tnm,x
      if (n.eq.1) then
        s=(b-a)*func(0.5*(a+b))
      else
        it=3**(n-2)
        tnm=it
        del=(b-a)/(3.*tnm)
        ddel=del+del
        x=a+0.5*del
        sum=0.
        do 11 j=1,it
          sum=sum+func(x)
          x=x+ddel
          sum=sum+func(x)
          x=x+del
11      continue
        s=(s+(b-a)*sum/tnm)/3.
      endif
      return
      END
!----------------- END MIDPNT -----------------------------

! --------------------------- BEGIN LOCATE ------------------
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
! --------------------------- END LOCATE ------------------

      SUBROUTINE odeint(ystart,nvar,x1,x2,eps,h1,hmin,nok,nbad,derivs)

!  6/01/95 - D. Boore removed rkqs from the list of calling arguments 
!            and from the external statement.
!  2/09/03 - Set kmax = 0

      INTEGER nbad,nok,nvar,KMAXX,MAXSTP,NMAX
      REAL eps,h1,hmin,x1,x2,ystart(nvar),TINY
      EXTERNAL derivs
      PARAMETER (MAXSTP=10000,NMAX=50,KMAXX=200,TINY=1.e-30)
      INTEGER i,kmax,kount,nstp
      REAL dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp
      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      kmax = 0
      do 11 i=1,nvar
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.*dxsav
      do 16 nstp=1,MAXSTP
        call derivs(x,y,dydx)
        do 12 i=1,nvar
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvar
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
        call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvar
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvar
              yp(i,kount)=y(i)
15          continue
          endif
          return
        endif
        if(abs(hnext).lt.hmin) 
     :    call error(' stepsize smaller than minimum in odeint')
        h=hnext
16    continue
      call error(' too many steps in odeint')
      return
      END
! --------------- END ODEINT ---------------------------------

! --------------- BEGIN RKQS ---------------------------------
      SUBROUTINE rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
      INTEGER n,NMAX
      REAL eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
!U    USES derivs,rkck
      INTEGER i
      REAL errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4)
      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr,derivs)
      errmax=0.
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x) call error(' stepsize underflow in rkqs')
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END
! --------------- END RKQS ---------------------------------

! --------------- BEGIN RKCK ---------------------------------
      SUBROUTINE rkck(y,dydx,n,x,h,yout,yerr,derivs)
      INTEGER n,NMAX
      REAL h,x,dydx(n),y(n),yerr(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
!U    USES derivs
      INTEGER i
      REAL ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,
     *B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5,
     *B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512.,
     *B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,
     *C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,
     *DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336.,
     *DC6=C6-.25)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivs(x+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivs(x+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivs(x+A4*h,ytemp,ak4)
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivs(x+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivs(x+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END
! --------------- END RKCK ---------------------------------

!  ------------------- BEGIN GASDEV --------------------------
      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
!U    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (idum.lt.0) iset=0
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
!  ------------------- END GASDEV --------------------------

!  ------------------- BEGIN RAN1 --------------------------
      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
!  ------------------- END RAN1 --------------------------

!  ------------------- BEGIN REALFT --------------------------
      SUBROUTINE realft(data,n,isign)
      INTEGER isign,n
      REAL data(n)
!U    USES four1
      INTEGER i,i1,i2,i3,i4,n2p3
      REAL c1,c2,h1i,h1r,h2i,h2r,wis,wrs
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/dble(n/2)
      c1=0.5
      if (isign.eq.1) then
        c2=-0.5
        call four1(data,n/2,+1)
      else
        c2=0.5
        theta=-theta
      endif
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      wr=1.0d0+wpr
      wi=wpi
      n2p3=n+3
      do 11 i=2,n/4
        i1=2*i-1
        i2=i1+1
        i3=n2p3-i2
        i4=i3+1
        wrs=sngl(wr)
        wis=sngl(wi)
        h1r=c1*(data(i1)+data(i3))
        h1i=c1*(data(i2)-data(i4))
        h2r=-c2*(data(i2)+data(i4))
        h2i=c2*(data(i1)-data(i3))
        data(i1)=h1r+wrs*h2r-wis*h2i
        data(i2)=h1i+wrs*h2i+wis*h2r
        data(i3)=h1r-wrs*h2r+wis*h2i
        data(i4)=-h1i+wrs*h2i+wis*h2r
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
11    continue
      if (isign.eq.1) then
        h1r=data(1)
        data(1)=h1r+data(2)
        data(2)=h1r-data(2)
      else
        h1r=data(1)
        data(1)=c1*(h1r+data(2))
        data(2)=c1*(h1r-data(2))
        call four1(data,n/2,-1)
      endif
      return
      END
!  ------------------- END REALFT --------------------------

!  ------------------- BEGIN FOUR1 --------------------------
      SUBROUTINE four1(data,nn,isign)
      INTEGER isign,nn
      REAL data(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
            tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
!  ------------------- END FOUR1 --------------------------

! ------------------ BEGIN SORT --------------------------
      SUBROUTINE sort(n,arr)
      INTEGER n,M,NSTACK
      REAL arr(n)
      PARAMETER (M=7,NSTACK=50)
      INTEGER i,ir,j,jstack,k,l,istack(NSTACK)
      REAL a,temp
      jstack=0
      l=1
      ir=n
1     if(ir-l.lt.M)then
        do 12 j=l+1,ir
          a=arr(j)
          do 11 i=j-1,l,-1
            if(arr(i).le.a)goto 2
            arr(i+1)=arr(i)
11        continue
          i=l-1
2         arr(i+1)=a
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
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(l+1))then
          temp=arr(l)
          arr(l)=arr(l+1)
          arr(l+1)=temp
        endif
        i=l+1
        j=ir
        a=arr(l+1)
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
        goto 3
5       arr(l+1)=arr(j)
        arr(j)=a
        jstack=jstack+2
        
!        if(jstack.gt.NSTACK)pause 'NSTACK too small in sort'  ! replaced with code below on 11/02/12
                                                               ! because "pause" produced a compile error
                                                               ! with the Unix Fortran compiler
        if(jstack.gt.NSTACK) then
          print *,' NSTACK too small in sort; QUIT!!!'
          stop
        end if
        
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
! ------------------ END SORT --------------------------
 
