The files accompanying the SMSIM Open-File report (David M. Boore (2000). 
SMSIM --- Fortran Programs for Simulating Ground Motions from Earthquakes:
Version 2.0 --- A Revision of OFR 96-80-A, U.S.Geological Survey Open-File 
Report 96-80-A, 73 pp.) are contained in compressed files that can be extracted
using WinZip or PKZip (or equivalent).  These files are SMSIMxxx.ZIP, which 
contains the SMSIM programs and SITEAxxx.ZIP, which contains the site 
amplification programs.  Both SMSIMxxx.ZIP and SITEAxxx.ZIP contain source 
code, executables, and sample input and output files.  In the file names, 
"xxx" represent the current version number (e.g., xxx = 218 is version 2.018 
of the program).  

The manual, in Adobe Acrobat format, is contained in the file SMSIM_MANUAL.PDF.

I recommend that the programs be placed in a directory with the name "smsim" 
because the Fortran routines contain include statements that assume that the 
programs to be included are in a folder or directory with this name (e.g., 
"include '\smsim\rvtdsubs.for'").  This recommendation is not relevant if only 
the executable files are being used.

Version 1.5: Includes Arias intensity 

Version 1.6 (01/21/99): Includes an improved way of adjusting the duration used
in computing response spectra from random vibration theory.  This
improvement is described in L. Liu and S. Pezeshk, 1999, Improvement on the 
estimation of pseudo response spectral velocity using RVT method, Bull. Seism.
Soc. Am., (submitted).  Response spectra computed using the Liu and Pezeshk 
equations are closer to those computed from time domain calculations than are 
the RVT simulations using the equations in D. Boore and W. Joyner, 1984, A 
note on the use of random vibration theory to predict amplitudes of transient 
signals, Bull. Seism. Soc. Am. 74, 2035--2039.  The new method can be specified
by setting the new variable osc_crrctn in the line following rv params to 2; 
the Boore and Joyner equations are used if osc_crrctn =1.  For consistency
with previous runs, osc_crrctn.dat has been set to 1.0 in the *.dat files
provided with the smsim programs, but I recommend using osc_crrctn = 2 for
new runs.  This version also includes program TDFADRVR to compute the Fourier
amplitude spectrum from the time series as well as the target spectrum 
(FAS_DRVR computes only the target spectrum from the underlying equations).
Also new with this version is TD_DRVR and RV_DRVR printing out the relative 
displacement response spectra in addition to the pseudo relative velocity and 
pseudo absolute acceleration response spectra.

Version 1.7 (02/12/99): Includes computation of displacement and peak 
displacement.  Note that in the time-domain calculations these are computed
by integrating the acceleration trace twice.  If a low-cut filter is used to
simulate processed observed accelerograms, it is important to allow a long 
enough tshift and minimum duration to capture the filter response (the 
filtering is only done on the acceleration spectrum); otherwise the 
displacement will probably exhibit a long term trend that will
dominate the signal (the filter is acausal, so allowance for the filter
response before tshift is important).  It is a good idea to save the
time series for one of the suite of simulations to check that the various 
time-domain parameters have been chosen so that numerical artifacts such as 
long-period drifts are not present.  To help ensure that adequate pre- and
post-signal pads are included, the algorithm to determine the number of
points used in the FFT has been changed, and the parameter TSIMDUR in previous
versions has been changed to DUR_FCTR (make sure that this change has been
made in *.dat files).  The algorithm overides tshift if necessary to allow
for filter response.  E.g., if fcut = 0.1, norder = 2, pads of 15 sec are needed
before and after the signal (the pads are given by 1.5*(norder/2)/fcut), 
according to Converse in the BAP manual... USGS OFR 92-296A).  If tshift were 
set to 5 sec, this would be replaced with 15 sec.  If, on the other hand, 
tshift were greater than 15 sec then thift would not be overwritten). 
DUR_FCTR is used to give some control on the total duration used in
the FFT.  The number of points in the FFT is a power of 2 such that 

npts*dt > max(filter pad, tshift) + dur_fctr*(source duration + path duration)
 + filter pad.

Also include in this version is the site amplification program NRATTLE.FOR, 
which is a modification by R. Herrmann of C. Mueller's program RATTLE.FOR. 
The program is numerically better behaved than RATTLE at high frequencies, and
the input has been redesigned to be more user friendly. 
Various supplementary files connected with NRATTLE.FOR are also included, 
including F4NRATTL.FOR, which creates an input file for NRATTLE from the 
output of SITE_AMP (using SITE_AMP is a convenient way of obtaining a 
"stair-step" model from a gradient model, even if the quarter-wavelength 
amplifications computed using SITE_AMP are ignored).  The sample control file 
needed by F4NRATTL is F4NRATTL.CTL.  Not surprisingly, the control file needed
by NRATTLE is NRATTLE.CTL.

Other minor changes have been made, including renaming the variable "nruns"
to "nsims"

Version 1.8 (03/05/99): A minor revision of v. 1.7, renumbered only because
many copies of v. 1.7 were recently distributed via a mass emailing, and 
I do not want confusion over what is the most recent version.  The main changes 
are to separate out the get_date and get_time routines into a separate file 
(datetime.for), to write a "banner" with the version number, to change "a\"
to "a" in format specifications to allow redirection of I/O, and to gather 
together the specification of the various subroutines needed to
create an application by using include statements at the end of the driver 
programs to bring in the various subroutines rather than a few include 
statements in e.g., smsim_rv.for and specifying other specific programs in 
the compile and link batch files (e.g., cl_rv.bat).  The result of specifying
the files in several places before is that I made the mistake of including
some files twice.

Version 1.81 (04/23/99): includes a fix to fas_drvr to correct a problem in 
printing the column file for the FAS of oscillator response.

Version 1.82 (07/02/99): Added arguments to durpath to incorporate Atkinson 
and Silva (1999) magnitude-dependent modification to distance.

Version 1.83 (08/03/99): Added option to use normal deviates in time series
programs (iran_type = 0 for normal, anything else for uniform).

Version 1.84 (02/06/00): Added C. Mueller's improvement to cl68_integrand
(to correct a numerical problem he encountered while compiling and running
using DEC Fortran; I never encountered the problem using Lahey F77, running 
on a pc), corrected error if stem of column file name is less than 4 
characters, and improved construction of column headers for some of the 
column files.  I also included the most recent versions of the Numerical Recipes
routines (the biggest change seems to be in locate.for, with a lesser change in 
qtrap.for (which I altered to qmidpnt.for)

Version 1.85 (04/25/00): Minor change --- added site_amp program pwr2lyr,
and some useful comments regarding usage have been added to the text 
at the beginning of f4nrattl.

Version 1.86 (06/05/00 & 06/08/00): Added a separate velocity (c_q) to be 
used in computation of Q, instead of assuming it is the same as the velocity 
near the source.  This requires the addtion of a parameter in the dat file, so
I have updated those dat files included in the standard distribution.  I also
changed the reference for source model 9 (Atkinson and Silva) from 1999 to 
2000 (the year in which the paper was published).  I also added some
comments regarding the meaning of "r" for source model 9 in appropirate places
in rvtdsubs.for.   On 06/08/00 I added the variable r_ref and redid the 
geometrical spreading so that it is nondimensional.  The dimensionality of 
1/distance is now contained in the definition of the constant factor in
const_am0_gsprd; usually r_ref = 1.0 km, but if this value or the units are 
changed, care must be taken to ensure that the program is producing the desired
output.  In particular, changing the units requires eliminating the factor
of 1.e-20 in variable const, which can only be done in the source code.  It 
is safest to keep the dimensions of velocity and distance in km, and the 
dimension of density in gm/cm**3.

Version 1.87 (06/09/00): Bob Herrmann found some include statements that did not
pointed to directories forprogs and site_amp.  I included a statement in 
read.me that the programs should be placed in a directory named "smsim", and 
I tried to change all include statements to only point to this directory.

Version 1.88 (06/20/00): No changes to the main programs, but some 
auxiliary programs have been modified, an additional dat file has 
been added been added (e_bck01l.datm, used by Frankel et al., 1996), 
and an update to the portion of the manual describing the use of the 
smsim programs has been added (smsim_in.pdf).   

Version 1.89 (06/22/00): Print out standard error of the mean rather 
than std.

Version 1.90: 07/29/00: Major change to tbldrvrr.  
Obtain input from a control file, eliminate redundancy in code.  
Added computation of twopi to smsim.fi, and corrected an error in 
harmoscf in rvtdsubs when idva = 2.  Luckily, I think harmoscf is only 
called by random vibration routine smsim_rv, and in that routine I 
force idva = 0.  08/08/00: Obtain units (cgs or g) and type of response
spectrum from control file.  With this change, drvr4art (used to compute
the ground motions used in Frankel et al, 1996, is no longer needed)

Version 2.00: 10/11/00:  This probably does not warrant an increase 
from 1.xx to 2.00, but I am doing so because the manual is being 
revised, and it will have a version number 2.0.  The manual is being 
revised for inclusion in the CD accompanying the IASPEI hanbook on 
seismology (Willie Lee's brainchild).   Only a few minor changes were 
made in the programs between versions 1.90 and 2.00.

Version 2.10: 02/12/01:  Modified the exponential window, using 
normalized time and adding a parameter to control the total extent of 
the window; this required a modification to the parameter files (*.dat).
In addition, use dynamic allocation of time series arrays rather than
hardwiring a dimension.  Combined smsim_td and get_acc.  The version 
number 2.10 rather than 2.01 is warranted by the use of allocatable 
arrays for the first time. 

Version 2.11: 02/16/01: Corrected error in computing average FAS in 
tdfadrvr.for

Version 2.12: 03/12/01: Use double precision version of rd_calc and improve
                        dealing with end of control file for program
                        ratiotbl

Version 2.13: 04/17/01: Allow variable number of headers in control
                        files for f4nrattl.for and nrattle.for
 
Version 2.14: 05/01/01: Change nrattle to obtain halfspace velocity
                        and density from the layer specified as the
                        effective halfspace (if less than the actual
                        halfspace).

Version 2.15: 08/14/01: In get_motion.for in smsim_rv.for: 
                        Prompted by Bob Herrmann's suggestion, I changed the 
                        code dealing with small values of arg, in order to 
                        eliminate sqrt of a negative number.   I probably 
                        should use a double precision version of odeint 
                        (called by amom_rv), but the results are not too 
                        sensitive to the exact value of arg when it is small 
                        (as it is for a very narrow band response).  The 
                        variables deltay and eps_rv are affected by the
                        sqrt(arg) code; eps_rv and deltay is used only in the
                        Liu & Pezeshk oscillator correction.  According to Bob 
                        Herrmann, by eliminating the sqrt .le. 0.0 problem, a 
                        larger eps (e.g., 0.001 rather than 0.00001) can 
                        be used with little loss of accuracy and a significant
                        increase in computational speed.  If arg < 0.0, this
                        version prints to the screen the value of arg.  These
                        messages may be deleted in future versions.
                        In site_amp.for: Fill in density only if dens .eq. 0.0,
                        unlike the change on 4/22/98, in which case the density
                        would also be obtained from the subroutine if 
                        specify_dens_coeff was true.  I have no idea why I had
                        made that change, which no longer makes sense to me.
Version 2.16: 01/27/02: 1) Following Silva, add 2 parameters to the input 
                        parameter for geometrical spreading to allow 
                        magnitude-dependent slopes in 
                        the geometrical spreading.  The slope of each segment
                        is now given by a_s(i) + b_s(i) * ( amag - m_s(i))
                        instead of slope(i).  This requires using 0.0 for
                        b_s if a magnitude-independent slope is desired.
                        2) Reset zup, the upper limit of integration used to
                        evaluate Cartwright & L.H. eq. 6.8 in smsim_rv, if 
                        needed to limit the integration from 0.0 to the value
                        of z when the integrand is so small that the compiler
                        sets it to 0.0 because of underflow.  Evaluating the 
                        integral for larger values is a waste of resources
                        and on rare occasions can lead to small numerical
                        "chatter" in the results (the values of the pk_rms
                        factor changing discontinuously by about 2 or 3 % 
                        for closely spaced oscillator periods).
Version 2.17: 05/29/02: 1) Placed the code that computes the acceleration
                        time series in smsim_td into a separate subroutine
                        (smsimacc.for) that can be used in drivers for 
                        which only a suite of accelerograms are desired 
                        (and not all of the other things provided by 
                        smsim_td, such as response spectra, velocity and 
                        displacements, peak motion parameters, etc.). 
                        2) Wrote acc_drvr.for to compute a suite of 
                        accelerograms for a given M, R, storing each as a
                        smc file (for later processing, using one of my
                        smc utility programs).

Version 2.18: 06/14/02: Extensive reorganization and renaming of various 
                        drivers and modules.

Version 2.19: 08/08/02: Removed the last input parameter (remove dc from 
                        noise) because if the mean noise is removed, 
                        distortion is produced at long periods.  Think of 
                        it as subtracting a box function with duration 
                        equal to the duration of the noise.  If I had 
                        removed the mean from the whole time series (noise
                        segment plus leading and trailing zeros) there
                        would have been no distortion, but then the 
                        portions of the time series before and after the 
                        noise segment would have been nonzero.   The 
                        simulation process works just fine without forcing 
                        the mean of each noise sample to be zero --- 
                        averages of the spectra from a number 
                        of simulations match the target spectrum.

Version 2.20: 02/10/03: Compiled using Lahey/Fujitsu LF95 compiler, so 
                        the programs will run under the Windows XP
                        operating system.  Some minor changes were made,
                        but overall the programs are the same as V 2.19.
                        
Version 2.30: 07/15/05: Added new Atkinson ENA model as source 10.                         
              07/15/05: In tbldrvrr - Write r, rs (not log) also
              08/16/05: Took advantage of the lack of
                         file name length restriction (8.3 previously).
                         This allowed the renaming of many program files
                         to make it easier to know what the program does---
                         e.g., gm_td_drvr rather than gm_drvrt.  I also
                         started to handle character strings in a better
                         fashion (using trim_c), but this has not been
                         done throughout.  Another change in some
                         programs is to suggest output file names as
                         defaults if cr is pressed.
                         Add some smc utility programs to help in computing
                         velocity, displacements, and response spectra for
                         the smc files made using a_ts_drvr (smc2vd and
                         smc2rs2).   I've also included program smctsplt
                         to make a Postscript file with many time series on
                         a single page; this is very useful in taking a 
                         quick look at the accelerations (and velocities
                         and displacements, if made using smc2vd) made using
                         a_ts_drvr.  No comparable program is given to look
                         quickly at the suite of response spectra, but the
                         program smc2asc will make a single ascii file with
                         columns containing individual response spectra;
                         this ascii file can be imported into a graphics
                         program for plotting any set of spectra.

Version 2.31: 01/13/06: Some very minor changes (usually formatting).
Version 2.32: 03/08/06: Some minor changes (usually formatting).
Version 2.33: 04/22/06: Some minor changes (Added tmr_td_drvr, write null
                        (-999.9) values for the tmr_rv_drvr, tmr_td_drvr
                        output when pgv or pga is specified).
Version 2.34: 05/23/06: Improvements to layr2plt ("depth" can be depth-to-bottom
                        or thickness of each layer, and reading density from the
                        input file is an option.
Version 2.40: 04/09/07: SMSIM: Use skipcmnt to read up to 60 text headers before each
                        line containing input parameters in the SMSIM
                        parameter files, and put the various forprogs utility
                        programs in a separate file that is created each
                        time that one of the cl*.bat programs is used 
                        (to make sure that the latest versions of the utility
                        subroutines are used).  Also, rename the *.dat files as 
                        *.params, and edit to add "!" to explanatory text headers.
                        Added program tmrs_rv_drvr, which includes the stress parameter
                        as an input parameter (in which case the params file must specify
                        a source that uses Deltasigma as an input parameter, such as
                        source 1).   The tmr and tmrs programs are the
                        most general programs, because the output can be arranged
                        to be many periods for a single R and M, many M's for a
                        given R, many Deltasigmas for a given R and M, etc. The
                        control file for this and these programs can be constructed
                        in Excel if many distances or periods are wanted, and the
                        control can subsequently be modified by a text editor that
                        allows editing of blocks of text (e.g., 
                        TextPad: http://www.textpad.com/).
                        Remove "\smsim\" from include statements so that the smsim 
                        programs can be unzipped into any folder and the Fortran
                        code can be compiled from that folder.  This eliminates
                        the necessity to edit the include statments in the 
                        Fortran source code if the programs had been unzipped
                        in a folder other than "\smsim\".
                        The programs were compiled with the "-chk" flag in Lahey
                        Fortran 95, in order to check for exceedances of array bounds,
                        etc.  This slows down the execution time somewhat, and I think
                        that this check is unnecessary.  But for the time being, I will
                        sacrify speed for safety.
                        SITE_AMP: show more lines of input file to to determine
                        on which line the model starts.  Also, allow more depths.
                        Update density function, including a parameter to indicate
                        the units of vel. Correct error when do not read density from
                        input file but do read Q or 1/Q.  NOTE: I have only changed
                        the include statements to refer only to files in the working
                        folder for program site_amp.
                        ***UNFINISHED***:  I have not updated the smsim manual to 
                        incorporate the recent changes.  WARNING: In particular, the params
                        file has not in the manual has not kept up to date.  Please look 
                        at ofr.params to see the current input parameters.
Version 2.41: 04/10/08: Add a general two-corner model (model 11).  This required changes to
                        the params file (see ofr.params).
                        
Version 2.42: 03/01/09: Numerous small changes, plus added programs TMRS_FF_RV_DRVR and
                        TMRS_FF_TD_DRVR to compute an effective distance, accounting
                        for fault finiteness, before doing the SMSIM simulations.
                        
Version 2.45: 06/01/10: See the comment lines at the beginning of the source code for each program
                        to see what changes have been made.
                        
Version 3.2:  04/16/11: I am no longer updating this log of changes.   I will note that 
                        there are three main programs for doing stochastic model simulation, with a
                        time-domain (td) and a random-vibration (rv) version of each.  Here is a brief description
                        of these programs (note that for all progams, the important model parameters are in a file
                        (usually named with "params" as the extension to the file name):  
                        
                        NAME                                        DESCRIPTION
                        gm_rv_drvr, gm_td_drvr                      Obtains input, such as the name of the params 
                                                                    file, the magnitude and distance and type of 
                                                                    ground-motion intensity measure for which simulations
                                                                    will be made, interactively from the terminal 
                                                   
                        tmrs_rv_drvr, tmrs_td_drvr                  Obtains input, such as the name of the params 
                                                                    file, the magnitude and distance and type of 
                                                                    ground-motion intensity measure for which simulations
                                                                    will be made, from a control file.  Period (T), 
                                                                    magnitude (M), distance (R), and stress parameter (S)
                                                                    (for source models for which stress is a parameter, as
                                                                    is NOT the case, for example, for the Atkinson and 
                                                                    Silva (2000) source model) are entered n one line.  
                                                                    This line is read and the simulation is made and
                                                                    the results written to an output file.  The next line
                                                                    of the control file is then read and this process is 
                                                                    repeated until the end of the file or the character 
                                                                    string "stop" is reached.  This makes the program very
                                                                    flexible, and by editing the control file or constructing
                                                                    a new one using a spreadsheet, the output can be ground
                                                                    motion as a function of M for a given R, a function of R for
                                                                    a given M, a function of stress (S) for a given M and R,
                                                                    a function of period (T) for a given M, R, and S, etc.                                                                    
                                                                    
                        tmrs_loop_rv_drvr, tmrs_loop_td_drvr        Obtains input, such as the name of the params 
                                                                    file, the magnitude and distance and type of 
                                                                    ground-motion intensity measure for which simulations
                                                                    will be made, from a control file.  Simulations are done
                                                                    for loops over ranges of period (T), magnitude (M), 
                                                                    distance (R), and stress parameter (S).  This can be more
                                                                    convenient than editing the control file used by 
                                                                    tmrs_rv_drvr and tmr_td_drvr for the common cases when
                                                                    want the output for a range of period, distance, magnitude,
                                                                    or stress.
                        
                        

                    