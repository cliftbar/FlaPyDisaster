! Control file for program tmrsk_loop_td_drvr
! This program is also a substitute for tmr_loop_rv_drvr.
! Revision of program involving a change in the control file on this date:
   03/03/16
!Name of Summary File:
 tmrsk_loop_td_drvr.sum
!
!Comment:
! (This is written to the summary file,
! but not the output column file containing
! the results)
  A test case
!Name of file with SMSIM input parameters:
! ***WARNING*** If want the stress parameter to be used,
! be sure that the source specified
! in the parameter file uses DeltaSigma as a parameter
! (of the 12 sources currently built into the smsim
! programs, only sources 1, 2, 11, and 12 use the DeltaSigma
! as a free parameter).
  100b_1c_raoof_k_0.04.params
!Name of Output Column File for response spectra (and pga. pgv) output:
 tmrsk_loop_td_drvr.col 
!Write a subset of usual output information (no extra material regarding the calculations):
 Y
!Character Tag to Add to Column Labels (up to 4 characters):
 tmrs
!FractionalDamping
 0.05
!log-spaced periods (0) or individual periods (1)
 1
!if log-spaced periods: nper, per_start, per_stop:
!if individual periods: nper, list of nper periods:
! Note that pgv and pga are computed automatically, so per = - 1 or 0
! should not be specified.  If want only pgv and pga, specify nper <= 0
! 200 0.01 100.0
 5 0.01, 0.1, 0.2, 1.0, 2.0
!linearly spaced magnitudes (0) or individual magnitudes (1)?
 0
!if individual magnitudes: nmag, list of nmag magnitudes:
!if linearly spaced magnitudes: m_start, delta_m, m_stop
! 3 4.0 6.0 7.0
 4.0 1.0 8.0
!kappa_0's (k0's) from params file (-1), log-spaced k0 (0) or individual k0 (1)
 0
!if from params file, still need a line as a placeholder (which will be skipped over):
!if log-spaced k0: nk0, k0_start, k0_stop:
!if individual k0: nk0, list of nk0 k0's:
! Note: if specify k0, then dkappadmag, amagkref will be set to 0.0 (they are used in
! kappa_f, called by dimin in rv_td_subs.for).  This means that k0 will not be magnitude dependent.
! This is not a great loss, because I never used this option in the params file.  Note that the kappa 
! obtained from the line before (except when the value is obtained from the params file)
! is the same as "akappa" in smsim.fi. kappa = akappa + dkappadmag*(mag-amagkref in the kappa_f function 
! in rv_td_subs.for. A word on notation: "kappa" should be "k0" for kappa_0, but I wrote my smsim programs before appreciating
! the need for notation that distinguished kappa measured at some distance from the kappa extrapolated
! to zero distance, which is what is used in the calculations.
7 0.0025 0.16 
! 7  0.0025 0.005 0.01 0.02 0.04 0.08 0.16
!log-spaced distances (0) or individual distances (1)
 1
!if log-spaced distances: nr, r_start, r_stop:
!if individual distances: nr, list of nr distances:
! 200 1.0 400.0
 3 10.0 40.0 80.0
!
!Parameters M1, h1, c1, c2, c3, M2, h2 for pseudodepth used to convert loop R values (assumed to be
!  Rjb unless the pseudodepth = 0.0, as would be given by specifying h1=h2=c0=c1=c2=c3=0.0; or 
!  by specifying M1=20.0 (or some other number greater than largest magnitude for which the motions are
!  to be computed) and h1=0.0), in which case it is assumed that the loop R values are Rrup).
!  The equation is
!     M <= M1: h = h1
!     M1< M < M2: h = h1+ c1*(M-M1) + c2*(M-M1)^2 + c3*(M-M1)^3
!     M >= M2: h = h2
!   Note that all the parameters are read in one read statement, so that they can be strung together on
!   one line if desired.  I have separated them into three lines for clarity.
!
! Note: for a quadratic with zero slope at h1 and h2:
! c1 = 0.0
! c2 = -(3/2)*c3*(M2-M1)
! c3 = -2*(h2-h1)/(M2-M1)^3
!
! For a line joining the values:
! c1 = (h2-h1)/(M2-M1)
!
!No pseudodepth:
! 3.75  0.0
! 0.0 0.0 0.0
! 7.50  0.0 
!
!quadratic joining h1 & h2 smoothly (zero slope) (h1 = 8 km):
! 3.75 8.0
! 0.0 -1.706666667   0.303407407
! 7.5 0.0
!quadratic joining h1&  h2 smoothly (zero slope) (h1 = 13 km):
 3.75 13.0
 0.0 -2.773333333   0.493037037
 7.5 0.0
!
!stress from params file (-1), log-spaced stresses (0) or individual stress (1)
 0
!if log-spaced stresses: nstress, stress_start, stress_stop:
!if individual stresses: nstress, list of nstress stresss:
!NOTE: if choose stress from params file, then the input below is not used
!  This allows using an M-dependent stress option, as specified by the parameters
!  stressc, dlsdm, and amagc in the params field, for sources for which stress is a free
!  parameter (such as sources 1, 2, 11, and 12).
 4 1.0 1000.0
 