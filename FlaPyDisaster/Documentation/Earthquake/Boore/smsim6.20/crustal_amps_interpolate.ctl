!Control file for crustal_amps_interpolate:
!
! Revision of program involving a change in the control file on this date:
   02/10/15
!
!Files with site amps:
! NOTE: the file must have a "!" at the beginning of any header lines, followed by a line containing
! the number of amps.
  crustal_amps_ab06_bc.txt
!vel, dens used for input site amps
  3.7 2.8
!vel, dens for modified site amps
  3.7 2.8
!log-spaced frequencies (0) or individual frequencies (1)
 0
!if log-spaced frequencies: nfreqs, freqstart, freqstop:
!if individual frequencies: nfreqs, list of nfreqs frequencies:
  0.005 100 50
!  7 0.5, 1.0, 2.0, 5.0, 10.0, 20.0, 100.0
!Type of interpolation:
! 1 = linear f, linear a
! 2 = linear f, log    a
! 3 = log    f, linear a
! 4 = log    f, log    a
!
 4
!File with output:
  crustal_amps_ab06_bc_interpolated.out
 