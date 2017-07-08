!Control file for program tbl_fas
!Name of Summary File:
 tbl_fas.sum
!Comment:
 Test case for Open-File report
!Name of File with Input Parameters:
 ofr4.params
!Name of Tag to Add to Output File:
 tbl_fas_
!Enter V1 and V2 for BJF (a(V2)/A(V1) is computed; if pgv computed, no amps)
 310.0 310.0
!Enter mag_start, dmag, nmag
 5.0 1.0 3
!Enter rlow, rhigh, ndist (log spaced):
 1.0 100.0 11
!Enter damp, nper 
 0.05 6
!per (<0 => FAS of ground, not oscillator response)
  0.1
  0.2
  0.5
  1.0
  2.0
 11.0
 