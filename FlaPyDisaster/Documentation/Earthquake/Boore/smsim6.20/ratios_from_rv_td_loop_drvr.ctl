!Control file for program Ratios_from_RV_TD_Loop_Drvr
! As many comment lines as desired, each starting with "!"
! The string "pp:" indicates a new set of processing parameters
! to be applied to the following smc files.  The parameters are given on the
! following lines.
! Revision of program involving a change in the control file on this date:
   11/14/15
!Name of summary file:
  ratios_from_rv_td_loop_drvr.sum
!Name of output file:
  rv_td_ratios.out
!Relative accuracy of period to be equal (dPer/Per):
 0.001
!Skip pgv? (Y/N):
 N
!Skip pga? (Y/N):
 N
!Note: The above input is needed to make sure that the periods for consecutive lines read in
! files 1 and 2 are the same.  The input is needed because if an output file was made using tmrs_loop_td_drvr, 
! then output always contains pgv and pga.  The file output was made using tmrs_loop_rv_drvr, may start 
! with spectral values (unless the control file specifically specifies that pgv and pga are to be computed), 
! in which case no lines should be skipped.
!
!Lines to skip in files 1 and 2 to reach simulation values (1 before I
!  added, on 15 June 2011, the SMSIM parameter file name to the first line in the 
!  tmrs_loop output files; 2 after that date, but then I added parameters for Rjb2Rrup conversion,
!  so the value should be 3).  Enter values for file 1, then file 2
 3 3
!List of smsim files,  The ratios will be both file1/file2 and file2/file1
! NOTE: This version is not completely general, as it assumes that the
! first file was made using the RV program (this assumption is needed
! in order to extract the various durations).
!
tmrs_loop_rv_drvr.ena_scf_250b_hr_xdrms.many_m_r.col
tmrs_loop_td_drvr.ena_scf_250b_hr_xdrms.many_m_r.col
