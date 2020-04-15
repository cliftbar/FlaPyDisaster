!Control file for program RV_TD_Ratios
! As many comment lines as desired, each starting with "!"
! The string "pp:" indicates a new set of processing parameters
! to be applied to the following smc files.  The parameters are given on the
! following lines.
! Revision of program involving a change in the control file on this date:
   01/30/11
!Name of summary file:
 rv_td_ratios.sum
! rv/td (0) or td/rv (/=0)?
  0
!List of files
! This version assumes an output file name, followed by two TD files and then by two RV files, made using
! the two choices for osc_crrctn.  If the ratio of only two files is desired, just
! duplicate the entries.
 rv_td_ratios_m5_r005.out
 lc_0.04_osc_crrctn_1.m5.00r005.0_rs.td.col
 box.m5.00r005.0_rs.td.col
 lc_0.04_osc_crrctn_1.m5.00r005.0_rs.rv.col
 lc_0.04_osc_crrctn_2.m5.00r005.0_rs.rv.col
Stop
