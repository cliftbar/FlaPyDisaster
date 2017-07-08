!Control file for program RATIOS_OF_LOOP_PROGRAM_OUTPUT
! As many comment lines as desired, each starting with "!"
! The string "pp:" indicates a new set of processing parameters
! to be applied to the following smc files.  The parameters are given on the
! following lines.
! Revision of program involving a change in the control file on this date:
! This version of the program assumes that the format of the two files to be used in computing
! ratios is the same.  Look at the program ratios_from_rv_td_loop_drvr when that might not
! be the case.
   03/03/16
!Name of summary file:
  ratios_of_loop_program_output.sum
!Name of output file:
  ratios_of_loop_program_output.out
!Relative accuracy of period to be equal (dPer/Per):
 0.001
!Output from which program?
! 1 = tmrsk_loop_rv_drvr
! 2 = tmrsk_loop_td_drvr
! 3 = fmrsk_loop_fas_drvr
 1
!Lines to skip in files 1 and 2 to reach simulation values (for tmrsk_loop_rv_drvr output
! this is 3).  Enter values for file 1, then file 2
 3 
 3
!ncols2read and ncol4gmim for files 1 and 2. 
! For tmrsk_loop_rv_drvr or tmrsk_loop_td_drvr output with the option for less output:
!   ncols2read=14, and ncol4gmim = 13 or 14 for units of g or cgs, respectively. 
! For fmrsk_loop_fas_drvr:
!   ncols2read=13, and ncol4gmim = 12 or 13 for D or A FS respectively. 
!Note that this version of the program assumes the less output option 
! for the tmrsk_loop_rv_drvr or tmrsk_loop_td_drvr programs
! Enter values for file 1, then file 2
 14 13
 14 13
!List of smsim files,  The ratios will be both file1/file2 and file2/file1
! NOTE: This version is not completely general, as it assumes that the
! first file was made using the RV program (this assumption is needed
! in order to extract the various durations).
!
tmrsk_loop_rv_drvr.ena.scf.bt15scr_fff.bs11_atten.bt15_dp.dmb_3kps_amps_aoi_00.bt15e_drms.col
tmrsk_loop_rv_drvr.ena.scf.bt15scr_fff.bs11_atten.bt15_dp.dmb_avg_bc_amps.bt15e_drms.col

 