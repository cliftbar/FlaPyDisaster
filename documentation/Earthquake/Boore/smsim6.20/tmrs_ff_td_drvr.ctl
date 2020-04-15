! Control file for program trms_ff_td_drvr
! Revision of program involving a change in the control file on this date:
   02/27/09
!Name of file with SMSIM input parameters:
! ***WARNING*** For consistency, the source specified
! in the parameter file should use DeltaSigma as a parameter
! (of the 12 sources currently built into the smsim
! programs, only sources 1, 2, 11, and 12 use the DeltaSigma
! as a free parameter).
 ofr4.params
!Output file names stem
  ff_td_drvr_m7.0_stress_250_az045_54sites
!lat and lon of near corner of the upper edge of fault
! (the fault extends away from this point at an azimuth
! given by the strike, and it dips to the right, looking along the
! strike direction).
  0.0 0.0  
!strike,dip, depth of fault
  0.0 50.0 2.0            	
!fault type (S=strikeslip; R=reverse; N=normal; U=undifferentiated) 
! (Only used if Wells and Coppersmith is used to obtain FL and FW).
  R                               
!fault length and width, dl, dw, stress_ref
!Note: Force program to use Wells and Coppersmith (WC) for FL and/or FW if
! either entry = 0.0.
! If Wells and Coppersmith are used to obtain FL and/or FW, the WC values are
! modified to account for the scaling implied by differences in the stress
! specified above and a stress that is assumed to be valid for the generic WC
! relations; this stress is stress_ref. The value of 70 bars is an educated
! guess for stress_ref, but it is not based on a quantitative analysis.
! The WC values of FL and/or FW are multiplied by the factor
! (stress_ref/stress)^(1/3).
! Note that four entries on the following line are needed as placeholders,
! even if not used)
  0.0 0.0 1.5 1.5 70.0 !used for WC scaling (need an entry as a placeholder even if not used)
!fq
   10.0
!coord flag (1=lat,long; 2=R,Az; 3=N,E)
   2                      	
!If "Y" below and strike = 0.0:
!  if coord flag = 2, move origin of the radial line to the midpoint of
!                         the top edge of the fault
!  if coord flag = 3 and coord(1) = 0, redefine coord(1) to be
!                         the midpoint of the top edge of the fault (so that the
!                         sites will be along a line normal to the midpoint
!  if coord flag = 3 and coord(2) = 0, redefine coord(1) to be the
!                         far end of the fault, so that the sites are along a line
!                         along the strike of the fault 
 Y
!FractionalDamping	PerOsc	M    stress  coord(1)	coord(2)
0.05	-1	7	250	1.00	45
0.05	-1	7	250	1.12	45
0.05	-1	7	250	1.26	45
0.05	-1	7	250	1.41	45
0.05	-1	7	250	1.58	45
0.05	-1	7	250	1.78	45
0.05	-1	7	250	2.00	45
0.05	-1	7	250	2.24	45
0.05	-1	7	250	2.51	45
0.05	-1	7	250	2.82	45
0.05	-1	7	250	3.16	45
0.05	-1	7	250	3.55	45
0.05	-1	7	250	3.98	45
0.05	-1	7	250	4.47	45
0.05	-1	7	250	5.01	45
0.05	-1	7	250	5.62	45
0.05	-1	7	250	6.31	45
0.05	-1	7	250	7.08	45
0.05	-1	7	250	7.94	45
0.05	-1	7	250	8.91	45
0.05	-1	7	250	10.00	45
0.05	-1	7	250	11.22	45
0.05	-1	7	250	12.59	45
0.05	-1	7	250	14.13	45
0.05	-1	7	250	15.85	45
0.05	-1	7	250	17.78	45
0.05	-1	7	250	19.95	45
0.05	-1	7	250	22.39	45
0.05	-1	7	250	25.12	45
0.05	-1	7	250	28.18	45
0.05	-1	7	250	31.62	45
0.05	-1	7	250	35.48	45
0.05	-1	7	250	39.81	45
0.05	-1	7	250	44.67	45
0.05	-1	7	250	50.12	45
0.05	-1	7	250	56.23	45
0.05	-1	7	250	63.10	45
0.05	-1	7	250	70.79	45
0.05	-1	7	250	79.43	45
0.05	-1	7	250	89.13	45
0.05	-1	7	250	100.00	45
0.05	-1	7	250	112.20	45
0.05	-1	7	250	125.89	45
0.05	-1	7	250	141.25	45
0.05	-1	7	250	158.49	45
0.05	-1	7	250	177.83	45
0.05	-1	7	250	199.53	45
0.05	-1	7	250	223.87	45
0.05	-1	7	250	251.19	45
0.05	-1	7	250	281.84	45
0.05	-1	7	250	316.23	45
0.05	-1	7	250	354.81	45
0.05	-1	7	250	398.11	45
0.05	-1	7	250	446.68	45
0.05	0	7	250	1.00	45
0.05	0	7	250	1.12	45
0.05	0	7	250	1.26	45
0.05	0	7	250	1.41	45
0.05	0	7	250	1.58	45
0.05	0	7	250	1.78	45
0.05	0	7	250	2.00	45
0.05	0	7	250	2.24	45
0.05	0	7	250	2.51	45
0.05	0	7	250	2.82	45
0.05	0	7	250	3.16	45
0.05	0	7	250	3.55	45
0.05	0	7	250	3.98	45
0.05	0	7	250	4.47	45
0.05	0	7	250	5.01	45
0.05	0	7	250	5.62	45
0.05	0	7	250	6.31	45
0.05	0	7	250	7.08	45
0.05	0	7	250	7.94	45
0.05	0	7	250	8.91	45
0.05	0	7	250	10.00	45
0.05	0	7	250	11.22	45
0.05	0	7	250	12.59	45
0.05	0	7	250	14.13	45
0.05	0	7	250	15.85	45
0.05	0	7	250	17.78	45
0.05	0	7	250	19.95	45
0.05	0	7	250	22.39	45
0.05	0	7	250	25.12	45
0.05	0	7	250	28.18	45
0.05	0	7	250	31.62	45
0.05	0	7	250	35.48	45
0.05	0	7	250	39.81	45
0.05	0	7	250	44.67	45
0.05	0	7	250	50.12	45
0.05	0	7	250	56.23	45
0.05	0	7	250	63.10	45
0.05	0	7	250	70.79	45
0.05	0	7	250	79.43	45
0.05	0	7	250	89.13	45
0.05	0	7	250	100.00	45
0.05	0	7	250	112.20	45
0.05	0	7	250	125.89	45
0.05	0	7	250	141.25	45
0.05	0	7	250	158.49	45
0.05	0	7	250	177.83	45
0.05	0	7	250	199.53	45
0.05	0	7	250	223.87	45
0.05	0	7	250	251.19	45
0.05	0	7	250	281.84	45
0.05	0	7	250	316.23	45
0.05	0	7	250	354.81	45
0.05	0	7	250	398.11	45
0.05	0	7	250	446.68	45
0.05	0.2	7	250	1.00	45
0.05	0.2	7	250	1.12	45
0.05	0.2	7	250	1.26	45
0.05	0.2	7	250	1.41	45
0.05	0.2	7	250	1.58	45
0.05	0.2	7	250	1.78	45
0.05	0.2	7	250	2.00	45
0.05	0.2	7	250	2.24	45
0.05	0.2	7	250	2.51	45
0.05	0.2	7	250	2.82	45
0.05	0.2	7	250	3.16	45
0.05	0.2	7	250	3.55	45
0.05	0.2	7	250	3.98	45
0.05	0.2	7	250	4.47	45
0.05	0.2	7	250	5.01	45
0.05	0.2	7	250	5.62	45
0.05	0.2	7	250	6.31	45
0.05	0.2	7	250	7.08	45
0.05	0.2	7	250	7.94	45
0.05	0.2	7	250	8.91	45
0.05	0.2	7	250	10.00	45
0.05	0.2	7	250	11.22	45
0.05	0.2	7	250	12.59	45
0.05	0.2	7	250	14.13	45
0.05	0.2	7	250	15.85	45
0.05	0.2	7	250	17.78	45
0.05	0.2	7	250	19.95	45
0.05	0.2	7	250	22.39	45
0.05	0.2	7	250	25.12	45
0.05	0.2	7	250	28.18	45
0.05	0.2	7	250	31.62	45
0.05	0.2	7	250	35.48	45
0.05	0.2	7	250	39.81	45
0.05	0.2	7	250	44.67	45
0.05	0.2	7	250	50.12	45
0.05	0.2	7	250	56.23	45
0.05	0.2	7	250	63.10	45
0.05	0.2	7	250	70.79	45
0.05	0.2	7	250	79.43	45
0.05	0.2	7	250	89.13	45
0.05	0.2	7	250	100.00	45
0.05	0.2	7	250	112.20	45
0.05	0.2	7	250	125.89	45
0.05	0.2	7	250	141.25	45
0.05	0.2	7	250	158.49	45
0.05	0.2	7	250	177.83	45
0.05	0.2	7	250	199.53	45
0.05	0.2	7	250	223.87	45
0.05	0.2	7	250	251.19	45
0.05	0.2	7	250	281.84	45
0.05	0.2	7	250	316.23	45
0.05	0.2	7	250	354.81	45
0.05	0.2	7	250	398.11	45
0.05	0.2	7	250	446.68	45
0.05	2	7	250	1.00	45
0.05	2	7	250	1.12	45
0.05	2	7	250	1.26	45
0.05	2	7	250	1.41	45
0.05	2	7	250	1.58	45
0.05	2	7	250	1.78	45
0.05	2	7	250	2.00	45
0.05	2	7	250	2.24	45
0.05	2	7	250	2.51	45
0.05	2	7	250	2.82	45
0.05	2	7	250	3.16	45
0.05	2	7	250	3.55	45
0.05	2	7	250	3.98	45
0.05	2	7	250	4.47	45
0.05	2	7	250	5.01	45
0.05	2	7	250	5.62	45
0.05	2	7	250	6.31	45
0.05	2	7	250	7.08	45
0.05	2	7	250	7.94	45
0.05	2	7	250	8.91	45
0.05	2	7	250	10.00	45
0.05	2	7	250	11.22	45
0.05	2	7	250	12.59	45
0.05	2	7	250	14.13	45
0.05	2	7	250	15.85	45
0.05	2	7	250	17.78	45
0.05	2	7	250	19.95	45
0.05	2	7	250	22.39	45
0.05	2	7	250	25.12	45
0.05	2	7	250	28.18	45
0.05	2	7	250	31.62	45
0.05	2	7	250	35.48	45
0.05	2	7	250	39.81	45
0.05	2	7	250	44.67	45
0.05	2	7	250	50.12	45
0.05	2	7	250	56.23	45
0.05	2	7	250	63.10	45
0.05	2	7	250	70.79	45
0.05	2	7	250	79.43	45
0.05	2	7	250	89.13	45
0.05	2	7	250	100.00	45
0.05	2	7	250	112.20	45
0.05	2	7	250	125.89	45
0.05	2	7	250	141.25	45
0.05	2	7	250	158.49	45
0.05	2	7	250	177.83	45
0.05	2	7	250	199.53	45
0.05	2	7	250	223.87	45
0.05	2	7	250	251.19	45
0.05	2	7	250	281.84	45
0.05	2	7	250	316.23	45
0.05	2	7	250	354.81	45
0.05	2	7	250	398.11	45
0.05	2	7	250	446.68	45
stop  