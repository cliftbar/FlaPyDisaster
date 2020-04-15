!Control file for program reff
! Revision of program involving a change in the control file on this date:
   02/17/09
!MW, Stress 
  7.0 140.0       	
!lat and lon of upper edge of fault
  0.0 0.0  
!strike,dip, depth of fault
  0.0 90.0 0.0            	
!fault type (S=strikeslip; R=reverse; N=normal; U=undifferentiated) 
! (Only used if Wells and Coppersmith is used to obtain FL and FW).
  R                               
!fault length and width, dl, dw, stress_ref
!Note: Force program to use Wells and Coppersmith for FL and/or FW if either entry = 0.0
  29.4 9.0 1.5 1.5 70.0 ! used for WC scaling (need an entry as a placeholder even if not used_
!gsprd: r_ref, nsegs, (rlow(i), a_s, b_s, m_s(i))  (Usually set r_ref = 1.0 km)
    1.0
    3
      1.0 -1.3 0.0 6.5
     70.0 +0.2 0.0 6.5
    140.0 -0.5 0.0 6.5
!q: fr1, Qr1, s1, ft1, ft2, fr2, qr2, s2, c_q
   1.0 1000.0 0.0 1.4242 1.4242 1.0 893.0 0.32 3.7   
!fq
   10.0
!Output file names stem
  reff_m7.0_140b_dip_90_fl_29.4_fw_9.0_dl_dw_1.5
!Number of Sites, coord flag (1=lat,long; 2=R,Az; 3=N,E)
   4 3                      	
!Coordinates of each site
 14.7 2.5
 14.7 160.0
 31.9 0.0
 189.4 0.0
!Move the sites to the midpoint and end of the surface projection 
!of the upper edge of the fault if isitecoordflag = 3 and 
!siteLocation(1) = 0 (center) and siteLocation(2) = 0 (end), respectively
!(this only makes sense if the strike of the fault = 0.0)?
 N

!Number of Sites, coord flag (1=lat,long; 2=R,Az; 3=N,E)
   54 2                      	
!Coordinates of each site
 1              45
 1.122018454	45
 1.258925412	45
 1.412537545	45
 1.584893192	45
 1.77827941	45
 1.995262315	45
 2.238721139	45
 2.511886432	45
 2.818382931	45
 3.16227766	45
 3.548133892	45
 3.981071706	45
 4.466835922	45
 5.011872336	45
 5.623413252	45
 6.309573445	45
 7.079457844	45
 7.943282347	45
 8.912509381	45
 10             45
 11.22018454	45
 12.58925412	45
 14.12537545	45
 15.84893192	45
 17.7827941	45
 19.95262315	45
 22.38721139	45
 25.11886432	45
 28.18382931	45
 31.6227766	45
 35.48133892	45
 39.81071706	45
 44.66835922	45
 50.11872336	45
 56.23413252	45
 63.09573445	45
 70.79457844	45
 79.43282347	45
 89.12509381	45
 100            45
 112.2018454	45
 125.8925412	45
 141.2537545	45
 158.4893192	45
 177.827941	45
 199.5262315	45
 223.8721139	45
 251.1886432	45
 281.8382931	45
 316.227766	45
 354.8133892	45
 398.1071706	45
 446.6835922	45
!Move the sites to the midpoint and end of the surface projection 
!of the upper edge of the fault if isitecoordflag = 3 and 
!siteLocation(1) = 0 (center) and siteLocation(2) = 0 (end), respectively
!(this only makes sense if the strike of the fault = 0.0)?
 Y


!Number of Sites, coord flag (1=lat,long; 2=R,Az; 3=N,E)
   4 3                     	
!Coordinates of each site
 0.0 2.5
 0.0 160.0
 2.5 0.0
 160.0 0.0
!Move the sites to the midpoint and end of the surface projection 
!of the upper edge of the fault if isitecoordflag = 3 and 
!siteLocation(1) = 0 (center) and siteLocation(2) = 0 (end), respectively
!(this only makes sense if the strike of the fault = 0.0)?
 Y


  