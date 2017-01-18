import math


"""
SPH: Standard Project Hurricane
PMH: Probable Maximum Hurricane

Pressure profile equation

(p-Cp)/(Pw - Cp) = e ^ (-R/r)

Pw: Peripheral Pressure, pressure at edge of storm, should be a bit below MSLP
Cp: Central Pressure (P0 in paper)
Rmax: Radius of Maximum Winds (R in paper)
Fspeed: Forward speed of hurricane center (T in paper)
Dir: Track direction
Vgx: Maximum Gradient Winds
Rho0: Surace air density
r: distance (radius) from hurricane center
fcorr: Coriolis parameter, dependant on latitude
Vx: Observed maximum 10-m, 10-min winds over open water.  75% to 105% of Vgx.  Standard is 95%
    For moving hurricane: Vx = 0.95 * Vgx + (1.5 * T ^ 0.63 * To ^ 0.37 * cos(beta)
Vpt: 10m, 10min winds at a point (V in paper)
"""
Pw_SPH_kPa = 100.8
Pw_PMH_kPa = 102.0
Pw_SPH_inhg = 29.77
Pw_PMH_inhg = 30.12
Rho0_kPa = 101.325  # Mean Sea Level Pressure
KmToNmi = 0.539957
MpsToKts = 1.94384
KpaToInhg = 0.2953
MbToInhg = 0.02953


def linear_interpolation(x, x1, x2, y1, y2):
    """

    :param x:
    :param x1:
    :param x2:
    :param y1:
    :param y2:
    :return:
    """
    return ((y2 - y1) / (x2 - x1)) * (x - x1)


def radial_decay(r_nmi, rmax_nmi):
    """
    Calculates the radial decay factor for a given radius.
    Rmax_nmi < r_nmi: NWS 23 pdf page 53, page 27, Figure 2.12, empirical fit
    Rmax_nmi > r_nmi: NWS 23 pdf page 54, page 28, Figure 2.13, empirical fit (logistic regression)
    :param r_nmi: int Point radius from center of storm in nautical miles
    :param rmax_nmi: int Radius of maximum winds in nautical miles
    :return: float 0 <= radial decay <= 1
    """

    # ret = 0
    if r_nmi >= rmax_nmi:
        # NWS 23 pdf page 53
        slope = (-0.051 * math.log(rmax_nmi)) - 0.1757
        intercept = (0.4244 * math.log(rmax_nmi)) + 0.7586
        ret = (slope * math.log(r_nmi)) + intercept
    else:
        # NWS 23 pdf page 54
        # ret = 1.01231578 / (1 + math.exp(-8.612066494 * ((r_nmi / float(rmax_nmi)) - 0.678031222)))
        ret = 1 # this is a concession for modeling time series, where everything within the max wind radius is expected to experience the max wind radius while the storm translates

    # keep radial decay between 0 and 1
    ret = max(min(ret, 1), 0)
    return ret


def coriolis_frequency(lat_deg):
    """
    Calculate the coriolis factor for a given latitude
    :param lat_deg: float deg
    :return: float hr**-1 coriolis factor
    """
    w = 2.0 * math.pi / 24
    return 2.0 * w * math.sin(math.radians(lat_deg))


def k_density_coefficient(lat_deg):
    """
    NWS 23 pdf page 50, page 24, figure 2.10, emperical relationship (linear regression)
    This is for the PMH, We can also improve this relationship
    This is what I thought, but apparently not: (1.0/(Rho0_kPa * math.e)) ** (0.5)
    DEP: lat 24, K 68.1; lat 45, K 65
        SPH: (65-68.1)/(45-24) = -0.147619048
        PMH: (66.2 - 70.1)/(45 - 24) = -0.185714286

    :param lat_deg:
    :return: float (kts) K factor for kts, In. Hg
    """

    # return 70.1 + -0.185714286 * (lat_deg - 24.0)
    return 69.1952184 / (1 + math.exp(0.20252 * (lat_deg - 58.72458)))


def gradient_wind_at_radius(pw_inhg, cp_inhg, r_nmi, lat_deg):
    """
    NWS 23 pdf page 49, page 23, equation 2.2
    :param pw_inhg: float Peripheral Pressure, pressure at edge of storm, should be near MSLP, In. Hg
    :param cp_inhg: float Central Pressure in In. Hg
    :param r_nmi: int Radius from center of storm in nautical miles.  Use Radius of max winds (Rmax) to get maximum gradient wind
    :param lat_deg: int deg Latitude of hurricane eye
    :return:
    """

    k = k_density_coefficient(lat_deg)
    f = coriolis_frequency(lat_deg)
    return k * ((pw_inhg - cp_inhg) ** 0.5) - (r_nmi * f) / 2


def asymmetry_factor(fspeed_kts, r_nmi, rmax_nmi, angle_from_center, track_bearing):
    """
    NWS 23 pdf page 51, page 25, equation 2.5
    NWS 23 pdf page 263, page 269
    NWS 23 pdf page 281, page 257
    Factor for a moving hurricane, accounts for effect of forward speed on hurricane winds
    To conversion factors: 1 kt, 0.514791 mps, 1.853248 kph, 1.151556 mph
    :param fspeed_kts: float kts Forward speed of the storm
    :param r_nmi: int Radius from the center of the storm in nautical miles
    :param rmax_nmi: int Radius of maximum winds in nautical miles
    :param angle_from_center: float deg
    :param track_bearing: float deg
    :return: float Asymmetry Factor
    """

    to = 1
    phi_r = inflow_angle(r_nmi, rmax_nmi)  # need to figure out direction
    phi_rmax = inflow_angle(rmax_nmi, rmax_nmi)  # need to figure out direction
    phi_beta = (phi_r - phi_rmax) % 360
    bearing_shift = (90 - angle_from_center + track_bearing) % 360
    beta = (phi_beta + bearing_shift) % 360
    # print("Phi_r: {0}, Phi_rmax: {1}, beta: {2}, bearing_shift: {3}".format(phi_r, phi_rmax, beta, bearing_shift))
    asym = 1.5 * (fspeed_kts ** 0.63) * (to ** 0.37) * math.cos(math.radians(beta))

    return asym
    # return beta


def inflow_angle(r_nmi, rmax_nmi):
    """
    Emperical inflow angle calculation of PMH
    NWS 23 pdf page 55
    NOAA_NWS23_Inflow_Calc.xlsx
    :param rmax_nmi: int Radius of maximum winds in Nautical Miles
    :param r_nmi: int Point radius from hurricane center in Nautical Miles
    :return: float deg Inflow angle
    """

    # phi = None
    r_phi_max = (3.0688 * rmax_nmi) - 2.7151
    if r_nmi < r_phi_max:
        a = 11.438 * (rmax_nmi ** -1.416)
        b = (1.1453 * rmax_nmi) + 1.4536
        phi_max = 9.7043566358 * math.log(rmax_nmi) - 2.7295806727
        phi = phi_max / (1 + math.exp(-1 * a * (r_nmi - b)))
    else:  # following
        r_nmi_use = min(r_nmi, 130)

        x1 = (0.0000896902 * rmax_nmi * rmax_nmi) - (0.0036924418 * rmax_nmi) + 0.0072307906
        x2 = (0.000002966 * rmax_nmi * rmax_nmi) - (0.000090532 * rmax_nmi) - 0.0010373287
        x3 = (-0.0000000592 * rmax_nmi * rmax_nmi) + (0.0000019826 * rmax_nmi) - 0.0000020198
        c = (9.7043566341 * math.log(rmax_nmi)) - 2.7295806689
        phi = (x3 * ((r_nmi_use - r_phi_max) ** 3)) + (x2 * ((r_nmi_use - r_phi_max) ** 2)) + (x1 * (r_nmi_use - r_phi_max)) + c
        if 130 < r_nmi < 360:  # justification on NWS23 pdf page 287 page 263
            delta_phi = linear_interpolation(r_nmi, 130, 360, phi, (phi - 2))
            phi += delta_phi
        elif 360 <= r_nmi:
            phi -= 2
    return phi


def calc_windspeed(cp_mb, r_nmi, lat_deg, fspeed_kts, rmax_nmi, angle_to_center, track_heading, pw_kpa=Pw_PMH_kPa, vmax_kts=None, gwaf=0.9):
    """
    Calculate the windspeed at a given point from parameters
    :param cp_mb: float mb central pressure
    :param r_nmi: int n. mi. Point radius from center of storm
    :param lat_deg: int deg latitude of hurricane eye # todo Check This
    :param fspeed_kts: float kts Forward speed of the storm
    :param rmax_nmi: int n.mi.  Radius of maximum winds
    :param angle_to_center: float deg Simple angle from point to center of storm, in bearing notation (North = 0)
    :param track_heading: float def eading of track from current point to next point, except for the last point, which uses the previous heading
    :param pw_kpa: float mb Peripheral Pressure, pressure at edge of storm, should be near MSLP
    :param vmax_kts: int kts Input max windspeed to skip the calculation for it.  Useful when Vmax is know for a storm.
    :param gwaf: float  Gradient Wind Adjustment Factor, semi-emprical adjustment to the Gradient Wind. Range 0.75-1.05, Generally between 0.9 and 1. NWS 23 pdf page 50, page 24, 2.2.7.2.1
    :return: float kts Windspeed at a given radius for the storm
    """
    if cp_mb is None and vmax_kts is None:
        raise ValueError('No vmax and no cp')

    if cp_mb is not None:
        cp_inhg = cp_mb * KpaToInhg
    pw_inhg = pw_kpa * KpaToInhg

    # Step 1: Calculate Maximum Gradient Windspeed if unknown, 10m-10min Average
    vgx = 0
    if vmax_kts is None:
        vgx = gradient_wind_at_radius(pw_inhg, cp_inhg, rmax_nmi, lat_deg)
    else:
        vgx = vmax_kts
    # Step 2: Calculate the Radial Decay
    radial_decay_factor = radial_decay(r_nmi, rmax_nmi)  # need to convert to nmi
    # Step 3: Calculate the Asymmetry Factor
    asym = asymmetry_factor(fspeed_kts, r_nmi, rmax_nmi, angle_to_center, track_heading)

    # apply all factors and return windspeed at point
    windspeed_kts = (vgx * gwaf * radial_decay_factor) + asym
    return windspeed_kts
    # return asym + 100
