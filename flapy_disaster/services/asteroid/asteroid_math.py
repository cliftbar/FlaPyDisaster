import math

#############
# Constants #
#############
# Joules per megaton tnt
JoulesPerMegatonTNT = 4184000000000000
# Surface atmospheric density, kg/m^3
RhoZero = 1
# Scale height, assumed to be 8000m on average
H = 8000
# Drag coefficient for pre-breakup phase
CD = 2
# "Pancake Factor", represents the ratio of diameter to debris dispersion after airburst
FP = 7
# Pi...
PI = 3.1415


#############
# Functions #
#############
def hello():
    ret_string = "This is the asteroid math package!  Some help info is below."
    print(ret_string)


def kenetic_energy(impactor_density_kgpm3, diameter_m, initial_velocity_mps):
    """
    Impact energy in Joules.
    :param impactor_density_kgpm3: impactor density in kg/m^3
    :param diameter_m: impactor diameter in meters
    :param initial_velocity_mps: initial velocity in m/s
    :returns: Kenetic energy in Joules (kg-m^2/s^2)
    :Reference: EarthImpactEffect.pdf, Equation 1*
    """

    return (PI / 12) * impactor_density_kgpm3 * (diameter_m ** 3) * (initial_velocity_mps * initial_velocity_mps)


def joules_to_megaton_tnt(energy_j):
    """
    Convert Joules to Megatons TNT.
    :param energy_j: Energy in Joules(kg-m^2/s^2)
    :returns: Energy in Megatons TNT
    """

    return energy_j / JoulesPerMegatonTNT


def return_period_earth(energy_mttnt):
    """
    Return period of Asteroid/Comet of a given energy (Megatons TNT) in Years.
    :param energy_mttnt: Energy in Megatons TNT
    :returns: Return period of given energy level in years
    :Reference: EarthImpactEffect.pdf, Equation 3*
    """

    return 109 * (energy_mttnt ** 0.78)


def breakup_altitude(impactor_density_kgpm3, diameter_m, velocity_mps, angle_rad):
    """
    Altitude where air stagnation pressure surpasses Asteriod strength.  Precursor to Airburst.
    :param impactor_density_kgpm3: Impactor Density in kg/m^3
    :param diameter_m: Impactor diameter in meters
    :param velocity_mps: impactor velocity in m/s
    :param angle_rad: impactor approach angle above tangent plane in radians. 90 deg, PI/2 is straight down
    :returns: breakup altitude in m.
    :Reference: EarthImpactEffect.pdf, Equation 11*
    """

    yi = yield_strength(impactor_density_kgpm3)
    if_term = max(calc_if_term(impactor_density_kgpm3, diameter_m, velocity_mps, angle_rad), 0)

    z_star = 0
    if if_term <= 1:
        z_star = -H * (math.log(yi / (velocity_mps * velocity_mps)) + 1.308 - (0.314 * if_term) - (1.303 * math.sqrt(1 - if_term)))

    return z_star


def yield_strength(impactor_density_kgpm3):
    """
    Yield strength equation for breakup altitude calculation. Only valid for density range 1000 to 8000.
    :param impactor_density_kgpm3: Impactor density in kg/m^3
    :returns: Yield Strength in Pascals.
    :Reference: EarthImpactEffect.pdf, Equation 10
    """

    return 10 ** (2.107 + (0.0624 * math.sqrt(impactor_density_kgpm3)))


def calc_if_term(impactor_density_kgpm3, diameter_m, velocity_mps, angle_rad):
    """
    If term for breakup altitude equation.
    :param impactor_density_kgpm3: Impactor density in kg/m^3
    :param diameter_m: Impactor diameter in meters
    :param velocity_mps: Impactor velocity in km/s
    :param angle_rad: impactor approach angle above tangent plane in radians. 90 deg, PI/2 is straight down
    :returns: If term for breakup altitude equation
    :Reference: EarthImpactEffect.pdf, Equation 12*
    """

    numerator = 4.07 * CD * H * yield_strength(impactor_density_kgpm3)
    denominator = impactor_density_kgpm3 * diameter_m * (velocity_mps * velocity_mps) * math.sin(angle_rad)
    return numerator / denominator


def atmospheric_density(altitude_m):
    """
    Returns the atmospheric density at a given height in kg/m^3
    :param altitude_m: input height in meters
    :returns: density of atmosphere in kg.m^3
    :Reference: EarthImpactEffect.pdf, Equation 5
    """
    return RhoZero * math.exp(-1 * altitude_m / H)


def airburst_altitude(breakup_altitude_m, diameter_m, impactor_density_kgpm3, angle_rad):
    """
    Altitude of Airburst, occurs after Breakup.
    :param breakup_altitude_m: Breakup Altitude in meters
    :param diameter_m: Impactor diameter in meters
    :param impactor_density_kgpm3: Impactor diameter in kg/m^3
    :param angle_rad: impactor approach angle above tangent plane in radians. 90 deg, PI/2 is straight down
    :returns: Airbust height in meters.  If zero, there is no airburst
    """

    air_density_at_breakup = atmospheric_density(breakup_altitude_m)
    l = diameter_m * math.sin(angle_rad) * math.sqrt(impactor_density_kgpm3 / (CD * air_density_at_breakup))
    second_term = 2 * H * math.log(1 + ((l / (2 * H)) * math.sqrt((FP * FP) - 1)))

    if breakup_altitude_m > second_term:
        return breakup_altitude_m - second_term
    else:
        return 0


def velocity_at_altitude_pre_breakup(altitude_m, init_velocity_mps, diameter_m, impactor_density_kgpm3, angle_rad):
    """
    Calculates the velocity at a given height, as the object experiences drag, valid before the impactor breaks up.
    :param altitude_m: input height in meters
    :param init_velocity_mps: Velocity when first impacting the atmosphere.
    :param diameter_m: diameter of impactor
    :param impactor_density_kgpm3: impactor density in kg/m^3
    :param angle_rad: impactor angle in radians
    :returns: velocity at the given height in m/s
    :Reference: EarthImpactEffect.pdf, Equation 8
    """
    return init_velocity_mps * math.exp((-3 * atmospheric_density(altitude_m) * CD * H) / (4 * impactor_density_kgpm3 * diameter_m * math.sin(angle_rad)))


def post_breakup_velocity(breakup_altitude_m, breakup_velocity_mps, diameter_m, impactor_density_kgpm3, angle_rad, is_airburst):
    """
    Calculates the velocity directly after a breakup or airburst occurs.
    :param breakup_altitude_m: Height that the breakup occurs in meters
    :param breakup_velocity_mps: Velocity when breakup occurs in m/s
    :param diameter_m: Impactor diameter in meters
    :param impactor_density_kgpm3: Impactor density in kg/m^3
    :param angle_rad: Impactor appreak angle in radians
    :param is_airburst: Whether the event is an airburst.  If an airburst occurs, a the velocity given is the velocity directly after the airburst.
                        If no airburst occurs, the velocity given is the velocity at impact.  
    :Reference: EarthImpactEffect.pdf, Equation 17*, 19*, 20; Fig. 2 a, b
    """
    second_term = 0
    atmo_density_at_breakup = atmospheric_density(breakup_altitude_m)
    l_term = diameter_m * math.sin(angle_rad) * math.sqrt(impactor_density_kgpm3 / (CD * atmo_density_at_breakup))
    if is_airburst:
        alpha_term = math.sqrt((FP * FP) - 1)
        second_term = (l_term * diameter_m * diameter_m * alpha_term / 24) * ((8 * (3 + (alpha_term * alpha_term))) + ((3 * alpha_term * l_term / H) * (2 + (alpha_term * alpha_term))))
    else:
        second_term = ((H ** 3) * diameter_m * diameter_m) / (3 * l_term * l_term) * ((3 * (4 + ((l_term / H) ** 2)) * math.exp(breakup_altitude_m / H)) + (6 * math.exp(2 * breakup_altitude_m / H)) - (16 * math.exp(2 * breakup_altitude_m / H)) - (3 * ((l_term / H) ** 2)) - 2)

    airburst_velocity_mps = breakup_velocity_mps * math.exp((-3 * atmo_density_at_breakup * CD) / (4 * impactor_density_kgpm3 * (diameter_m ** 3) * math.sin(angle_rad)) * second_term)
    return airburst_velocity_mps
