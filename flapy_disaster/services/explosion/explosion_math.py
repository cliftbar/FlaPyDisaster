import math


def hello():
    ret_string = "This is the explosion math package!  Some help info is below."
    print(ret_string)


def general_bomb_equation(mass_kg, radius_m):
    """
    General Sadovsky bomb overpressure equation, surface explosion at standard atmospheric condidtions.
    :param mass_kg: Mass in kg TNT
    :param radius_m: Distance from explosion in meters
    :returns: Overpressure in atmospheres
    :Reference: BlastEffectCalculation.pdf, Equation 2
    """
    if radius_m == 0 or mass_kg == 0:
        return -1
    return ((0.95 * (math.pow(mass_kg, .33333) / radius_m))
            + (3.9 * math.pow((mass_kg * mass_kg), .33333) / (radius_m * radius_m))
            + (13.0 * mass_kg / (radius_m ** 3.0)))


def newmark_overpressure(energy_mttnt, radius_m):
    """
    Newmark-Hansen Overpressure formula.  Intended for surface blasts, but adapted to air-bursts.
    :param energy_mttnt: Energy in Megatons TNT
    :param radius_m: Actual distance from blast in m (hypotenuse distance for airburst events).
    :returns: overpressure in bar
    :Reference: NuclearBlastOverpressure.pdf, Equation 3
    """

    energy_tnt = energy_mttnt * 1000000
    return (6784 * (energy_tnt / (radius_m ** 3))) + (93 * (math.sqrt(energy_tnt / (radius_m ** 3))))


def radius_from_overpressure(overpressure_bar, energy_tnt, radius_upper_bound_km=1000, error_threshold=0.0001, max_iterations=100):
    """
    Find the radius of a given overpressure for a given event energy.  Lower limit of 0 
    Uses a bisection search to solve the Newmark-Hansen Ovepressure Formula
    :param overpressure_bar: Overpressure in Bars
    :param energy_tnt: Energy in Megatons TNT
    :param radius_upper_bound_km: Upper bound for radius in kilometers. Default value of 1000 km
    :param error_threshold: Error threshold (percentage) to stop bisection search at. Default value of 0.0001
    :param max_iterations: Maximum number of bisection search iterations to run. Default value of 100
    :returns: Radius in km and calculation error in a tuple, in that order
    """

    x_upper = radius_upper_bound_km * 1000
    x_lower = 0.1
    x_mid = 0

    y_mid = 0
    x_old = 1
    i = 0
    error_val = 100

    while True:
        x_mid = (x_upper + x_lower / 2)
        y_mid = newmark_overpressure(energy_tnt, x_mid)

        if x_mid != 0:
            error_val = math.fabs((x_mid - x_old) / x_mid) * 100

        if y_mid < overpressure_bar:
            x_upper = x_mid

        elif y_mid > overpressure_bar:
            x_lower = x_mid
        else:
            return [x_mid / 1000, error_val]

        i += 1

        if error_val <= error_threshold or i > max_iterations:
            return [x_mid / 1000, error_val]
