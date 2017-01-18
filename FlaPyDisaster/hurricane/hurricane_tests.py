import unittest as ut
import hurricane.hurricane_nws23 as nws


class Nws23Tests(ut.TestCase):
    """
    NWS 23 pdf page 82 page 56
    """
    def test_coriolis(self):
        self.assertEqual(nws.coriolis_frequency(33.5), 0.28899352971680986)  # NWS23: 0.290
        self.assertEqual(nws.coriolis_frequency(-60), -0.4534498410585544)

    def test_k_density_coefficient(self):
        self.assertEqual(nws.k_density_coefficient(33.5), 68.77942934780634)  # NWS23: 68.8

    def test_gradient_wind_at_radius(self):
        self.assertEqual(nws.gradient_wind_at_radius(30.12, 26.31, 15, 33.5), 132.08463873197573)  # NWS23: 132.1 (kts)

    def test_radial_decay(self):
        # r >= r_max
        self.assertEqual(nws.radial_decay(15, 15), 1)  # NWS23: 1
        self.assertEqual(nws.radial_decay(30, 15), 0.8405648494664133)  # NWS23: 0.870
        self.assertEqual(nws.radial_decay(60, 15), 0.6230479443948826)  # NWS23: 0.590
        self.assertEqual(nws.radial_decay(100, 15), 0.4627454692076476)  # NWS23: 0.428
        self.assertEqual(nws.radial_decay(200, 15), 0.24522856413611716)  # NWS23: 0.25
        self.assertEqual(nws.radial_decay(300, 15), 0.11798933139634826)  # NWS23: 0.158

        # r < r_max
        self.assertEqual(nws.radial_decay(13.5, 15), 0.8819294073223086)  # NWS23: 0.937
        self.assertEqual(nws.radial_decay(10.5, 15), 0.5538972410933576)  # NWS23: 0.491
        self.assertEqual(nws.radial_decay(7.5, 15), 0.1797098691063691)  # NWS23: 0.206
        self.assertEqual(nws.radial_decay(4.5, 15), 0.037582331274012275)  # NWS23: 0.06
        self.assertEqual(nws.radial_decay(1.5, 15), 0.006924657565759972)  # NWS23: 0.01

    def test_inflow_angle(self):
        self.assertEqual(nws.inflow_angle(1.5, 15), 0.33617743822056195)  # NWS23: 0.3
        self.assertEqual(nws.inflow_angle(10.5, 15), 2.7818954726287375)  # NWS23: 4.0
        self.assertEqual(nws.inflow_angle(15, 15), 6.816851496756403)  # NWS23: 7.2
        self.assertEqual(nws.inflow_angle(30, 15), 22.212487494795912)  # NWS23: 23.6
        self.assertEqual(nws.inflow_angle(60, 15), 22.669510325613956)  # NWS23: 24.5
        self.assertEqual(nws.inflow_angle(100, 15), 19.03507904284718)  # NWS23: 20.9
        self.assertEqual(nws.inflow_angle(200, 15), 16.911468607774182)  # NWS23: 15.9
        self.assertEqual(nws.inflow_angle(300, 15), 16.041903390382878)  # NWS23: 14.2

    def test_asymmetry_factor(self):
        # Along max radial
        self.assertEqual(nws.asymmetry_factor(10, 1.5, 15, 90, 0), 6.357805024754615)  # NWS23: 6.4
        self.assertEqual(nws.asymmetry_factor(10, 10.5, 15, 90, 0), 6.382832382799295)  # NWS23: 6.4
        self.assertEqual(nws.asymmetry_factor(10, 15, 15, 90, 0), 6.398692782023891)  # NWS23: 6.4
        self.assertEqual(nws.asymmetry_factor(10, 30, 15, 90, 0), 6.16907970888782)  # NWS23: 6.2
        self.assertEqual(nws.asymmetry_factor(10, 60, 15, 90, 0), 6.155333491544212)  # NWS23: 6.1
        self.assertEqual(nws.asymmetry_factor(10, 100, 15, 90, 0), 6.25375353422354)  # NWS23: 6.2
        self.assertEqual(nws.asymmetry_factor(10, 200, 15, 90, 0), 6.299638785778841)  # NWS23: 6.3
        self.assertEqual(nws.asymmetry_factor(10, 300, 15, 90, 0), 6.315933782554578)  # NWS23: 6.4
        # Along other radial
        self.assertEqual(nws.asymmetry_factor(10, 1.5, 15, 60, 0), 5.867124729746776)  # NWS23: 5.9
        self.assertEqual(nws.asymmetry_factor(10, 10.5, 15, 60, 0), 5.752817231625939)  # NWS23: 5.7
        self.assertEqual(nws.asymmetry_factor(10, 15, 15, 60, 0), 5.541430500244814)  # NWS23: 5.5
        self.assertEqual(nws.asymmetry_factor(10, 30, 15, 60, 0), 4.4932086749994635)  # NWS23: 4.4
        self.assertEqual(nws.asymmetry_factor(10, 60, 15, 60, 0), 4.4567273870172075)  # NWS23: 4.3
        self.assertEqual(nws.asymmetry_factor(10, 100, 15, 60, 0), 4.738813415051242)  # NWS23: 4.6
        self.assertEqual(nws.asymmetry_factor(10, 200, 15, 60, 0), 4.894884241259593)  # NWS23: 5.0
        self.assertEqual(nws.asymmetry_factor(10, 300, 15, 60, 0), 4.956862971282132)  # NWS23: 5.1

    # def test_coriolis(self):
    #     self.assertTrue(False, 'False')

if __name__ == '__main__':
    ut.main()
