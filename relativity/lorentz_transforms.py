class LorentzTransform:
    """
    A class that encapsulates the Lorentz transformations for time and space.

    Attributes:
        c (float): The speed of light in meters per second.
    """

    def __init__(self, c=299792458):
        self.c = c

    def gamma(self, v):
        """
        Calculate the Lorentz factor.

        Args:
            v (float): The relative speed between the two frames in meters per second.

        Returns:
            float: The Lorentz factor.
        """

        return 1 / ((1 - (v**2 / self.c**2)) ** 0.5)

    def time_transform(self, t, x, v):
        """
        Calculate the time coordinate of an event in a moving frame.

        Args:
            t (float): The time coordinate of the event in the stationary frame, in seconds.
            x (float): The x-coordinate of the event in the stationary frame, in meters.
            v (float): The relative speed between the two frames in meters per second.

        Returns:
            float: The time coordinate of the event in the moving frame, in seconds.
        """

        gamma = self.gamma(v)

        return gamma * (t - v * x / self.c**2)

    def space_transform(self, t, x, v):
        """
        Calculate the space coordinate of an event in a moving frame.

        Args:
            t (float): The time coordinate of the event in the stationary frame, in seconds.
            x (float): The x-coordinate of the event in the stationary frame, in meters.
            v (float): The relative speed between the two frames in meters per second.

        Returns:
            float: The x-coordinate of the event in the moving frame, in meters.
        """

        gamma = self.gamma(v)

        return gamma * (x - v * t)


lt = LorentzTransform()
x = 9  # meters
t = 30e-9  # seconds
v = 0.6 * lt.c  # meters per second
# v = 0.6 * 3 * 10**8  # meters per second
# t_prime = lt.time_transform(t, x, v)
x_prime = lt.space_transform(t, x, v)

print("x' =", x_prime, "meters")
