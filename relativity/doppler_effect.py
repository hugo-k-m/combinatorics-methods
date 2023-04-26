import math

# c = 299792458  # speed of light
c = 3e8  # speed of light


def emitting_frequency(f0, v):
    """
    Calculates the frequency of a spectral line emitted by an object moving at a velocity v relative to the observer.

    Parameters:
        f0 (float): The rest frequency of the spectral line in Hz.
        v (float): The velocity of the emitting object relative to the observer in m/s.

    Returns:
        float: The frequency of the spectral line emitted by the object in Hz.
    """
    f_emitting = f0 * math.sqrt((c + v) / (c - v))

    return f_emitting


def receding_frequency(f0, v):
    """
    Calculates the frequency of a spectral line emitted by an object moving away from the observer at a velocity v.

    Parameters:
        f0 (float): The rest frequency of the spectral line in Hz.
        v (float): The velocity of the emitting object relative to the observer in m/s.

    Returns:
        float: The frequency of the spectral line observed by the observer in Hz.
    """
    f_receding = f0 * math.sqrt((c - v) / (c + v))
    return f_receding


def doppler_velocity(f0, f):
    delta_lambda_over_lambda0 = (f - f0) / f0

    return c * delta_lambda_over_lambda0


f0 = 1.5e6
f = 1.8e6
vel = -c * (f0**2 - f**2) / (f0**2 + f**2) / c

print(f"Velocity: {vel}c")
