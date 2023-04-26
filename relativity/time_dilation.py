import math

c = 299792458


def time_interval_observer(t_prime, v):
    """
    Computes the time interval experienced by the observer on Earth, given the time
    interval experienced by a moving object and the velocity of the object relative to the observer.

    Args:
        t_prime (float): Time interval experienced by the moving object
        v (float): Velocity of the moving object relative to the observer

    Returns:
        float: Time interval experienced by the observer on Earth
    """
    return t_prime / math.sqrt(1 - (v**2 / (c**2)))


def time_interval_moving_object(t, v):
    """
    Computes the time interval experienced by a moving object, given the time interval
    experienced by the observer on Earth and the velocity of the object relative to the observer.

    Args:
        t (float): Time interval experienced by the observer on Earth
        v (float): Velocity of the moving object relative to the observer

    Returns:
        float: Time interval experienced by the moving object
    """
    return t * math.sqrt(1 - (v**2 / (c**2)))


def velocity(t, t_prime):
    """
    Computes the velocity of a moving object relative to an observer, given the time intervals
    experienced by the observer and the moving object.

    Args:
        t (float): Time interval experienced by the observer on Earth
        t_prime (float): Time interval experienced by the moving object

    Returns:
        float: Velocity of the moving object relative to the observer
    """
    v = c * math.sqrt(1 - ((t_prime / t) ** 2))
    return v


# Example 1
print("Example 1")
t_prime = 60
v = 0.8 * c
observer_time = time_interval_observer(t_prime, v)
print(observer_time, end="\n\n")

print("Example 2")
t_prime = 1
v = 5830
observer_time = time_interval_observer(t_prime, v)
print(observer_time, end="\n\n")

print("Example 3")
t_prime = 30
t = 100
vel = velocity(t, t_prime) / c
print(vel, end="\n\n")
