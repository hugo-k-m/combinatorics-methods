import numpy as np


def muller_method(p0, p1, p2, f, TOL, N):
    """
    Muller's method for approximating roots of f.

    Args:
        p0 (float): initial guess for root.
        p1 (float): second initial guess for root.
        p2 (float): third initial guess for root.
        f (function): function to approximate.
        TOL (float): tolerance for approximation.
        N (int): maximum number of iterations.

    Returns:
        float: approximate root p or message of failure.
    """
    # Set initial values
    h1 = p1 - p0
    h2 = p2 - p1
    delta1 = (f(p1) - f(p0)) / h1
    delta2 = (f(p2) - f(p1)) / h2
    i = 3

    # Start the loop to iteratively refine the approximation of the root
    while i <= N:
        # Calculate the estimate of the derivative at p2 using a finite difference formula
        d = (delta2 - delta1) / (h2 + h1)

        # Calculate the coefficients for the quadratic equation using the current estimates of the root
        b = delta2 + h2 * d
        D = (b**2 - 4 * f(p2) * d) ** (1 / 2)

        # Choose the sign of the square root that gives a larger absolute value for the denominator
        if abs(b - D) < abs(b + D):
            E = b + D
        else:
            E = b - D

        # Calculate the new estimate of the root using the quadratic formula
        h = -2 * f(p2) / E
        p = p2 + h

        # Check if the new estimate of the root is within the desired tolerance
        if abs(h) < TOL:
            return (f"The approximate root is {p}.", i)

        # Update values for the next iteration
        p0 = p1
        p1 = p2
        p2 = p
        h1 = p1 - p0
        h2 = p2 - p1
        delta1 = (f(p1) - f(p0)) / h1
        delta2 = (f(p2) - f(p1)) / h2
        i += 1

    # Return a message of failure if the method fails to converge within the maximum number of iterations
    return f"Method failed to converge after {N} iterations."


# Define the function f
def f(x):
    return x**3 - 2 * x - 5


p0 = -1
p1 = 0
p2 = 1
TOL = 2e-1
N = 100

result = muller_method(p0, p1, p2, f, TOL, N)

# Print the result
if isinstance(result, str):
    print(result)
else:
    root, iter = result
    print(root)
    print(f"The number of iterations is {iter}.")
