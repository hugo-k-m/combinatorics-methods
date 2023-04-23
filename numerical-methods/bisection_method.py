def bisection_method(a, b, TOL, N, f):
    """
    Implements the bisection method to find the root of a function f.

    Args:
    a (float): left endpoint of interval
    b (float): right endpoint of interval
    TOL (float): tolerance
    N (int): maximum number of iterations
    f (function): the function whose root is to be found

    Returns:
    The approximate root p or a message of failure.
    """
    # Initialize variables
    i = 1

    # Check if the endpoints have opposite signs
    if f(a) * f(b) >= 0:
        return "Error: f(a) and f(b) must have opposite signs."

    fa = f(a)

    # Iterate until either the tolerance or maximum number of iterations is reached
    while i <= N:
        # Calculate the midpoint of the interval
        p = a + (b - a) / 2
        fp = f(p)

        # Check if the absolute difference between a and b is less than the tolerance
        if fp == 0 or (b - a) / 2 < TOL:
            return (f"The approximate root is {p}.", i)

        # Check which half of the interval to keep
        if fa * fp > 0:
            a = p
            fa = fp
        else:
            b = p

        # Update variables for next iteration
        i += 1

    # If the loop terminates without returning, return a message of failure
    return f"Method failed to converge after {N} iterations."


# Define your function f
def f(x):
    return x**3 - 2 * x - 5


result = bisection_method(2, 3, 1e-4, 100, f)

# Print the result
if isinstance(result, str):
    print(result)
else:
    root, iter = result
    print(root)
    print(f"The number of iterations is {iter}.")
