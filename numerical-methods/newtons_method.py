def newtons_method(p0, TOL, N, f, df):
    """
    Implements Newton's method to find the root of a function f.

    Args:
    p0 (float): initial approximation
    TOL (float): tolerance
    N (int): maximum number of iterations
    f (function): the function whose root is to be found
    df (function): the derivative of the function f

    Returns:
    The approximate root p or a message of failure.
    """
    # Initialize variables
    i = 1

    # Iterate until either the tolerance or maximum number of iterations is reached
    while i <= N:
        # Calculate the next approximation
        p = p0 - f(p0) / df(p0)

        # Check if the absolute difference between the current approximation and the previous approximation is less than the tolerance
        if abs(p - p0) < TOL:
            return (f"The approximate root is {p}.", i)

        # Update variables for next iteration
        i += 1
        p0 = p

    # If the loop terminates without returning, return a message of failure
    return f"Method failed to converge after {N} iterations."


# Define functions f and df
def f(x):
    return x**3 - 2 * x - 5


def df(x):
    return 3 * x**2 - 2


result = newtons_method(2, 1e-3, 100, f, df)

# Print the result
if isinstance(result, str):
    print(result)
else:
    root, iter = result
    print(root)
    print(f"The number of iterations is {iter}.")
