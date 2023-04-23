def secant_method(p0, p1, TOL, N, f):
    """
    Implements the secant method to find the root of a function f.

    Args:
    p0 (float): first initial approximation
    p1 (float): second initial approximation
    TOL (float): tolerance
    N (int): maximum number of iterations
    f (function): the function whose root is to be found

    Returns:
    The approximate root p or a message of failure.
    """
    # Initialize variables
    i = 2
    q0 = f(p0)
    q1 = f(p1)

    # Iterate until either the tolerance or maximum number of iterations is reached
    while i <= N:
        # Calculate the next approximation
        p = p1 - q1 * (p1 - p0) / (q1 - q0)

        # Check if the absolute difference between the current approximation and the previous approximation is less than the tolerance
        if abs(p - p1) < TOL:
            return (f"The approximate root is {p}.", i)

        # Update variables for next iteration
        i += 1
        p0 = p1
        q0 = q1
        p1 = p
        q1 = f(p)

    # If the loop terminates without returning, return a message of failure
    return f"Method failed to converge after {N} iterations."


# Define the function f
def f(x):
    return x**3 - 2 * x - 5


result = secant_method(-1, 0, 1e-2, 100, f)

# Print the result
if isinstance(result, str):
    print(result)
else:
    root, iter = result
    print(root)
    print(f"The number of iterations is {iter}.")
