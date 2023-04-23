def false_position_method(p0, p1, TOL, N, f):
    """
    This function implements the false position method to approximate the root of a function f.

    Inputs:
    - p0, p1: initial approximations
    - TOL: tolerance for the approximation
    - N: maximum number of iterations
    - f: function to be approximated

    Outputs:
    - p: approximate solution
    - or message of failure if the method fails to converge
    """

    # Initialize variables
    i = 2
    q0 = f(p0)
    q1 = f(p1)

    # Loop until the tolerance is met or the maximum number of iterations is reached
    while i <= N:
        # Compute the next approximation using the false position formula
        p = p1 - q1 * (p1 - p0) / (q1 - q0)

        # Check if the approximation satisfies the tolerance
        if abs(p - p1) < TOL:
            return (f"The approximate root is {p}.", i)

        # Compute the function value at the new approximation
        q = f(p)

        # Update the approximations and function values
        if q * q1 < 0:
            p0 = p1
            q0 = q1
        p1 = p
        q1 = q

        i += 1

    # If the maximum number of iterations is reached, the method failed to converge
    return f"Method failed to converge after {N} iterations."


# Define the function f
def f(x):
    return x**3 - 2 * x - 5


result = false_position_method(0, 3, 1e-5, 100, f)

# Print the result
if isinstance(result, str):
    print(result)
else:
    root, iter = result
    print(root)
    print(f"The number of iterations is {iter}.")
