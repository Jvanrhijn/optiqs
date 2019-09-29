import sys
import math

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm


# hacky solution to read space separated points from file
def read_points(plane, wigner):
    xs, ys, ws = [], [], []
    with open(plane, "r") as file:
        for line in file:
            line = line.split()
            xs.append(float(line[0]))
            ys.append(float(line[1]))
    with open(wigner) as file:
        for line in file:
            line = line.split()
            ws.append(float(line[0]) + 1j * float(line[1]))
    return np.array(xs), np.array(ys), np.array(ws)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise Exception(
        """Please provide the path of the complex plane points and
        Wigner function values""")
    plane, wigner = sys.argv[1], sys.argv[2]
    xs, ys, ws = read_points(plane, wigner)

    # reshape data into 2d arrays
    n = int(math.sqrt(ws.size))

    ws = ws.reshape((n, n))
    xs = xs.reshape((n, n))
    ys = ys.reshape((n, n))

    #ws = np.fft.fftshift(ws)

    fig = plt.figure(figsize=(20, 10))

    ax1 = fig.add_subplot(1, 2, 1, projection="3d")

    ax1.plot_surface(xs, ys, np.abs(ws), cmap=cm.coolwarm, cstride=1, rstride=1)

    ax1.set_xlabel(r"Re $\beta$")
    ax1.set_ylabel(r"Im $\beta$")

    ax2 = fig.add_subplot(1, 2, 2)

    ax2.set_xlabel(r"Re $\beta$")
    ax2.set_ylabel(r"Im $\beta$")

    ax2.axis("equal")

    c = ax2.contourf(xs, ys, np.abs(ws))
    fig.colorbar(c)

    plt.axis('equal')
    plt.show()



