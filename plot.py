import numpy as np
import matplotlib.pyplot as plt

    # Read the simulation conditions from first line
f = open("output.txt")
length, time = f.readline().split(",")
length = float(length)
time = float(time)

    # Extract the plot data
data = np.loadtxt("output.txt", unpack=True, skiprows = 1)
x = (np.array(range(data.shape[0]-1))+1)*length/data.shape[0]
y = (np.array(range(data.shape[1]))+1)*time/data.shape[1]

    # Extract data on heat sums
sums = data[data.shape[0]-1, :]
data = data[:data.shape[0]-1, :data.shape[1]]
sums = sums / data.shape[0]

    # Plot heat at position by time
plt.subplot(121)
contour = plt.contourf(x, y, data.T, cmap = 'hot')
plt.colorbar(contour, label='Heat')
plt.xlabel("Position (m)")
plt.ylabel("Time (s)")

    # Plot progression of average heat
plt.subplot(122)
plt.plot(y, sums)
plt.ylim(min(sums)-0.2, max(sums)+0.2)
plt.xlabel("Time (s)")
plt.ylabel("Average Heat per Index")


plt.tight_layout()
plt.show() 