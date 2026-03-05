import numpy as np
import matplotlib.pyplot as plt

f = open("output.txt")
length, time = f.readline().split(",")

length = float(length)
time = float(length)

data = np.loadtxt("output.txt", unpack=True, skiprows = 1)

x = (np.array(range(data.shape[0]))+1)/length
y = (np.array(range(data.shape[1]))+1)/time

print(data.shape)
contour = plt.contourf(x, y, data.T, cmap = 'hot')
plt.colorbar(contour, label='Heat')

plt.xlabel("Position (m)")
plt.ylabel("Time (s)")
plt.show()