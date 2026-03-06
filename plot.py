import numpy as np
import matplotlib.pyplot as plt

f = open("output.txt")
length, time = f.readline().split(",")

length = float(length)
time = float(time)

data = np.loadtxt("output.txt", unpack=True, skiprows = 1)

x = (np.array(range(data.shape[0]-1))+1)*length/data.shape[0]
y = (np.array(range(data.shape[1]))+1)*time/data.shape[1]

sums = data[data.shape[0]-1, :]
data = data[:data.shape[0]-1, :data.shape[1]]


plt.subplot(121)
contour = plt.contourf(x, y, data.T, cmap = 'hot')
plt.colorbar(contour, label='Heat')
plt.xlabel("Position (m)")
plt.ylabel("Time (s)")

plt.subplot(122)
plt.plot(y, sums)
plt.ylim(min(sums)-0.5, max(sums)+0.5)
plt.xlabel("Time (s)")
plt.ylabel("Total Heat")


plt.tight_layout()
plt.show() 