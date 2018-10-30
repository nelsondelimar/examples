# Import all usefull libraries
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
from mpl_toolkits.basemap import Basemap

# Define:
# 1 - Number of points
# 2 - Values for longitude (degrees)
# 3 - Values for latitute (degrees)
x = np.arange(-180,181,1)
y = np.arange(-90,91,1)
# Create the meshgrid
X, Y = np.meshgrid(x, y)

# Some constants
gamma0 = 978032.7
beta1 = 0.0053024
beta2 = -0.0000058
# Compute the constants for the sin values used in the calculation
sin = np.sin(np.deg2rad(Y))
sin2 = np.sin(np.deg2rad(2*Y))
# Compute the international gravity formula
gamma1980 = gamma0 * (1 + beta1 * (sin**2) + beta2 * (sin2**2))

# Plot all values for theoretical gravity
plt.figure(figsize = (12, 10))
plt.title('Theoretical gravity - 1980 formula', fontsize = 24)
plt.contourf(X, Y, gamma1980, 100, cmap = plt.cm.jet)
plt.xlabel('Longitude', fontsize = 16)
plt.ylabel('Latitute', fontsize = 16)
plt.xticks(fontsize = 14)
plt.yticks(fontsize = 14)
plt.colorbar()
#plt.savefig('gravity1980_contour.png')
plt.show()

# Ploting the values on the theoretical Earth
plt.figure(figsize=(20,12))
m = Basemap(projection='robin', lon_0=0., resolution='c')
m.drawcoastlines()
m.drawparallels(np.arange(-90.,90.,30.))
m.drawmeridians(np.arange(-180.,180.,60.))
m.contourf(X, Y, gamma1980, cmap='RdBu_r')
#CS1 = m.contour(X, Y, gamma1980, 10, linewidths=0.5, colors='k', latlon=True)
CS2 = m.contourf(X, Y, gamma1980, 100, cmap=plt.cm.jet, extend='both', latlon=True)
cb = m.colorbar()
cb.set_label('mGal', fontsize = 15)
plt.title('Theoretical gravity - 1980 formula', fontsize = 25)
plt.savefig('grav1980_globe.png', bbox_inches='tight',dpi=500)
plt.show()




