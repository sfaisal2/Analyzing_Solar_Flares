# Name: Victor Bui
# PSID: 2150907
# Activity 3: RHESSI Image Cube Exploration

from astropy.io import fits
import numpy as np
import matplotlib.pyplot as plt
from PIL import Image

# loading FITS files 
flareM = fits.open("rhessi_imagecube_vis_flareM.fits")
flareNon = fits.open("rhessi_imagecube_vis_noflare.fits")

# dimension = (x, y, energy bands, time)
# flareM.info() # dimension: (101, 101, 2, 22)
# flareNon.info() # dimension (101, 101, 3, 13)

dataM = flareM[0].data
dataNon = flareNon[0].data

# print data cubes
print(dataM.shape) # output: (22, 2, 101, 101)
print(dataNon.shape) # output: (13, 3, 101, 101)

# not sure why .shape reversed the dimension to (time, energy, y, x)
# fixed it using np.transpose to unreverse it
dataM = np.transpose(dataM, (3, 2, 1, 0))
dataNon = np.transpose(dataNon, (3, 2, 1, 0))

print(dataM.shape) # now output: (101, 101, 2, 22)
print(dataNon.shape) # now output: (101, 101, 3, 13)

# energy band labels for flareM and flareNon
# flareM should have 2 energy bands: "6-12 keV" and "12-25 keV"
# flareNon should have 3 energy bands: "3-6 keV", "6-12 keV", and "12-25 keV"
bandsM = ["6–12 keV", "12–25 keV"]
bandsN = ["3–6 keV", "6–12 keV", "12–25 keV"]

# (a) visualizing all energy bands for one time frame
print("part (a)")
timeFrame = 0  # first time frame

# m class flare
plt.figure(figsize=(10, 4))
for i in range(len(bandsM)):
    plt.subplot(1, len(bandsM), i + 1)
    plt.imshow(dataM[:, :, i, timeFrame], cmap="hot", origin="lower")
    plt.title(bandsM[i])
    plt.xlabel("X")
    plt.ylabel("Y")
plt.suptitle("M-class Flare: All Energy Bands at One Time Frame")
plt.tight_layout()
plt.show()

# non flare
plt.figure(figsize=(13, 4))
for i in range(len(bandsN)):
    plt.subplot(1, len(bandsN), i + 1)
    plt.imshow(dataNon[:, :, i, timeFrame], cmap="hot", origin="lower")
    plt.title(bandsN[i])
    plt.xlabel("X")
    plt.ylabel("Y")
plt.suptitle("Non-flare: All Energy Bands at One Time Frame")
plt.tight_layout()
plt.show()

# (b) visualizing a selected energy band over time
print("part (b)")
energyBand = dataM.shape[2] - 1  # picking "12-25 keV" as our selected band

# m class flare
for i in range(dataM.shape[3]):
    plt.imshow(dataM[:, :, energyBand, i], cmap="hot", origin="lower")
    plt.title(f"M-class Flare (12–25 keV) Time: {i+1}")
    plt.xlabel("X")
    plt.ylabel("Y")
    plt.pause(0.25)
plt.show()

# non flare
for i in range(dataNon.shape[3]):
    plt.imshow(dataNon[:, :, energyBand, i], cmap="hot", origin="lower")
    plt.title(f"Non-flare Event (12–25 keV) Time: {i+1}")
    plt.xlabel("X")
    plt.ylabel("Y")
    plt.pause(0.25)
plt.show()

# (c) visualize all energy bands averaged over time
print("part (c)")

# m class flare
plt.figure(figsize=(10, 4))
for i in range(len(bandsM)):
    avg_img = np.mean(dataM[:, :, i, :], axis=2)
    plt.subplot(1, len(bandsM), i + 1)
    plt.imshow(avg_img, cmap="hot", origin="lower")
    plt.title(f"{bandsM[i]} (average)")
    plt.xlabel("X")
    plt.ylabel("Y")
plt.suptitle("M-class Flare: Average Intensity Over Time")
plt.tight_layout()
plt.show()

# Non-flare
plt.figure(figsize=(13, 4))
for i in range(len(bandsN)):
    avg_img = np.mean(dataNon[:, :, i, :], axis=2)
    plt.subplot(1, len(bandsN), i + 1)
    plt.imshow(avg_img, cmap="hot", origin="lower")
    plt.title(f"{bandsN[i]} (average)")
    plt.xlabel("X")
    plt.ylabel("Y")
plt.suptitle("Non-flare: Average Intensity over Time")
plt.tight_layout()
plt.show()

# (d) creating an animation over time and saving it as GIF
print("part (d)")
band = dataM.shape[2] - 1

# m class flare
framesM = []
for t in range(dataM.shape[3]):
    plt.imshow(dataM[:, :, band, t], cmap="hot", origin="lower")
    plt.title("M-class Flare (12–25 keV) - Frame " + str(t+1))
    plt.xlabel("X")
    plt.ylabel("Y")
    filename = "mflare" + str(t) + ".png"
    plt.savefig(filename)
    framesM.append(Image.open(filename))
    plt.close()
framesM[0].save("flareM.gif", save_all=True, append_images=framesM[1:], duration=300, loop=0)
print("Saved flareM.gif")

# non flare 
framesN = []
for t in range(dataNon.shape[3]):
    plt.imshow(dataNon[:, :, band, t], cmap="hot", origin="lower")
    plt.title("Non-flare (12–25 keV) - Frame " + str(t+1))
    plt.xlabel("X")
    plt.ylabel("Y")
    filename = "nonflare" + str(t) + ".png"
    plt.savefig(filename)
    framesN.append(Image.open(filename))
    plt.close()
framesN[0].save("flareNon.gif", save_all=True, append_images=framesN[1:], duration=300, loop=0)
print("Saved flareNon.gif")

# (e) pinpoint flare location over the Sun
print("part (e)")

# m class flare
avgM = np.mean(dataM, axis=(2, 3))  # average across energy and time
yM, xM = np.unravel_index(np.argmax(avgM), avgM.shape)
plt.imshow(avgM, cmap="hot", origin="lower")
plt.scatter(xM, yM, color="cyan", marker="x", s=80)
plt.title(f"M-class Flare Brightest Point (x={xM}, y={yM})")
plt.xlabel("X")
plt.ylabel("Y")
plt.show()

# non flare
avgN = np.mean(dataNon, axis=(2, 3))
yN, xN = np.unravel_index(np.argmax(avgN), avgN.shape)
plt.imshow(avgN, cmap="hot", origin="lower")
plt.scatter(xN, yN, color="cyan", marker="x", s=80)
plt.title(f"Non-flare Brightest Point (x={xN}, y={yN})")
plt.xlabel("X")
plt.ylabel("Y")
plt.show()
