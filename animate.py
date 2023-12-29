import numpy as np
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation, PillowWriter
import os

itemlist = sorted(item for item in os.listdir('./dump')
                        if not item.startswith('.'))

fig, ax = plt.subplots(figsize = (8,8))

def animateSnapshot(i):
    ax.clear()
    image = ax.imshow(np.genfromtxt(f'./dump/{i:>8d}', delimiter = ','),
                      cmap = 'binary')
    title = ax.set_title(f"{i:08d}")

    return image, title

animation = FuncAnimation(fig, animateSnapshot,
                          repeat = True,
                          ##### EDIT HERE #####
                          frames = range(1, 2000001, 10000))
                          ##### EDIT HERE #####
animation.save("./result.gif", dpi = 100, writer = PillowWriter(fps = 30))

print(" Created an Animation ! >:D ")
