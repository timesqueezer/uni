import numpy as np
import matplotlib
import PyQt5

matplotlib.use('Qt5Agg')

import matplotlib.pyplot as plt

data1 = [
    608.048553, # 1c
    310.301359, # 2c
    211.117236, # 3c
    163.006624, # 4c
    136.714105, # 5c
    121.693339, # 6c
    111.431568, # 7c
    104.769027, # 8c
    97.564436, # 9c
    87.000344, # 10c
    78.343819, # 11c
    73.029624, # 12c
]

data2 = [
    607.980708, # 1c
    311.732062, # 2c
    211.422651, # 3c
    162.886294, # 4c
    136.971907, # 5c
    121.634993, # 6c
    112.341817, # 7c
    104.207370, # 8c
    97.176669, # 9c
    88.464298, # 10c
    77.012606, # 11c
    73.952961, # 12c
]

data3 = [
    608.349436, # 1c
    311.441337, # 2c
    213.881750, # 3c
    163.799995, # 4c
    136.885376, # 5c
    122.122480, # 6c
    111.726232, # 7c
    103.658585, # 8c
    97.479076, # 9c
    88.769330, # 10c
    77.902245, # 11c
    72.652225, # 12c
]

# calculate averages
data = [sum(i) / 3 for i in zip(data1, data2, data3)]

fig, ax = plt.subplots()

index = np.arange(len(data))
bar_width = 0.35

rects1 = ax.bar(index, data, bar_width, color='b', label='cores')
ax.set_xlabel('cores (#)')
ax.set_xticks(index)
ax.set_xticklabels([str(i + 1) for i in index])
ax.set_ylabel('runtime (s)')
ax.set_title('runtime per core-count')

# ax.set_yscale('log')

fig.tight_layout()
plt.show()
