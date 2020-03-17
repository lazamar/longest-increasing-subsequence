# Create a graph from a CSV with time data
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import csv

with open('stats.csv') as csv_file:
    stats = csv.DictReader(csv_file, delimiter=';')

    commits = []
    alloc = []
    memory = []
    time = []
    for c in stats:
        commits.append(c["commit"])
        alloc.append(int(c["allocations"]))
        memory.append(int(c["memory"]))
        time.append(float(c["time"]))


    # Data for plotting
    fig, ax = plt.subplots()
    fig.autofmt_xdate(rotation=90)

    color = 'tab:green'
    ax.set_yscale("log", nonposy='clip')
    ax.set_ylabel("Allocations (bytes)", color=color)
    ax.step(commits, alloc, color=color)

    color = 'tab:blue'
    ax2 = ax.twinx()  # instantiate a second axes that shares the same x-axis
    ax2.set_ylabel('Memory (MB)', color=color)  # we already handled the x-label with ax1
    # ax2.set_yscale("log", nonposy='clip')
    ax2.step(commits, memory, color=color)
    ax2.tick_params(axis='y', labelcolor=color)
    ax2.spines["right"].set_position(("axes", 1.2))

    color = 'tab:red'
    ax3 = ax.twinx()  # instantiate a second axes that shares the same x-axis
    ax3.set_ylabel('Time (seconds)', color=color)  # we already handled the x-label with ax1
    ax3.set_yscale("log", nonposy='clip')
    ax3.step(commits, time, color=color)
    ax3.tick_params(axis='y', labelcolor=color)

    fig.tight_layout()  # otherwise the right y-label is slightly clipped
    fig.savefig("plot.svg",
                format="svg",
                transparent=True
                )

    # plt.show()
