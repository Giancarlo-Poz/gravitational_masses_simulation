# Gravitational_masses_simulation
This project creates a realistic physics simulation of gravitationally interacting masses using Lagrangian mechanics. 
The code can be modified to create slow motions or high resolution videos.
The simulation is numerically intense because each mass interacts with all the other masses.

## How to run a simulation
Run, in order, the tree .wl files. Then, join the videos.

### Files
In the first file it possible to modify the physical scenario, for example parameters, masses, initial conditions (position and velocities) and other variables.
The Euler-Lagrange equations are automatically calculated and solved. The solutions are saved into a "solutions.mx" file.

In the second file it possible to modify the video options, for example aspect ratio (16:9), image quality (4K), text font, background and others.
Frames are generated and saved into a "data.mx" file.

In the third file it possible to modify outupt options, for example frame rate and the number of avi files in which the simulation is splitted. (Long simulations are splitted into many avi files to avoid memory overloads and crashes).
avi videos are generated.

### Join the videos
To join multiple .avi files, create a "mylist.txt" with file names, for example:
```file 'simulation001.avi'
file 'simulation002.avi'
file 'simulation003.avi'
file 'simulation004.avi'
...
...
...
```
(the "simulationNNN.avi" files are generated by the third .wl file)

Then, in the linux terminal
`ffmpeg -f concat -i mylist.txt -c copy output.avi`

To add audio to the avi file 
`ffmpeg -i output.avi -i ../audio/FlightofTheBumblebee.wav \
-shortest -c:v copy -c:a aac video.avi`

An example of such video is
https://www.youtube.com/watch?v=EABwQ0Jdv1U
