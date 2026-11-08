# Robot Manipulator Kinematics

Wolfram Mathematica notebooks on the kinematics, trajectory tracking, and dynamics of three industrial robot manipulators. Each notebook combines symbolic calculations, worked examples, and interactive 3D visualisations.

## Notebooks

| Manipulator | Notebook |
| --- | --- |
| Type I spherical robot | [Robot_Sferico_I_tipo.nb](robot/Robot_Sferico_I_tipo.nb) |
| Stanford manipulator | [Manipolatore_di_Standford.nb](robot/Manipolatore_di_Standford.nb) |
| Anthropomorphic manipulator | [Robot_Antropomorfo.nb](robot/Robot_Antropomorfo.nb) |

## What the notebooks cover

- **Forward kinematics** for 3-DOF and 6-DOF configurations.
- **Inverse kinematics** with tests and examples of reaching a target position.
- **3D visualisations and animations** of the manipulators in motion.
- **Trajectory tracking** using Newton's method, a gradient method, and a hybrid method.
- **Manipulator dynamics** through kinetic energy calculations and the Euler–Lagrange equations.

## How to run

The notebooks were created in **Wolfram Mathematica 14.3**.

1. Download or clone the repository and open a notebook from the `robot` folder in Mathematica.
2. Load the shared Wolfram Language definitions, including helper functions such as `cinematicaDiretta`.
3. Evaluate the notebook from top to bottom. The **Newton e gradiente** section contains the trajectory-tracking examples.

The notebook headings and explanations are in Italian.

## Academic context

Developed for the Industrial Robotics course at the University of Rome Tor Vergata.
