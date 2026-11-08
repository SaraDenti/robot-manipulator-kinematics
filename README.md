# Robot Manipulator Kinematics

A collection of Wolfram Mathematica notebooks on the kinematics, trajectory tracking, and dynamics of three industrial robot manipulators. Each notebook works through a complete example, from computing the end-effector pose to visualising the robot in motion.

## Notebooks

| Manipulator | Notebook |
| --- | --- |
| Type I spherical robot | [`Robot_Sferico_I_tipo.nb`](Robot_Sferico_I_tipo.nb) |
| Stanford manipulator | [`Manipolatore_di_Standford.nb`](Manipolatore_di_Standford.nb) |
| Anthropomorphic manipulator | [`Robot_Antropomorfo.nb`](Robot_Antropomorfo.nb) |

## What the notebooks cover

- **Forward kinematics** for 3-DOF and 6-DOF configurations.
- **Inverse kinematics** with worked tests and target-reaching examples.
- **3D visualisations and animations** of the manipulators in motion.
- **Trajectory tracking** using Newton's method, a gradient method, and a hybrid method.
- **Dynamics** through kinetic energy calculations and Euler–Lagrange equations.

## How to run

The notebooks were created in **Wolfram Mathematica 14.3**.

1. Download or clone this repository and open one of the `.nb` files in Mathematica.
2. Load the shared Wolfram Language definitions used by the notebook before evaluating its cells. The notebooks call helper functions such as `cinematicaDiretta`.
3. Evaluate the notebook from top to bottom. To explore the trajectory tracking examples, go to the **Newton e gradiente** section after evaluating the preceding cells.

The notebook headings and explanations are in Italian.

## Academic context

Developed for the Industrial Robotics course at the University of Rome Tor Vergata.
