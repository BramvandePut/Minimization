# Minimization
A Fortran90 program for the numerical minimization of n-dimensional mathematical functions.

Created by: Bram van de Put, 2019
For the course: Scientific Computing and Programming

Minimization is a program written in Fortran90 and was compiled and tested with the 
gfortran compiler. Gnuplot is required for the plot feature of this program though it
should not be required to compile and run the rest of the program.



COMPILE INSTRUCTIONS
Use the following command to compile and run the program:
gfortran -o Minimization Minimizer.f90 ConfigHandler.f90 ConjugateGradient.f90 GradientCalculator.f90 MatFunction.f90 PlotModule.f90 RastriginFunction.f90 SteepestDescent.f90 Testing.f90 ; ./Minimization


PROGRAM MODULES

Minimization consists of the following modules, (ordered by dependencies):
ConfigHandler
MatFunction
RastriginFunction
GradientCalculator
SteepestDescent
ConjugateGradient
PlotModule
Testing

And the main program:
Minimization



MODULE DESCRIPTIONS

ConfigHandler:
The ConfigHandler module sets all the initial variables and constants needed to run the
minimizations.
Upon execution of the ConfigHandler 'Initialize' subroutine all variables are initialized to
the contents of 'config.txt'.
After initialization the user is prompted with an option to adjust these settings by choosing
either 'y' or 'n'.
If the user chooses 'n' the minimization is performed using the settings as loaded from 
config.txt, if the user chooses 'y' the user is prompted with a selection of the different
variables which can than be set individually.
After editing the required variables the configuration is saved to config.txt and control
returns to the main program.

The adjustable variables are: 
'mode' 'algorithm' 'direction' 'plot' 'list' 'order' 'x1' 'h' 'convergelimit' 'iterationmax'

mode - selects the function to be minimized 'default' for the function 'X^2 + cos(y)'
which may be altered from within the source code 'rastrigin' for th rastrigin energy function.

algorithm - selects the minimization algorithm to be performed 'steep' for the steepest descent
algorithm or 'conjugate' for the conjugate gradient algorithm.

direction - sets the direction of the optimization 'min' for minimization 'max' for maximization.

plot - toggles the execution of the Plot routine 'on' or 'off'.

list - toggles listing the coordinates for each iteration 'on' or 'off'.

order - takes an integer value for the order of the function to be optimized.

x1 - takes a number of real values for the first estimate of the minimum location.

h - takes a real value used in the finite difference approximation of the gradient and hessian
matrix. The approximation increases in accuracy with decreasing h.

convergelimit - takes a real number for the precision of the minimzation. 

iterationmax - takes an integer for the maximum number of iterations before the program quits.



MatFunction:
The MatFunction module contains the mathematical function to be minimized
as of release: x^2 + cos(y)
This function may be altered from the source code as needed.
It should be noted that when the order of the function is altered, this should be adjusted 
accordingly in the configuration.



RastriginFunction:
The RastriginFunction module contains the formula for the nth order Rastrigin surface.
The rastrigin surface scales with the order and is therefore not dependent on manual adjustment
of the configuration.



GradientCalculator:
The GradientCalculator module contains the routines 'CalculateGradient' and 'CalculateHessian' 
which are used to calculate the gradient vector and the hessian matrix respectively.
both are based on the central difference approximation.



SteepestDescent:
The SteepestDescent module contains the routine 'Steep' which performs the steepest descent
algorithm for minimizing the chosen function.
The SteepestDescent algorithm takes iterative steps towards the negative gradient (direction
of largest decrease). This vector is multiplied by a scalar which is calculated such that the
minimum is not overshot, but rather the algorithm converges towards the minimum.
The algorithm exits either when the number of iterations exceeds the chosen 'iterationmax' or 
the difference between the latest and previous input is lower than the chosen 'convergelimit'.



ConjugateGradient:
The ConjugateGradient module contains the routine 'Conjugate' which performs the conjugate
gradient algorithm for minimizing the chosen function.
The ConjugateGradient algorithm takes iterative steps based on the direction of largest 
decrease, Compared to the steepest descent algorithm though, each subsequent direction is 
chosen to be orthogonal to each previous step.
The Cojugate Gradient method converges faster than the steepest descent method, though it is
limited in it's first estimate of the minimum which must be 0.
The algorithm exits either when the number of iterations exceeds the chosen 'iterationmax' or 
the difference between the latest and previous input is lower than the chosen 'convergelimit'.



PlotModule:
The PlotModule module contains the routine 'plot' which creates a surface plot and overlaps it
with the minimization steps as vectors. The plot function is only enabled for functions of 2nd
order. Use of the plot function requires gnuplot to be installed.


Testing:
The Testing module performs unit tests on the routines: CalculateGradient and CalculateHessian.
The tests are performed by initializing the settings and checking the output of the program
