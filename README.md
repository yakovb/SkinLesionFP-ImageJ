# Skin Lesion FP
MSc project: *Detecting changes in skin lesions: A functional programming approach*
Supervised by Prof Steve Maybank

Department of Computer Science and Information Systems, Birkbeck College, University of London, 2015

## Abstract
Doctors advise people to monitor their skin lesions for signs of change as this could signal the onset of
melanoma skin cancer. However, monitoring via visual self examination or the visual comparison of
photographs is slow and error-prone. This project provides a solution to this problem by automating the
process of skin lesion analysis. It does this by implementing a set of algorithms to measure key lesion
descriptors: asymmetry, border irregularity, and colour variegation. The data created by these measurements
is statistically evaluated to determine whether any change has occurred in the lesion. The software to process
the images is developed using a functional programming approach; that is, favouring immutability of data,
referentially transparent expressions, and the composition of functions. Through this approach a library of
image processing primitives is built up around the key abstractions of image transformation, traversal, and
pixel manipulation. This library is shown to be capable of providing an impressive level of compile-time
type safety. This feature, along with the composability of image processing primitives, is the project's first
major contribution and innovation; the second is in demonstrating the utility of tracking skin lesions via
computational analysis of images.

## Running plugins from ImageJ
The `build.sbt` file contains all dependencies. I wrote the code in IntelliJ so this advise is specific to that editor (hopefully it won't be too different for others). Clone the repository, build the project (which will pull in all the dependencies), and set up the ImageJ sbt plugin as per the instructions [on this fantastic site](https://codingonthestaircase.wordpress.com/2014/11/23/developing-imagej-plugins-with-sbt-using-sbt-imagej/) by Jarek Sacha. This will let you run ImageJ from within IntelliJ and all my plugins will show in the Plugin menu.

## Running a bulk analysis
The `core` package contains a `RunBulkAnalysis` object with a `main` method. Runnig this will prompt you for a directory holding image files. All images will be processed and the results of the analysis written as `output.csv` in the same directory as the images. 

## Running the tests
If the cloned repo directory is the current working directory then the tests should run without issue. However, if there is a problem you will need to manually update the directory of the test images. Go to the `Matrices` class in the test folder and edit line 6 - the `root` variable. 
