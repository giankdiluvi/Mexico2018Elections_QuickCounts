# Mergers

Code used to merge different estimations into a final estimation.

In Chiapas, two estimations are merged by joiing the intervals and taking the mean of the point estimate.
The Bayesian team merger joins the two shortest intervals of the three members of the team, and takes the mean of the corresponding point estimates.
The Federal merger uses the so-called "big median" criterion, taking the mean of the extremes of the two longest intervals, and the mean of their point estimates.
