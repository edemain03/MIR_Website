Next set of plans:
Look over code, work out any reactive issues, I am sure theres a few here and there

New panel ->
Implement a page (navPanel) that chooses the best model for the user. To begin, it can just go based on error metrics.
User inputs the response variable and what kind of data they have (if their samples were taken from a sample depth of 30cm, they use that, and input the 
variable they want to predict, i.e., total carbon or aggregate stability) and chooses a model for them.

From there, add in secondary feature that weighs in the pca graph. i.e., if their sample points fall within training sample points in pca space, weigh that
into the automatic model selection (to begin start with like 50/50 weight and go from there)

add maps, about tab, averaging 

For CNN models - Confidence Interval

in order: averaging, cnn model ci, build your own models,