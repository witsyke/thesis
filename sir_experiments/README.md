# SIR Simulation instructions

Take not that the mobility data is not publicly available.

1. Open the sir_evaluation.R file 
2. Set the working directory to the directory of the sir_evaluation.R file
3. Set the scenario name:
```r
experiment <- "<<SCENARIO NAME>>"
```
Options are:
- "observed"
- "no_mobility"
- "only_partial"
- "partial_from_beginning"
- "extended_at_partial"
- "extended_no_partial"
- "extended_from_beginning"

4. Set parametrization of the simulation has to be set manually and by default the simulation is run with the optimal parameters. To chose limits of the calibration confidence interval adjust line 253 in the sir_evaluation.R file.
```r
run_experiment(list(beta = <<beta parameter>>, gamma = <<gamma parameter>>),
...
```
Parameters can be either beta or gamma followed by _fit, _min, _max for the optimal parameter of the lower or upper bound of the 99% confidence interval, respectively.

5. Run the rest of the script