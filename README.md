# WKREBUILD2 FLR toolset

FLR-based tools for evaluating recovery plans in ICES.


## Distribution

A github template repository based on this example will be available to be applied when creating a new TAF repository.

This example repository contains a `SOFTWARE.bib` file that install the latest versions of needed FLR packages: [https://flr-project.org/FLCore](FLCore), [https://flr-project.org/FLFishery](FLFishery), [https://flr-project.org/FLasher](FLasher), [https://flr-project.org/mse](mse), and [https://flr-project.org/mseviz](mseviz). Please run

```
library(icesTAF)
install.deps
taf.boostrap()
```

to get the updated package version installed locally. The capability to compile R packages containing C++ code for source is required, for example using [Rtools](https://cran.r-project.org/bin/windows/Rtools/) in the Windows OS.

## Workflow

There are four steps in the analysis of the performance of alternative management procedures (MPs). Each step in contained in a single R script, named according to the TAF standards. Additional sub-steps could be added if necessary or to improve clarity and structure, for example if alternative methods are applied to any of them.

- data loading, OM conditioning and OEM setup (`data.R`).
- MP(s) specification, running and tuning, if required (`model.R`).
- Performance evaluation (`output.R`).
- Generating graphical outputs (`report.R`).

An extra file, `utilities.R`, contains functions defined specifically for this analysis. These should end up on a new ICES-FLR R package.

### data.R - conditions OM and builds OEM for sol.27.4

### model.R - runs MPS for recovery advice rule and an F=0 projection

### output.R - computes performance

### report.R - creates plots and tables

### utilities.R - functions and methods written for this analysis

- icesControl

- tracking

## Example MSE run

- OM

The analysis shown here uses an operating model based on the 2022 WGNSSK stock assessment of sol.27.4. The output of the AAP model with McMC turned on, so as to include parameter estimation uncertainty and provide some varibility in initial conditions for the simulations. The stock was then subjected to yearly fishing mortalities of F=0.35 over the 2010-2022 period, to bring it to an overexploited state.

- deviances

- arule

- rebuild rules

- plot TS

- COMPUTE recovery year

- performance statistics

- plotBPs

- plotTOs

## Next steps

## References

- [How to create a github template repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository)
