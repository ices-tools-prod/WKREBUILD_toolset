# WKREBUILD2 FLR toolset

FLR-based tools for evaluating recovery plans in ICES.

## Summary


## Distribution

A github template repository based on this example will be available to be applied when creating a new TAF repository.

## Workflow

There are four steps in the analysis of the performance of alternative management procedures (MPs). Each step in contained in a single R script, named according to the TAF standards. Additional sub-steps could be added if necessary or to improve clarity and structure, for example if alternative methods are applied to any of them.

- data loading, OM conditioning and OEM setup (`data.R`).
- MP(s) specification, running and tuning, if required (`model.R`).
- Performance evaluation (`output.R`).
- Generating graphical outputs (`report.R`).

An extra file, `utilities.R`, contains functions defined specifically for this analysis. These should end up on a new ICES-FLR R package.

### data.R - OM conditioning

### model.R

### output.R

### report.R

- tracking

## Example MSE run



## References

- [How to create a github template repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/creating-a-template-repository)
