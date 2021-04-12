# TITLE

Data &amp; Analysis Compendium for the paper, entitled _title_. 
This study has received [approval](docs/EthicalApproval.pdf) for the Vrije Universteit Amsterdam's Research Ethics Committee and is registered at [osf](https://osf.io/kvtzp/) -- see [here](docs/pap/doc.pdf) for the pre-analysis plan.

## Draft
View the [draft of the paper here](report/draft.pdf).

## Code
The main code to prepare the data is located in the [src/data-processing](src/data-processing/). 
Of interest might be:

* [Prepare Data](src/data-processing/prep_data.md) This file combines ... and creates the variables for the [analysis](src/analysis/analysis.md).

## Data

The following data files might be of interest:

* [NAME](LOCATION) 

## Results

The main code to conduct the multiverse analyses is combined in the [src/analysis/analysis.md](src/analysis/analysis.md). 
Of interest might be:
* [Code](src/analysis/) to conduct the seperate multiverse analyses
* [Figures](report/figures) visualizing the descriptives and the results of the analyses 

## Replication 

To run all scripts, call [`doit`](https://github.com/ccs-amsterdam/ccs-compendium) in your command line:

```
sudo pip3 install doit
doit
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its term.