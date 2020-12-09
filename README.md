# Highspeed Analysis

## Overview

This repository contains all code for statistical analyses used in Wittkuhn & Schuck, 2020, *Nature Communications*.

Please visit https://wittkuhn.mpib.berlin/highspeed/ for the project website and https://gin.g-node.org/lnnrtwttkhn/highspeed-analysis to get the actual data.

## Dataset structure

- `/code` contains all project-specific code, mainly `.Rmd` notebooks. The rendered versions of the notebooks can be found on the project website at https://wittkuhn.mpib.berlin/highspeed/
- `/data` contains relevant input datasets: the behavioral `events.tsv` files in the BIDS dataset (`highspeed-bids`) and the decoding results of (`highspeed-decoding`)
- `/figures` and `/sourcedata` are empty subdirectories that are populated with the Figures and Source Data produced by the `.Rmd` scripts in `/code`

## Citation

> Wittkuhn, L. and Schuck, N. W. (2020). Dynamics of fMRI patterns reflect sub-second activation sequences and reveal replay in human visual cortex. *Nature Communications*.

A preprint (old version) is available at:

> Wittkuhn, L. and Schuck, N. W. (2020). Faster than thought: Detecting sub-second activation sequences with sequential fMRI pattern analysis. *bioRxiv*. [doi:10.1101/2020.02.15.950667](http://dx.doi.org/10.1101/2020.02.15.950667)

## Contact

Please [create a new issue](https://github.com/lnnrtwttkhn/highspeed-analysis/issues/new) if you have questions about the code or data, if there is anything missing, not working or broken.

For all other general questions, you may also write an email to:

- [Lennart Wittkuhn](mailto:wittkuhn@mpib-berlin.mpg.de)
- [Nicolas W. Schuck](mailto:schuck@mpib-berlin.mpg.de)

## License

All of the data are licensed under Creative Commons Attribution-ShareAlike 4.0.
Please see the [LICENSE](LICENSE) file and https://creativecommons.org/licenses/by-sa/4.0/ for details.
