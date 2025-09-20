# Article: Regime Characteristics and Prediction Performance

This repository for the thesis of my 2025 Master's in Economics at FGV-EESP, "Regime Characteristics and Prediction Performance" (title to be decided). The general research question is:

> What is the relationship between regimes' characteristics and forecasting performance, across different regime-switching models and DGPs?

The work is based on a theoretical framework for systematic comparisons of several DGP 'ingredients', models, and regime-conditional metrics. With this framework, Monte Carlo simulations are run, and regression explaining the models performance against the metrics, in different ways and contexts, are used to answer specific predefined questions.


## Repository structure

For the text:

- [docs/](docs/): contains documentation for the project, with `main/` holding the up-to-date LaTeX source for the thesis, and `others/` including earlier sketches, presentations, and proposal documents that may not reflect the latest changes.
- [figures/](figures/): stores figures generated for the thesis, including plots and diagrams.

For the code, a modular structure is used via the `box` package. 

- [main.R](main.R): the main entry point, orchestrating the workflow by integrating all modules from `src/`.
- [src/](src/): holds modular R code organized by functionality.
    - [src/utils/](src/utils/): general utility functions and packages loaded in all files.
    - Creators and options:
        - [src/creators/](src/creators/): function factories of SGPs and RGPs (serie and regime generating processes, respectively), models, and metrics.
        - [src/options/](src/options/): list of actual DGPs, models, and metrics options considered in the simulations, created by the factories in `creators/`. Each is a function.
        - The functions above follow similar definitions as in the text, although some details may differ for implementation performance.
    - [src/diagnostics/](src/diagnostics/): functions for diagnostics of the simulations, such as convergence checks and result summaries.
    - [src/tests/](src/tests/): benchmarks that I explored to improve the performance of the code.

Other files include:

[article-regime-id-performance.code-workspace](article-regime-id-performance.code-workspace), [article-regime-id-performance.Rproj](article-regime-id-performance.Rproj): project configuration files for VS Code and RStudio, respectively, not needed for reproducibility.

