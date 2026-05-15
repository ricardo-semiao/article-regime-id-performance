# Article: Regimes' Characteristics and Time Series Forecasting

This repository contains the thesis for my 2025 Master's in Economics at FGV-EESP. The general research question is:

> How do different regime-switching models perform across different DGPs, and how do the characteristics of the regimes' distributions relate to that performance?

In this README, I explain the organization of the repository and how one can reproduce and expand the results.

The final work sent to the thesis defense is in the [archive/thesis-defense](https://github.com/ricardo-semiao/article-regime-id-performance/tree/archive/version-defense) branch. The main branch includes reorganization, improvements, and some new analyses. The final text is [sketch.pdf](docs/others/sketch.pdf) (weird name for the final text, but at least better than _thesis\_final (1) (copy) nowforreal v2.pdf_).

To recreate the results:

1. Install [R](https://cran.rstudio.com/) (ideally R 4.5.1) and the [renv](https://rstudio.github.io/renv/) package.
2. Clone the repository.
3. Open an R terminal in the root folder and run `renv::restore()` to install the required packages.
4. Run the [src/main.R](src/main.R) file.

The current abstract can be seen below:

> This thesis investigates how regime-switching (RS) models learn and forecast under different data-generating processes, and whether each regime's distribution helps explain and predict model performance. I introduce a framework that separates the _series-generating process_ (SGP) from the _regime-generating process_ (RGP), and I formalize _regime-conditional metrics_ (RC) that summarize differences between regime distributions. A Monte Carlo setup generates series, estimates models, and computes RC metrics. The framework is expandable, but I focus on: stationary $AR(1)$ series; two-regime Markov Switching (MS), Self-Exciting Threshold (SET), and Smooth Transition (ST) mechanisms; MS, SET, and ST models, plus K-Means (KM) and Random Forest (RF); RC metrics based on the mean, SD, and lag-1 ACF. Results show that RGPs and SGPs interact in non-obvious ways, but RC metrics can sometimes characterize that behavior. KM and RF are the best performers, followed by SET and ST, while MS is more flexible and performs better in asymmetric regimes; KM and ST are robust across DGPs. Mis-specifying the RGP increases RMSE by $0.52$. ST performs poorly on no-RS series, but other model-RGP interactions are generally insignificant. Matching the regime is important for performance, but not for KM. SET and ST perform best on series with high mean separation; ST performs worse with high ACF separation, and KM fares well when regimes differ in SD. Under-specifying the number of regimes is less harmful when regime distributions are minimally separated, whereas over-specifying is less harmful in the opposite case.



## Repository structure

The structure is as follows:

```txt
root/
├── docs/                   -- Thesis text
│   ├── abnt.csl, references.bib
│   ├── others/             -- Main text, presentations
│   └── main/               -- Questioning...
├── outputs/                -- Graphs and tables generated
├── src/                    -- Codebase
│   ├── main.R              -- Main interactive entry point
│   ├── parameters.R, utils.R
│   └── ...                 -- Code modules
├── renv/, renv.lock, .Rprofile, .renvignore -- R library
├── .code-workspace, .Rproj -- IDE configuration
├── .gitignore
└── README.md
```

The codebase is described in the next section. There is also an ignored [data/](data/) folder, which is used for interactively storing simulation results. It is ignored as it is not required for reproducibility.

### Text and outputs

The outputs (graphs, tables) of the thesis are saved in the [outputs/](outputs/) folder, in _.pdf_ or _.tex_ format. They are separated into diagnostics, exploratory results, and systematic results.

The thesis text is in the [docs/](docs/) folder. The [others/](docs/others/) subfolder contains a sketch of the thesis, presentations, and other materials. These are built with [Quarto](https://quarto.org/). The [main/](docs/main/) subfolder would contain the main thesis document, built directly in LaTeX, but the current Quarto setup is too good, and the main thesis is currently in [sketch.md](docs/others/sketch.md).

The markdown files relate directly to the results in [outputs/](outputs/), so results are generally not hardcoded, making the text more reproducible.

## Codebase

The codebase is modular and organized via the [box](https://klmr.me/box/) package, which allows for creating mini packages (modules) within your project. A box module has an `__init__.R` file that organizes the functions defined in the folder and exports them as a single package.

The main entry point is [main.R](src/main.R), and the overall flow is as follows:

1. [utils.R](src/utils.R) defines packages and utility functions, while [parameters.R](src/parameters.R) defines the simulation parameters (such as the number of simulations) and is used throughout the codebase.
2. The [options/](src/options/) module calls the function factories in the [creators/](src/creators/) module with specific hyperparameters to define the RGPs, SGPs, models, and metrics used in the analysis.
3. [main.R](src/main.R) loads the [diagnostics/](src/diagnostics/) and [results/](src/results/) modules, with functions to produce the graphs and tables of the thesis.
4. [main.R](src/main.R) simulates errors, series, estimates models, and calculates metrics, diagnosing each step along the way; then, results are drawn separately in exploratory and systematic sections.

Next I explain the modules and folders in more detail. Then, some technical details are listed.

### Modules and folders

#### Creator Module

The [creators/](src/creators/) module has submodules for SGPs, RGPs, metrics, and models.

For example, in [creators/rgps/](src/creators/rgps/), there is a function factory for each RGP family, such as Markov switching or smooth transition. These take hyperparameters and return a function that generates the 'next' regime given $y$ and $r$, as described in the theoretical framework of the thesis. The SGPs and models submodules are similar, with the latter having one separate file per model family. The metrics submodule has plain functions to calculate the metrics.

#### Options Module and parameters.R

The [options/](src/options/) module invokes these creator functions with specific hyperparameters, thus defining the RGPs, models, and SGPs used in this work. The metrics submodule defines the `get_metrics_data()` function, which gets the estimations and generates the metrics dataset, only calling the metrics considered.

Each submodule also defines dictionaries to associate the modules with pretty labels for the _gt_ tables and _ggplot_ graphs.

The [parameters.R](src/parameters.R) file defines the simulation parameters, as well as the DGPs, SGPs, and models created in [options/](src/options/) that should actually be included in the final analysis, saving them in the `menu` object.

#### Results and diagnostics modules

The [diagnostics/](src/diagnostics/) and [results/](src/results/) modules have the functions that generate the diagnostics and results of the thesis. The former is divided into simulation, estimation, and metrics diagnostics, while the latter is divided into exploratory and systematic results, the two results sections of the thesis.

These outputs are saved in the [outputs/](outputs/) folder, often wrapped in an `if (FALSE)` block to avoid overwriting them unintentionally. These can be removed with a `CTRL+F` for a full run of the code.

#### Others folder

The [others/](docs/others/) folder has some interactive benchmarks that are not part of the actual results, and a [scaffolding.R](src/others/scaffolding.R) file that just sets up infrastructure for the IDE VS Code.

### Technical details

- **Naming**: Variable names aim to generally follow the thesis' notation.
- **Toolbox**: The code relies heavily on tidyverse and rlang; ggplot2 is used for graphs, and gt for tables; mirai is used for parallel execution of the simulations.
- **Reproducibility**: The version of the packages used is controlled via renv to guarantee full reproducibility. renv saves a library of packages specifically for this project under the [renv/](renv/) folder. This library is ignored in git but can be recreated by running `renv::restore()`. The Quarto version used in this project is `1.6.40`, and the current LaTeX distribution is MiKTeX `25.3`.
- **Documentation**: Most functions are documented with _roxygen2_ markdown comments, especially adding prototype hints. Many of the creator functions also have parameter testing.
- **Performance**: Many functions are built for performance and for use in parallel environments, thus their environments are set as children of the base environment.



## Extensions

**Additions:**
- Allow for varying both the SGP functional form and the RN by conceptually separating them, e.g., in the menu objects in `parameters.R` and thus in the names of these SGPs with a hyphen between the SGPF and the RN.
- Generalize the regime changes to permit different error distribution families across regimes. This would require a significant change in the code, as the errors could not be pre-generated so easily. They should be pre-separated by regime natures considered and then queried given the regime nature that the RGP defines dynamically.
- Serve the full R metric matrices for the simulations and estimation results without binarizing them via `max.col`, and then implement the weighted version of the RC metric instead of the binarized version.
- Check if the codebase can deal with `n_b = 0, n_l = 0, n_h = 0`.
- Models and DGPs:
    - Add more models such as MS smooth transition and ones that blur the lines, using both ST and abrupt changes, as well as those that use both threshold and Markov-based changes.
    - Add more hyperparameterizations of the models, RGPs, and models that already exist, such as different distributions for MS, threshold with abs X or delta X.
    - Add more SGP functional forms.
    - Add more general-purpose models such as neural networks.
    - Generalize the models to handle larger than 1 `n_l, n_r`.
    - Predict MS via simulation.
    - Add an `n_r` column in `*_meta`, and create the concept of 'no regimes.'
    - Consider multivariate and ensemble models.
    - SSR minimization-based RS identification model.
- Metrics:
    - Add unconditional metrics, metrics about the residue, and residue diagnostics (quality of the residues) to be added as controls.
    - Add more RC metrics and create silhouette-like distances of the metrics, distribution distances, and more distances like squared distances.

**Improvements:**
- Rethink a new framework for dealing with the objects clumping and filtering (SGPs, RGPs, and models), which are often clumped between their variations of symmetric, asymmetric, big or small, or filtered and grouped and formatted via labels. These are done in several different approaches across the codebase and should be standardized in a single framework, more akin to what the function `clump_dgps()` does, for example.
- Save more metadata about the models, especially about convergence and diagnostics information, to better empower the estimation diagnostics section.
- Make estimation diagnostics and exploratory results overall more elegant.
- Add unit tests for the model functions because they are more like black boxes as we didn't implement the models ourselves.
- Solve the creation of NAs in sigma estimation.
- Fully remove `n_b` after simulations, instead of ignoring it via indexing.
- Use more `seq_len()` and friends.

**Aesthetics:**
- Standardize the panel ordering (what goes on rows, what goes on columns) for graphs and tables.
- Left-align footnotes with the table.

**Performance:**
- Further study parallel benchmarks, considering passing the full data as a matrix or maybe preallocating some clusters and only passing the data it gets instead of the FIFO scheduling.
- Consider other model estimation packages, maybe even ones that use GPU acceleration.

**New analyses:**
- Check if AIC or BIC selection of hyperparameters performs well and when it doesn't.
- Compare with structural breaks models, such as using them to detect outliers, and overall relation to non-RS models that have nonlinearities and other complex dynamics, and see how RS models deal with those.
- Add a **getMatrixData** version that does not calculate the dispersion of the RC metric but actually reframes and gives one observation per estimation by each regime of estimation, so that we can use the information data of regimes in analysis.
- Add rolling moments calculation and check how they relate to the true metric difference.
