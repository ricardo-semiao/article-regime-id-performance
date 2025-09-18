---
title: "Thesis Plan: \"Regime Characteristics and Prediction Performance\""
author: "Ricardo Semião e Castro"
date: today

number-sections: true
format: html
---

This is the plan for my thesis, "Regime Characteristics and Prediction Performance" (title to be decided).



# Process and Timeline

My method is to look at the big picture first. Thus, the steps taken are:

1. Define the general goal and path of the thesis:
    - Define the possible research questions related to the general topic (relation between regime identification and forecasting performance in RS models).
    - For each question, define the exercise needed to answer it.
    - For each exercise, define the possible results, and how each answers (or not) the related question.
    - Define the claims to defend to justify the thesis, specially in relation to the literature.
2. Check if there are conflicting or too similar articles, and study the most important topics in the literature, required to follow on.
3. After setting the, in principle, list of questions/exercises, better define the methodology.
    - Not yet explicitly define all the dgps/models/metrics to be used.
4. Sketch the structure of introduction, literature review, data, and methodology sections.
5. Write the code for the simulations:
    - Create a generalistic, that can be easily expanded.
    - Focus on ease-of-use, and also optimization (as this is an computationally expensive task).
6. Get initial results for the simpler dgps/models/metrics, check if points 1. and 2. need to be adjusted.
7. Make any adjustments, run the full simulations, get the final results.
8. Write the text.



# Results and Methodology Goals {#goals}

## Research Question

**Summarized question:**

> What is the relationship between regimes' characteristics and forecasting performance, across different regime-switching models and DGPs?

**Sub-questions:**

1. Does regimes' characteristics matters for performance? [Assumedly yes].
2. How does regimes' characteristics matters?
    - In general, which characteristics matter, which don't?
    - Does the answer for the above change across different contexts? Specially, across DGPs and models?
    - What is important about each characteristic: the absolute value, the difference with the other regimes, the difference with the DGP?
    - Are both within-regime performance and the global performance of the model affected?
3. What practical use can be made of this information?
    - How does this discussion contribute to differentiating each RS model?
        - Is there evidence for a universal approximator? That is, is there a model that performs well in all cases?
    - Are there practical recommendations for metrics that econometricians should calculate when creating RS models?

**Claims to defend:**

- About the literature:
    - There is demand for more studies on RS model performance. [Very generic, we could go straight to the next one].
    - Little attention has been given to the relation between the characteristics of the regimes and the forecasting performance of the model.
    - More could be learned about in which context each RS model performs better.
- About why regime identification is important:
    - It could provide researchers with tools to better understand the expected performance of their models, by analyzing the characteristics of the estimated regimes.
    - It offers further information on the differences between RS models, and in which types of regime structures each performs better.
    - [Essentially, if my exercise yields useful practical recommendations, it is important; otherwise, not as much (and that is a result in itself).]


## Possible Exercises

First, some comments:

- Note that there is not a way to compare regimes between estimated and true models, as there is not a correspondence of ordering and such.
- Let $d$ denote the DGP, $m$ the model, $s$ the simulation, and $r$ the regime.
- Each result can vary across DGPs and models, but note that this can be considered in a qualitative fashion (e.g., across "AR(1)" and "AR(2)"), or in a quantitative fashion on the empirical characteristics of the series. The latter is more complex, as opens the door to think of all the relevant characteristics.

There are too many ways to associate regime-conditional metrics and performance, specially when having both the estimated and the true serie. Thus, i'll focus on approaches that only involve information known by an econometrician.

1. Calculate the metrics and performance for each regime, that is, a set of $(d, s, m, r)$ metrics + performance, and run a regression on performance ~ metrics + ....
    - For predictive performance, we only have 1 datapoint, of a single regime, per $(d, s, m)$, for the predicted regime (assuming only 1-step ahead). For fit performance, we have 1 datapoint per regime for each model.
    - One could also use the observation level, but I'm not sure what are the benefits.
    - The metric can be in absolute terms or as the difference between the unconditional average.
    - The regime should be the true or the estimated?

This is useful to know, given the regime, what is the expected performance of the model. It can inform the econometrician on how much faith to put in a prediction, given the predicted regime (and its characteristics).

This is not so useful thinking that the econometrician could already calculate the empirical performance conditional on the regime, with its true series, whereas this result is local to the set of DGPs that I choose.

2. On the aggregated side, one can calculate metrics of distance across regimes of a model. Study correlations between metric and performance, across DGPs and models, for each metric (heatmaps, regressions).
    - For more than 2 regimes, can be the average of the pairwise distances, or the distance between the average of each regime, amongst others.

The idea is that there might be characteristics that are more important to be different than others, and the econometrician can easily calculate them to check if he is in a good spot or not.

A better context for "a good spot", is to maybe relate the degree of difference between regimes and the optimal choice of the number of regimes, or even to not use a regime-switching model at all (only 1 regime).

Other options are:

- Unconditional results, just to know under which characteristics each model performs better, but that is less original, and would only be useful to compare results.
- Calculate the difference true-estimated of 2.

Additionally: apply the empirically acquired knowledge.

- Define a variable of interest, initially in the area of sentiment or uncertainty indices.
- Seek institutional and empirical information about the regime-switching context related to this variable.
- Estimate different RS models, calculate the metrics and compare the results based on the knowledge obtained. The idea of a universal approximator will be especially relevant.

All exercises start from a matrix with each row being a regime - rows indexed by $(p, m, s, r)$ (DGP, model, simulation, and regime) - and columns a metric, be it regime-conditional characteristics or model performance metrics. Then, some exercises might aggregate the rows by $(p, m, s)$ only. It is from these matrices that analyses are performed.

The sketch of methodology is being done in [sketch_methodology.md](docs_extra\sketch_methodology.md).


# Code

General practices to follow:

- It would be nice to use a performant but productive language, such as Julia, but we're more comfortable with R. Still, use best performance practices:
    - Use simpler data structures, such as integers (`1L`) and matrices (even undimentioned ones) when possible.
    - Preallocate memory when possible.
    - Use rlang.
    - Use parallelization.
- We probably wont get to that, but we could use a mix of languages, specially Python for ML models.


## Parallelization Strategy

1. generate random errors: parallel via rTRMG::rnorm_trng
2. simulate series for each dgp and error: parallel over errors (with dgps loop inside), via mirai::mirai_map with dynamic scheduling
3. estimate models for each simulation: parallel, probavly via mirai::mirai_map with dynamic scheduling. here we can divide between computers
4. calculate metrics: boils down to a vector of metrics for a (dgp, model) pair. parallel with the same logic as 2, via mirai::mirai_map, but maybe static scheduling
	- of fit/predictive power of a pair (simulation, estimation)
	- of regimes of an simulation
	- of regimes of an estimation
5. comparisons of all of the obtained vectors: probably sequentially
	- heatmaps?

Actually, lets define the paralleization strategy with the true data later.



# Text

General practices to follow:

- Not write final sentences until the final results are set, focus on writing the core/objective of each paragraph.
- We or I? And similar questions about voice.
- ...

The sketch of methodology is being done in [sketch_text.md](docs_extra\sketch_text.md).
