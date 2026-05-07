---
title: "Regimes' Characteristics and Time Series Forecasting"
subtitle: "FGV-EESP Masters' Thesis"
author: "Student: Ricardo Semião e Castro\nAdvisor: Prof. Marcelo Fernandes"
date: today

bibliography: ../references.bib
csl: ../abnt.csl

number-sections: true
fig-cap-location: top

format:
    pdf: 
        title-meta: "Thesis sketch"
        keep-tex: true
        colorlinks: true
        citecolor: green
        linkcolor: orange
        urlcolor: lightblue
        include-in-header:  
            - text: |
                \usepackage[a4paper, left=2cm, right=2cm, top=2.5cm, bottom=2.5cm]{geometry}
                \input{../main/configs/rspalette.tex}
                \setlength{\parindent}{1.5em}             
                \usepackage{amsmath}
                \usepackage{mathtools}
                \usepackage{tikz}
                \usetikzlibrary{positioning}
                \usetikzlibrary{decorations.pathreplacing}
                \usepackage{algorithm}
                \usepackage{algpseudocode}
                \usepackage{float}
                \usepackage{multirow}
                \usepackage{multicol}
                \usepackage{booktabs}
                \usepackage{pdflscape}
                \usepackage{graphicx}
                \makeatletter         
                \renewcommand\maketitle{
                    {\raggedright
                    \begin{center}
                    {\Large \bfseries \sffamily \@title }\\[4ex] 
                    { \@author}%\\[4ex] 
                    %\@date\\[8ex]
                    \end{center}}}
                \makeatother
                \DeclareMathOperator*{\argmax}{arg\,max}
                \DeclareMathOperator*{\argmin}{arg\,min}
                \setcounter{tocdepth}{2}
                \setcounter{secnumdepth}{3}
                \numberwithin{equation}{section}
                \let\oldsection\section
                \renewcommand\section{\clearpage\oldsection}         
---

```{=tex}
\newcommand{\sgp}{\text{sgp}}
\newcommand{\rgp}{\text{rgp}}
\newcommand{\dgp}{\text{dgp}}
\renewcommand{\mod}{\text{mod}}
\newcommand{\met}{\text{met}}
\newcommand{\disp}{\text{disp}}

\renewenvironment{quote}
    {\list{}{\rightmargin\leftmargin}%
    \item\relax\color{red}}
    {\endlist}
```

```{=tex}
\begingroup
\renewcommand\section{\oldsection}
\tableofcontents
\endgroup
```

# Introduction {#sec-intro}

Regime switching (RS) models describe time series that exhibit different behavior -- different parameters -- across different regimes. They are useful to capture non-linearities in time series, having been widely used in economics and finance, for instance, to model business cycles and market volatility. There are several types of regime switching modeling, some with stochastic switching, such as Markov-switching models, and some with deterministic switching, such as threshold models.

As with any forecasting model, it is important to understand the factors that influence their performance, and how econometricians can use this knowledge to improve their models. In this work, I focus on: (i) the ability of these models to learn and generate performant forecasts under the presence of mis-specification; and (ii) how that ability relates to the characteristics of the regimes by them.

The first focus is common in forecasting econometrics: exactly identifying the data generating process (DGP) is the exception, not the rule, so the modeling goal is actually to find a robust approximator. It is important to document how each RS model behaves under different mis-specifications, explore possible universal approximators, and understand how each element of the DGP affects the learning problem of the models.

The second focus is less orthodox and specific to RS models. These models are special in the sense that they not only identify the series in question but also its states -- its regimes -- thus allowing the econometrician to describe the distribution of each regime and how different they are from each other. This characterization of regimes' distributions might be informative for the model's performance: for example, if the DGP implies different intercepts across regimes, a model whose identified regimes have the same conditional average is probably not capturing that dynamic well; or some class of model can be good at capturing that dynamic but bad at capturing changes on the persistence. These examples might seem obvious, but I will show that there is much useful information to be taken from this kind of analysis.

The nature of this project is explorative. I will simulate a diverse set of DGPs and try to find stylized facts about how each RS model adjusts to them, and how the characteristics of the estimated regimes relate to this adjustment. To make things more concrete, in the remainder of this section I synthesize the methodology, describe the patterns I hope to find, and present some of the actual findings.


## Basic methodology and hypothesis {#sec-intro-method}

The methodology follows a common setup. The first step is to establish a theoretical framework that describes all RS models in a unified way. Here, I denote the separate 'ingredients' in an RS DGP: the _series generating process_ (SGP) and the _regime generating process_ (RGP). By varying these 'ingredients', one can define a diverse set of DGPs to be studied. I define the notion of regime conditional (RC) metrics and the different ways that they can be calculated and compared. I propose the Monte Carlo setup to generate series, estimate models, and calculate RC metrics. The code is available and implemented in a similarly modular and expandable way.

Creating a general framework was a goal in it of itself, but for this work, I considered only a specific set of DGPs, models, and metrics, answering only some of the questions it allows to be asked.

I focus on stationary $AR(1)$ processes, with regime switching via Markov Switching, Self Exciting Threshold, and Smooth Transition, with symmetric and asymmetric variations. There the regimes can differ in one of the three parameters of the $AR(1)$, and the change can be 'big' or 'small'. A no-RS model is included as a baseline.

For the analysis, the first step is exploratory, to understand how do the regimes' distributions differ across these DGPs.  Do the metrics correctly identify the differences between the distributions? Can they help us get stylized facts to better understand how these models DGPs work?

Then, the focus is to understand if the distribution of regimes informs something about the models' performance. That is, do the models fare better when faced with a specific profile of regimes' distributions? Or does matching the distributions improve models' performance in the sense of the metrics?

For the more orthodox objectives, we first explore the general performance of the considered models in this specific pool of DGPs. Which one does better? How do the models' components relate to that performance, and how does it vary across DGP scenarios (e.g. asymmetric DGPs)? Does misspecifying the DGP generate impacts on performance, how? Which model component (fit, regimes, parameters, etc.) is more important to match, in terms of performance?

This work's goal is to answer these questions, generating stylized facts about the DGPs, and practical recommendations for the modeling of regime switching time series.

The rest of this work is divided as follows: @sec-lit presents the literature review. the general framework is presented in @sec-theory and @sec-sim, while the specific implementation chosen is presented in @sec-impl. The results are split into a more exploratory section ([-@sec-sep]) and a systematic one ([-@sec-perf]). Finally, @sec-conc concludes.



# Related literature {#sec-lit}

The regime switching literature is vast and contains many model variations. It is important to map the models, the similarities, and the differences between them. Additionally, an important starting point is to discuss what is already known about their forecasting performance and the factors that influence it. Each is done in the sections below. Before doing so, I will better define the bounds of RS literature by discussing two closely related ones.

The first is the state-space (SS) literature, with its quintessential implementation by @Kalman1960. While RS and SS models have developed as somewhat independent fields, RS can be viewed as a subset of SS, where the state (regime) variable and the observed series variable are modeled separately. This separation is central to the framework used in this paper. Bridges between the literatures include Switching State-Space Models and the seminal work by @Kim1994, which extends Hamilton's Markov-switching model to general state-space models.

The second is the structural break (SB) literature, the most relevant starting point being @Chow1960. Much of it is devoted to diagnosing breaks, which are indeed present in RS settings, with the non-constant parameters. However, SB models typically treat breaks as exogenous and non-recurring. Bridging the gap, @Bai1998 allows for multiple unknown breaks, which can be relevant for RS contexts, while @Chib1998 demonstrates that SBs can be formulated as Markov-switching processes that have only positive probability for staying in the initial regime and switching to the next, not for switching back.


## Regime switching models

Two of the most essential aspects of the different RS approaches are: (i) if the latent regime variable is modeled in a deterministic or stochastic fashion, and (ii) if the changes between regimes are abrupt or smooth.

The most common deterministic models are the threshold-based ones, where some observable variable being above or below some threshold(s) is what determines the regime. The work of @Tong1978 and [-@Tong1980] popularized the threshold autoregressive model, each regime having its own set of autoregressive parameters. Tong proposed that capturing smooth transitions between regimes would be important, and @Terasvirta1992, [-@Terasvirta1994] defined the smooth transition autoregressive model, where the distance between an observable variable and some threshold determines the continuous weight of each regime.

On the stochastic front, the Markov switching literature started with @Hamilton1989 via the MSAR model, where the regime is governed by an unobservable Markov process -- the probability of switching to another regime is constant and depends only on the current regime. This implies a geometric distribution for the number of periods in a given regime's instance[^instance]. The Markov switching smooth transition model, as defined by @Elliott2018, exists, but has more added complexity than the very natural jump from the TAR to the STAR model.

[^instance]: Throughout this document, 'regime instance' will be used to describe a contiguous period of time without switches. In a given series, a given regime can have several instances.

### Variations of the classic models

Moving forward, many variations on the regime variable modeling were created. The threshold variable can have a delay or some transformation; it can be the series itself, an exogenous variable, or even a non-linear combination of variables [@Chen2011]. The probability distributions of MS models were extended to allow different distributions for the time spent in one regime, and dependence on more past values [@Ferguson1980]. The smooth transition function has several options, with common ones being the logistic and the exponential. The models were generalized to any number of regimes, with the STAR model being equivalent to a Neural Network, as described by @Medeiros2000.

Blurring the line between deterministic and stochastic models, @Chang2017 uses threshold dynamics but adds an innovation that is dependent on the previous state's innovation, and simplifies to a MS model when the threshold dynamic is exogenous and stationary. @Wu2007 creates a half-threshold, half-random regime process. There are also unsupervised approaches to estimation, that make no assumption on the nature of the latent process, as by @Akioyamen2020, where some clustering model can be used to identify regimes, and later the functional form can be estimated for each regime separately.

As I will note in this work, the functional form across regimes and the regime process itself are fairly independent, thus other variations arise from considering more complex functions than the autoregressive one. ARMA models have their RS counterparts [@Brockwell1992]. Not only the mean, but the variance can also be modeled: the ARCH/GARCH family, very relevant for finance, have their regime switching versions ([@Hamilton1994, @Chen2011]). More recently, models such as decision trees have been adapted to the regime switching context, as by @Adam2024. Similarly, there are also models for vectors of time series.

General reviews on RS models include [@Tan2025], [@Potter2000], and [@Hamilton2020], while [@Chen2011] focuses on threshold models, [@Dijk2002] in smooth transition, and @Song2021 in Markov switching. Note that RS models are considered in both frequentist and Bayesian frameworks, with the latter inheriting a lot from the SS models estimation literature.


## Forecasting performance

There are many research topics in RS performance. I'll focus on (i) important factors that relate to model selection and hyperparametrization, to contextualize the decisions I made in this work; and (ii) comparisons between models, to contextualize the experiments I ran.


### Hyperparametrization

While RS models are frequently cited for their superior in-sample fit, which is useful for explaining historical phenomena, @Dacco1999 noted that even minor errors in forecasting the future regime state can propagate through the non-linear structure, causing the overall prediction to perform worse than linear alternatives. Furthermore, standard metrics like mean squared error may be ill-suited for evaluating non-linear time series, potentially masking the utility of these models in capturing turning points or specific economic states.

A primary challenge in RS modeling is managing the trade-off between flexibility and overfitting. The most critical decision is on the number of regimes: too few can underfit, while too many might lead to overparameterization. Similarly, allowing all parameters to switch can help capture complex dynamics and avoid mis-specification, but doing it when unneeded often dilutes out-of-sample power [@Tan2025].

Each model also has its own specificities. For Markov Switching models, the estimation method is relevant: for example, EM algorithms have been noted to balance accuracy and speed in high-dimensional settings [@Akbal2024]. Moreover, the translation of soft posterior probabilities into hard regime labels affects accuracy, and different rules have different properties [@Hall2025]. For deterministic models, the challenge lies in variable selection: identifying the correct threshold variable, delay parameter, or non-linear combination of variables remains a significant hurdle for effective specification.

### Comparisons between models

Many papers compare the different RS models in many different contexts [@Clements1998], [@Bierbrauer2004], [@Pinson2008], [@Janczura2010], [@Elias2014], [@Chen2014], [@Panopoulou2015], [@Verne2021], [@Aydin2022]. No single model is universally superior, the same context can present a different "best" model depending on the focus (e.g. nowcasting, regime identification, portfolio performance, etc.) [@Akbal2024].

TAR models are best employed when regime changes are triggered by a single, observable variable with rigid boundaries. They've shown effective for financial assets like gold prices and exchange rates, where transitions are fast rather than gradual [@Aydin2022]. However, their reliance on observable triggers is a limitation: in contexts like offshore wind power, where fluctuations are driven by complex, non-observable states, TAR models fail to capture the underlying dynamics and significantly underperform compared to latent variable models [@Pinson2008].

STAR models are theoretically appropriate for gradual economic adjustments but often face practical identification challenges. In many financial applications, the estimated smoothness parameter becomes so high that the model collapses into an abrupt threshold model, rendering the specific "smooth" specification inefficient [@Aydin2022]. However, STAR models can outperform MS in macroeconomic contexts characterized by explosive volatility, such as GDP growth requiring the capture of "brutal" transitions typical of recession phases [@Verne2021].

Markov-Switching Models (MS/MSAR) are the superior choice when regimes are driven by latent, multi-factor variables (e.g., market sentiment or meteorology) rather than a single observable index. But, with a more flexible regime framework, they have shown to be more sensitive to specification of the number of regimes [@Bierbrauer2004, @Janczura2010].



# Theoretical framework {#sec-theory}

In this section, I define the theoretical framework that guides the rest of this work. First, I define the general structure of RS DGPs, aligning all in a common mathematical representation, and relate the concepts of models and metrics to it. An important idea is the separation of the DGP into RGP and SGP.


## The general regime switching DGP {#sec-theory-dgp}

Let $y_t \in \mathbb{R}$ denote the series of interest at time $t \in 1:T$[^colon], $T \in \mathbb{N}$. Let $S \in \mathbb{N}$ denote the number of regimes. The _regime variable_ is a vector $r_t \in \mathbb{R}^S$ of 'weights' for each regime, indexed by $r^s_t$, $s \in 1:S$.

In this work, I consider only univariate series.

[^colon]: Let $a:b \coloneqq \{a, a+1, \dots, b\}$ for $a \leq b \in \mathbb{Z}$, and $y_{a:b} \coloneqq \{y_a, \dots, y_b\}$.

A DGP can be written in terms of a pair: _regime generating process_ (RGP) and _series generating process_ (SGP). This is essentially the separation between the state/system equation, and the output/measurement equation, in state space models. They are functions with parameters $\Theta_r$ and $\Theta_y$, respectively, such that:

\begin{equation}
\begin{array}{rrlllll}
    r_t &= \rgp(&y_{1:(t-1)}, &r_{t-1}, &t &;~ \Theta_r &)\\
    y_t &= \sgp(&y_{1:(t-1)}, &r_t,     &t &;~ \Theta_y &)\\
        &= \sgp(&y_{1:(t-1)}, &\rgp(y_{1:(t-1)}, r_{t-1}, t; \Theta_r), &t &;~ \Theta_y &)
\end{array}
\end{equation}

Without loss of generality, I restrict the regime weights to be non-negative and sum to one, i.e. $r_t \in [0, 1]^S$ and $\sum_{s = 1}^S r^s_t = 1$. Notably, the number of regimes $S$ is a parameter in $\Theta_r$, and $\Theta_y$ is actually a set of different parameters for each regime, each indexed by $\Theta^s_y$. This means that the SGP can be written as:

\begin{equation}
    \sgp(y_{1:(t-1)}, r_t, t;~ \Theta_y) = \sum_{s = 1}^S f_{\sgp}(y_{1:(t-1)}, t;~ \Theta^s_y) \cdot r^s_t
\end{equation}

Note how each regime is weighted by the regime variable $r^s_t$. In the simplest case, this weight is binary ($r_t \in \{0, 1\}^S$) -- only one of the regimes is 'turned on', and all the others are 'turned off'. However, in some models, such as Smooth Transition, the weights can be continuous, with different regimes being partially 'on' at the same time.

Furthermore, $\Theta_y$ could encode different functional forms for each regime. As this is not common, I refer to $f_{\sgp}$ as the _SGP functional form_, or simply _SGP_, and the set of parameters $\Theta_y$ as the _regime nature_, as they define what actually changes between each regime. Figure 1 illustrates this structure.

\begin{figure}[H]
    \centering
    \caption{The general RS DGP structure.}

    \begin{tikzpicture}[font=\sffamily]
    % Styles:
    \tikzset{mybrace/.style={decorate, decoration={brace, amplitude=10pt, raise=1.3ex}}}
    \tikzset{node distance = 0.25cm and 0.1cm}

    % Main nodes:
    \node[] (dgp) {DGP};
    \node[] (e)   [right = 0.8cm of dgp] {$=$};
    \node[] (sgp) [right = 0.8cm of e] {SGP};
    \node[] (p)   [right = 1.4cm of sgp] {\&};
    \node[] (rgp) [right = 1.4cm of p] {RGP};

    % Lower nodes:
    \node[] (p2)   [below = 0.5cm of sgp] {\&};
    \node[] (csgp) [left  = of p2] {functional form};
    \node[] (rsch) [right = of p2] {regime nature};

    % Math:
    \node[] (msgp)  [above = of sgp] {$\sum_{s = 1}^S \sgp(. ~;~ \Theta_y^s) \cdot r_t^s$};
    \node[] (mrgp)  [above = of rgp] {$r_t = \rgp(. ~;~ \Theta_r)$};
    \node[] (mcsgp) [below = 0.35cm of csgp] {$f_{\sgp}$};
    \node[] (mrsch) [below = of rsch] {$\{\Theta^1_y, \dots, \Theta^s_y\}$};

    % Braces using the defined style, add mirror in place:
    \draw[mybrace]         (csgp.west) -- (rsch.east);
    \draw[mybrace, decoration={mirror}] (mrgp.west) -- (mrgp.east);
    \draw[mybrace, decoration={mirror}] (msgp.west) -- (msgp.east);
    \draw[mybrace, decoration={amplitude=8pt}]         (mcsgp.west) -- (mcsgp.east);
    \draw[mybrace]         (mrsch.west) -- (mrsch.east);
    \end{tikzpicture}
\end{figure}

To construct a diverse set of DGPs, I combine different RGPs, SGPs, and regime natures. One of the challenges of this work is to choose a comprehensible set of these elements, and analyze their differences in a systematic but manageable way.

In the notation above I omitted the error term inside $f_{\sgp}$. Many distributions are interesting, specially fat-tailed and skewed ones. The parameters $\Theta_y$ can even encode different error distributions across regimes. However, if we constrain the same distribution across regimes, with the possible exception of a multiplicative factor, we can simplify the notation and implementation, writing the DGP as a function that receives a sequence of random errors and returns the series and the regimes:

\begin{equation}
    (y_{1:T},~ r_{1:T}) = \dgp(\varepsilon_{1:T};~ \Theta_r, \Theta_y)
\end{equation}

Consider the notation shorthand $y \coloneqq y_{1:T}$, and similarly for other variables, used for the rest of this work.

Let the set of considered DGPs be $P$ (for 'processes'). These are present in the literature, as discussed in @sec-lit, and will be defined in @sec-impl.


## Models {#sec-theory-models}

Consider a model $\mod$ as a function with (hyper-) parameters $\Theta_m$ that generates the fitted values and $H$-step ahead predictions of the series and regimes. The model can also return a set $\hat{\pi}$ of general metadata, e.g. the estimated coefficients.

\begin{equation}
    (\hat{y},~ \hat{r},~ \hat{\pi}) = \mod(y_{1:(T-H)} ~;~ \Theta_m)
\end{equation}

Notably, the number of estimated regimes $\hat{S}$ is a parameter in $\Theta_m$, which may or may not be equal to $S$.

Let the set of models be $M$ (for 'models'). Also present in the literature, they will be defined in @sec-impl-sgp.


## Regime conditional metrics {#sec-theory-metrics}

A regime-conditional (RC) metric $\met$ is function that receives a vector of series and a vector of regimes, and returns a sequence with one value for each regime. They are used to characterize the distribution of $y_t$ or $(y_t, y_{t-j})$ within each regime.

\begin{equation}
    \met: (y, r) \mapsto \mathbb{R}^{S}
\end{equation}

An example is the function that returns, for each regime $s$, the mean of the series weighted by $r^s_t$. This can be done for many common metrics, and is equivalent to mapping $(y, r)$ to the $S$ sets $R_s$ of regimes' observations[^regime_obs_set], then applying the metric to each set. The benefit of the first approach is that it is more general, allowing for non-binary -- i.e., smooth transition -- regimes.

[^regime_obs_set]: $R_s \coloneqq \{ y_t ~:~ r^s_t = \max\{r_t\} \}$.

For the joint distribution $(y_t, y_{t-j})$, the metrics are more complex, as they must consider only the windows $(y_t, \dots, y_{t-j})$ fully contained in the same regime instance. This is further described in @sec-app-metrics.

In any case, RC metrics may lump together observations from different time windows. For them to describe a well-defined regime distribution, it is required that the series be stationary within each regime. This will impose restrictions on the DGPs that this work will consider.


### Within-regime stationarity

For the distribution of a regime to be well defined, all datapoints within it must be drawn from the same distribution. Formally, _within-regime strong stationarity_ requires, for all regimes $s \in S$, a CDF $F_s$ such that:

\begin{equation}
\begin{array}{cc}
    F_s(y_{t_1}, \dots y_{t_n}) = F_s(y_{t_1+j}, \dots y_{t_n+j})  & 
    \begin{array}{c}
        \forall \{y_{t_1}, \dots y_{t_n}\}, \{y_{t_1+j}, \dots y_{t_n+j}\} \subset R_s, \\
        \forall j \in \mathbb{N}_*
    \end{array}
\end{array}
\end{equation}

As we intend to characterize the distributions with specific metrics, weaker assumptions can be made. If we restrict ourselves to the moments of the (joint) distribution, we can require the weak version. Formally, _within-regime weak stationarity_ requires[^acf_stationarity], for all $s \in S$, that:

\begin{equation}
\begin{array}{cc}
    E[y_t] = E[y_{t'}] & \forall y_t, y_{t'} \in R_s \\
    Var[y_t] = Var[y_{t'}] & \forall y_t, y_{t'} \in R_s \\
    Cov[y_t, y_{t-j}] = Cov[y_{t'}, y_{t'-j}] & 
    \begin{array}{c}
        \forall \{y_t, \dots, y_{t-j}\}, \{y_{t'}, \dots, y_{t'-j}\} \subset R_s, \\
        \forall j \in \mathbb{N}_*
    \end{array}
\end{array}
\end{equation}

[^acf_stationarity]: The ACF condition should be read as "$j$'th autocorrelations between time-points within the same regime instance should always be equal", not as "$j$'th autocorrelations of every time-point in the same regime instance should always be equal", although the latter is true for AR processes with order higher than $j$.

Processes that have a non-binary $r_t$, i.e., smooth transitions, do not have truly separated regimes, and thus, generally do not satisfy the conditions above. Thus, the metrics cannot be interpreted as, e.g., "the mean of all datapoints in a regime". Still, their information might be useful, as will be studied in this work.


### Aspects of RC metrics usage {#sec-theory-usage}

There are two important aspects of the RC metrics usage. First is whether to use the whole sequence of values for each $s$, or to condense it into a single value of dispersion across regimes. An example of the latter is the 'average pairwise distance between the RC means', a single value that describes how distant the levels of the regimes are. This is equivalent to composing a dispersion function $\disp$:

\begin{equation}
    \disp \circ \met: (y, r) \mapsto \mathbb{R}^{S} \mapsto \mathbb{R}
\end{equation}

Second, which series to use: the true or estimated ones. One can use the true values $(y, r)$ to get the characteristics of the true DGP, and the estimated values $(\hat{y}, \hat{r})$ or $(y, \hat{r})$ to get the characteristics of the estimated model[^dimension]. Another option is to calculate the difference between the former and the latter[^order]. Another option is to calculate the metric of the difference $(y - \hat{y}, r)$ or $(y - \hat{y}, \hat{r})$.

[^dimension]: Note that the value of $S$ and $\hat{S}$ can be different, and thus, so the dimension of the metric's output.

[^order]: This is only possible if $S = \hat{S}$ and there is an unambiguous way to match the estimated and true regimes.

Less generally, sometimes there are other possible estimators for the same population RC metric, instead of simply using $(\hat{y}, \hat{r})$. A special case is when the metric is a moment of the (joint) distribution, and the SGP is simple: one can simply plug the estimated parameters into the analytical formula for the moment, and generally have a better estimator. This is further discussed in @sec-app-metrics.

This framework allows for mixing and matching these options, each being useful to answer different questions. In this work, the estimated ones using $y, \hat(r)$, as these are the values available to the econometrician in practice, while the true metrics are calculated with the analytical approach. Additionally, I ignore the metrics separated by regime, and condense their information via considering only their dispersion, as this is a simpler measure and one more comparable across DGPs and models.

Let the set of metrics $(\disp \circ \met)$ be $C$ (for 'criteria'). These will be defined in @sec-impl-metrics, but are mostly based on the moments of $y_t$ and of the pair $(y_t, y_{t-j})$, $j \in \mathbb{N}$, and the performance metrics for the dependent variable.

One can also be interested in describing the RGP, with information such as the average duration of each regime instance, the transition probabilities and measures derived from it, amongst others. I'll use these as control variables in the regression analysis.

Finally, some regime-inconditional metrics can be useful, specially ones that can denote overall non-linearity, such as the 3rd and fourth moments of the overall series.



# Simulation framework {#sec-sim}

One of the partial goals of this work was to create the theoretical framework described in the last section in a very general and expandable way, that easily allows for different exercises, even if they are not considered here. Similarly, the simulation structure was designed to follow the same concept.

There are the following steps to perform the simulations:

1. Generate random errors for all the DGPs.
2. For each DGP and simulation, fit and predict the model, generating ($y, r$).
3. For each DGP, simulation, and model, obtain $(\hat{y},~ \hat{r})$.
4. For each DGP, simulation, and model, compute each metric.
5. Aggregate the metrics, performance information, and DGP and model descriptors into a dataset.


## Forecast horizon

For the forecast performance, I focus on $1$-step ahead predictions. It would be interesting to expand that, be it with locally-projected models or not.

To obtain more than one prediction per simulation, I simulate a $T - H$-long series, and obtain $H$ predictions. There are two possible approaches:

1. For each iteration $h \in 1:H$, the model is estimated with the window $h:(T-H+h-1)$, and generates $\hat{y}_{T-H+h}$.
2. The model is estimated once with the window $1:(T-H)$, then for each $h$, $\hat{y}_{T-H+h}$ is generated using $y_{1:(T-H+h-1)}$.

The second approach is computationally cheaper, allowing for more simulations and DGPs to be considered. It is the one used in this work, but note that it is less accurate to what would be done in practice, as econometricians often re-estimate their models with new data.


## Simulation hyperparameters {#sec-sim-hyper}

The hyperparameters of the simulation are as follows:

- Number of simulations: $I$. Its main effect is on the the precision of the results, and diversity of series.
- Forecast horizon: $H$ predictions of $1$-step ahead values. Also affects the precision of the results, but does not change the diversity of series.
- Total number of observations: $T$. Its main effect is on the ability of the models to learn the dynamics and separate the regimes. Results for higher $T$'s are more relevant for contexts with a lot of data, such as high-frequency financial data, while lower $T$'s are more relevant for contexts with less data, such as macroeconomic data.
- Burn-in period: $B$. Its main effect is on reducing the dependence of the initial values, but with stationary processes, this is not too problematic.

Let $i \in 1:I$, $I \in \mathbb{N}$ be the simulation index.


## Simulating series

I will only consider DGPs have the same error distribution -- but note that a DGP can have a volatility parameter multiplying its error. For each DGP, indexed by $p \in 1:|P|$, there are $I$ random error vectors created, each of size $T$. Let $\Epsilon$ denote the set of all errors. Let $\Epsilon_{p, i}$ denote the vector of errors generated for the $p$-th DGP and the $i$-th simulation. Similar indexing definitions will be used for similar collections throughout this document.

Let $Y$ and $R$ denote the sets of generated series and regime variables. They are computed given $\Epsilon_{p, s}$:

\begin{algorithm}[H]
\begin{algorithmic}[1]
    \State Initialize $Y$ and $R$
    \For{$p$ \textbf{in} $1:|P|$}
        \For{$i = 1$ \textbf{to} $I$}
            \State $Y_{p, i},~ R_{p, i} \gets P_p(\Epsilon_{p, i})$
    \EndFor
\EndFor
\end{algorithmic}
\end{algorithm}


## Estimating models

Now, for each simulation, I estimate each model, generating the sets $\hat{Y}$, $\hat{R}$, and $\hat{\Pi}$. The models are trained using only $y_{(B+1):(T-H)}$, to avoid the burn-in period and leave space for the forecast horizon.

The nesting order is the same as above, for consistency, but with an additional inner loop for the model estimation.

\begin{algorithm}[H]
\begin{algorithmic}[1]
    \State Initialize $\hat{Y}$ and $\hat{R}$
    \For{$p$ \textbf{in} $1:|P|$}
        \For{$i$ \textbf{in} $1:I$}
            \For{$m$ \textbf{in} $1:|M|$}
                \State $\hat{Y}_{p, i, m},~ \hat{R}_{p, i, m},~ \hat{\Pi}_{p, i, m} \gets M_m(Y_{p, i},~ R_{p, i})$
            \EndFor
        \EndFor
    \EndFor
\end{algorithmic}
\end{algorithm}


## Calculating metrics

Then, for each model, the dispersion of the RC metrics are calculated and stored as columns of a dataset $D$. Each row of $D$ is identified by $(p, i, m)$.

\begin{algorithm}[H]
\begin{algorithmic}[1]
    \State Initialize $D$
    \For{$(p, i, m)$ \textbf{in} $(1:|P|) \times (1:I) \times (1:|M|)$}
        \For{$c$ \textbf{in} $1:|C|$}
            \State $D_{(p, i, m),~ c} \gets C_c(Y_{p, i, m}, R_{p, i, m},~ \hat{Y}_{p, i, m},~ \hat{R}_{p, i, m})$
        \EndFor
        \State $\hat{\Pi}_{p, i, m}$ is appended to $D_{p, i, m}$.
        \State Categorical variables $(p, i, m)$ are appended to $D_{p, i, m}$.
    \EndFor
\end{algorithmic}
\end{algorithm}

Recall the discussion in @sec-theory-metrics about the two different aspects of RC metrics usage. With different options, the function $C_c$ can use different inputs ($Y, R$, $Y, \hat{R}$, or $\hat{Y}, \hat{R}$), which is represented by all four objects being passed to it. Additionally, the function could return the whole sequence of RC metrics, not a single value, then, each row would be identified by $(p, i, m, s)$.

The dataset $D$ is already in a friendly format for analyzing the relationship between the performance of each observation and the characteristics of the regimes, as well of considering stratifications by DGP and model.



# Implementation {#sec-impl}

The framework described in the last two sections is very general, and allows for a lot of different exercises. In this specific work, I focus on a specific set of DGPs, models, and metrics. These are described here.

First of all, I focus only on DGPs where there are regime switching, and specifically two regimes ($S = 2$). More information about the ability of the models to identify regime dynamics with different (or zero) number of regimes is an interesting topic. A no-RS model is included as a baseline.

The choices of hyperparametrization were made to balance the 'population' of DGPs. The SGP was restricted to a simple stationary $AR(1)$ process. I consider the Markov Switching, Self Exciting Treshold, and Smoothe Transition RGPs, eachwith equal representation, and with a symmetric and an asymmetric variation. There are two RNs for each of the three $AR(1)$ parameters, a big and a small change. The related hyperparametrizations were chosen guided by the concept of "regime separation", described in @sec-sep.

The choice of models and their hyperparametrization is more flexible, as they do not affect the 'population' of the experiments. Each of the RGPs' empirical model counterparts is used, with a 'generic' hyperparametrization. But it would be interesting to increase the diversity of models.

The metrics are limited to the most essential descriptors of the regime distributions, the 1st, 2nd moments, and the lag 1 autocorrelation. This is another set that could be expanded easily. Performance and RGP-related metrics were also defined for the regression analysis.

Finally, some diagnostics on the series generation and model estimation are included, as well as describing the final dataset.


## Considered SGPs {#sec-impl-sgp}

The functional form of the SGP could be important in its interaction with the other ingredients of the DGP. Additionally, some topics are interested in specific SGPs, such as conditional volatility in finance and GARCH models. For now, however, this does not seem to be the main point of interest. I will consider only an $AR(1)$ process, for its simplicity, popularity, and ease of estimation.

Additionally, I'll only consider a Gaussian distribution for the error term, ignoring fat-tailed and skewed distributions. The distribution is regime-invariant, except for the multiplicative variance parameter $\sigma$.

As discussed, it is useful to consider only within-regime weak stationarity, even though many interesting DGPs are non-stationary. This restricts the absolute value of the $AR(1)$ parameter to $1$. The only SGP functional form considered is the following:

\begin{equation}
\begin{array}{ll}
    &f_{\sgp}(. ~;~ (\mu^s, \rho^s_1, \sigma^s)) = \mu^s + \rho^s_1 y_{t-1} + \sigma^s \cdot \varepsilon_t\\
    &\varepsilon_t \sim \mathcal{N}(0, 1)\\
    &|\rho^s_1| < 1, ~~ \sigma^s > 0, ~~ \forall s \in 1:S
\end{array}
\end{equation}

Several others SGP's could be considered, such as ones with transformations of $y_t$ as regressors, non-linear regression forms, or even decision trees, as in the common model Markov-switching Random Forest. Still, the $AR(1)$ is an essential building block, and its simplicity helps isolate the effects of the other ingredients.


## Considered RGPs and models {#sec-impl-rgp}

The next 'ingredient' is the RGP. I will consider the options Self-Exciting Threshold (SET), Smooth-Transition (ST), and Markov-Switching (MS). No regime switching (noRS) is included as a benchmark[^sb].

[^sb]: A structural breaks model was considered as a benchmanrk of model without reocurring regimes, but was ultimately incomparable with the other models.

Each of these RGPs has empirical model counterparts, which are also considered. <!-- There is an additional model with an unsupervised approach where the regimes are defined by some clustering technique and each regimes' AR is estimated independently afterwards (Clustering + AR, CAR). Finally, a non-RS Random Forest (RF) model is included as a benchmark. -->

The formal definition of each RGP/model is presented in @sec-app-cons, first the RGP hypothesis, then the empirical model's estimation strategy.

For all RGPs, an option with equally likely regimes and an asymmetric variation is considered.

<!-- - **Structural Breaks:**
    - A single break at $T / 2$, and a single break at $2T / 3$. -->

- **No Regime Switching:**
    - Always in regime 1.
- **Self Exciting Threshold:**
    - Fixed hyperparameters: switching based on $y_{t-1}$. Different lags are often specific to timing-related issues, and not considered here.
    - A single threshold at $0.5$, and a single threshold at $0.9$.
- **Smooth Transition:**
    - Fixed hyperparameters: switching based on $y_{t-1}$, logistic's CDF as transition function.
    - A single threshold at $0.5$, and a single threshold at $0.9$.
- **Markov Switching:**
    - Symmetric matrix, high persistence ($P(s | s) = 0.9$).
    - Asymmetric matrix, high persistence ($P(1 | 1) = 0.9$, $P(1 | 2) = 0.3$).

<!-- UPDATE -->

The values were chosen to generate a $50\%$ proportion of regime 1 in the symmetric case, and $75\%$ in the assymetric for each DGP. The @tbl-regimes_sim shows the absolute difference between the regime 1 proportion and 0.5, that is, we would like to have values close to 0 for the symmetric case, and close to 0.25 for the asymmetric case. Note that the RGP and RN interact, in such a way that the same RGP can generate different regime proportions for different RNs.

::: {#tbl-regimes_sim tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/diagnostics/regimes_sim.tex}
```

Proportion of regimes across DGPs
:::

For the models, most hyperparameters are as follows:

- All the coefficients are assumed to change across regimes, as this is a common assumption, especially in the face of possible mis-specification.
- The number of regimes $\hat{S}$ is fixed, not estimated. Models are estimated with 2 regimes.
- The values of model-specific hyperparameters are the same as the related RGP's values.


## Considered regime natures {#sec-impl-rn}

The following regime natures are considered, each representing a different way in which the SGP parameters change across regimes:

- **Mean ($\mu$) change:**
    - Small difference: ($\mu^1 = 0$, $\mu^2 = 0.5$)
    - Large difference: ($\mu^1 = 0$, $\mu^2 = 1$)
- **Persistence ($\rho_1$) change:**
    - Small difference: ($\rho_1^1 = 0.4$, $\rho_1^2 = 0.6$)
    - Large difference: ($\rho_1^1 = 0.2$, $\rho_1^2 = 0.8$)
- **Volatility ($\sigma$) change:**
    - Small difference: ($\sigma^1 = 1$, $\sigma^2 = 1.5$)
    - Large difference: ($\sigma^1 = 1$, $\sigma^2 = 2$)

<!-- UPDATE: possibly with:
- **Sign Switching ($\rho_1$):**
    - Small difference: ($\rho_1 = 0.3$, $\rho_1 = -0.3$)
    - Large difference: ($\rho_1 = 0.7$, $\rho_1 = -0.7$)
- **New Lag ($\rho_2$) introduction:**
    - Positive, small: ($\rho_2 = 0$, $\rho_2 = 0.2$)
    - Positive, large: ($\rho_2 = 0$, $\rho_2 = 0.5$)
    - Negative, small: ($\rho_2 = 0$, $\rho_2 = -0.2$)
    - Negative, large: ($\rho_2 = 0$, $\rho_2 = -0.5$) -->

Note that the regimes are always ordered increasingly by the parameter of interest. In the asymmetric RGPs, the rarer regime is always the second one, with the higher value of the relevant parameter.

The values were chosen in accordance to the regimes proportion discussed in the last section, and also to generate a reasonable level of regime separation, as described in @sec-sep.


## Considered metrics {#sec-impl-metrics}

The goal with RC metrics is to capture the change in the series characteristics across regimes. One important option is the estimated parameters of the model for each regime, e.g., $(\hat{\rho}_s)_{s \in \hat{S}}$, $(\hat{\mu}_s)_{s \in \hat{S}}$, etc. One might think that this would outshine all other metrics, but in more complex cases where more than one parameter changes, this becomes less useful. More general metrics generate benefits from their abstraction over the DGP. Additionally, in simple SGPs, there often is a metric that is directly connected to changes in parameters, such as the conditional average for changes in intercept.

In this work, I focus on the moments of the distribution of $y_t$ and $(y_t, y_{t-j})$. Specifically, the RC metrics considered are the RC mean, RC standard deviation, and RC autocorrelation of lag 1. Higher lags could be considered, but in the simple $AR(1)$ context this would bring little additional information.

As stated before, the RC mean and RC SD are simply the mean and SD of each set $R_s$. The autocorrelation is similar, but must be calculated separately for each concurrent set of observations in $R_s$. The formal definitions are stated in the @sec-app-metrics.

As the focus is on the dispersion of RC metrics, two important measures to consider are the standard deviation and the average pairwise absolute difference. For only two regimes, they are very similar and the absolute difference is more intuitive. All the metrics are composed such that all $d \circ c \in C$ return a single real value, and $d(x) = |x_1 - x_2|$.

There are some possible expansions on this work's metrics calculation. One is to use non-standard weights for the empirical moments, giving more importance to observations near the edges of regimes' instances. Another is to use a cluster separation measure, such as the silhouette score, instead of a simple absolute distance between the RC metrics. Finally, one can use distribution distance metrics, such as the Earth Mover's Distance, on the empirical distribution of each regime. These are not currently considered.

No regime-unconditional metrics are considered. The list of considered metrics is as below:

- First moment: RC mean $\hat{\mu}(y | S)$.
- Second moment: RC standard deviation $\hat{\sigma}(y | S)$.
- First autocorrelation: RC 1st autocorrelation $\hat{\rho_1}(y | S)$.


### Performance and RGP metrics

The performance metrics considered is the RMSE <!-- and MAPE --> for forecasting performance. The MSE is not included, following @Dacco1999. The fit performance is measured by $R^2$ for $y$ and binary mean error for $r$.

Other metrics pertaining to the RGP will be included in the regression analysis: the number of regime switches divided by $T$, as a measure of switching frequency; the absolute difference between the average duration of regime 1's instances and regime 2's instances, as a measure of regime asymmetry.

For works with more than two regimes, more complex measures of regime asymmetry can be used, ones that consider the whole matrix of transition probabilities.



## Simulation implementation

The implementation of the simulations, as well as their analysis in the next sections, is done with the R programming language, and the code can be found in [this paper's repository](https://github.com/ricardo-semiao/article-regime-id-performance). The code is highly modular and fully reproducible, following the intent of setting up an expandable framework.

Following @sec-sim-hyper, the chosen hyperparameters are as below. Some values are lower than they could be due to computational constraints. The choice of $T$ is discussed in @sec-sep-across.

- Number of simulations: $I = 500$.
- Forecast horizon: $H = 10$ predictions of $1$-step ahead values.
- Total number of observations: $T = 100$.
- Burn-in period: $B = 4$.
- As described above, there are $6$ RNs, $7$ RGPs, and $4$ models.

The error sequences were generated in parallel, using [`rTRNG::rnorm_trng`](https://github.com/cran/rTRNG). The models were estimated with [`stats::lm`](https://github.com/SurajGupta/r-source/tree/master/src/library/stats), [`mbreaks::dofix`](https://github.com/cran/mbreaks), [`tsDyn::setar`](https://github.com/cran/tsDyn), [`tsDyn::lstar`](https://github.com/cran/tsDyn), [`MSwM::msmFit`](https://github.com/cran/MSwM).


## Simulation diagnostics and removals {#sec-impl-diag}

On top of visualizing the series and guaranteeing no missing values, some diagnostics on the simulation, model estimation, and metrics calculation are performed. All of the diagnostics are presented in @sec-app-diag.

The errors should be i.i.d. Gaussian with mean $0$ and should not present any pattern, especially across the parallelization structure. This is guaranteed by the TRNG library, but it is also checked.

Some observations had to be removed. Table @tbl-estimation_issues lists the reasons and the amounts. Some models did not converge and produced no output. Others couldn't estimate some parameters. The need to remove those is straightforward. Some models' predictions were dominated by one regime and produced zero or only one observation of the other. While this is not a failed estimation, calculating the dispersion of regimes distributions is impossible in these cases, and they would be removed from most analysis anyway.

The last removal is less straightforward. Some models generated errors unreasonably big, and parameters unreasonably out of the normal range that they would be commonly disregarded. This is more subjective and prone to cherry-picking, thus I was parsimonious in this removal. Only RMSEs higher than 50, means higher than the 90th quantile of the whole dataset, $\rho$s higher than 10 standard deviations of itself.

::: {#tbl-estimation_issues tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/diagnostics/estimation_issues.tex}
```
Estimation issues
:::

The average coefficient generated by the models is presented in @tbl-coefs_table. Only the matched model-RGP pairs are included, and a test of difference agains the true parameter is performed. Many tests don't pass, but that is expected, as all parameters are allowed to change across regimes. It is important to note that they generally always do (except for $\sigma$).

To consider the metrics estimation, the regime-conditional and unconditional moments are estimated and tested against their true values in @tbl-metrics_table. The regime-conditional true values are calculated as the standard $AR(1)$ moments, but the unconditional true values are not calculated. Again, the tests are not expected to pass, specially given their high power.

As a final placebo test, the forecast performance (RMSE) was regressed against the index $i$ of the simulation. The @tbl-i_independence shows genreally no relation, as expected.

<!-- \begin{equation}
    rmse_{p, i, m} = \beta_0 + \beta_1 i + \varepsilon_{p, i, m} \label{eq-rmse-sim}
\end{equation} -->

::: {#tbl-i_independence tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/diagnostics/i_independence.tex}
```
RMSE and simulation index relationship
:::

<!-- UPDATE -->



# Regimes separation {#sec-sep}

The goal of this section is to explore how the information of the regime distributions, as captured by the RC metrics, tells us about the DGPs and models. This information will be useful to put the systematic results of the next section into perspective, and also present some stylized facts for the econometrician.

The first two sections,  [-@sec-sep-in] and [-@sec-sep-across] summarize, for each DGP, how separated the regimes are in terms of each metric, considering the whole sample, and then varying the sample size. @sec-sep-models explores how the models capture the regime separation.


## Regime separation in $T$ {#sec-sep-in}

The first step is to understand what each DGP implies for the distribution of $y_t$ across regimes. A direct way to do this would be to plot the regime-conditional distributions (and, ideally, the joint distribution of $(y_t, y_{t-1})$). However, those visualizations quickly become hard to digest when repeated across many DGPs and hyperparameters.

The approach in this work is instead to characterize each regime distribution with regime-conditional metrics, and to summarize "how different the regimes are" by a dispersion across regimes. As stated before, more metrics than the ones considered here could be necessary to fully capture the differences between regimes.

The first object of this subsection is the @tbl-metrics_sep_t. It should be read as a compact "profile" of the DGP in terms of regime separation. Each _row_ corresponds to one DGP configuration, grouped by the regime generating process (RGP) and the regime nature's parameter (RN); For each RC metric (mean, lag-1 autocorrelation, and standard deviation), there are two _columns_ corresponding to the "small" and "big" parameter changes of the RN; Each _cell_ is the absolute difference of the corresponding RC metric across the two regimes, as well as the '(SD)' and non-zero test p-value's stars. The table uses only symmetric RGPs, as well as 100 of the simulation indexes.

::: {#tbl-metrics_sep_t tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/exploratory/metrics_sep_t.tex}
```
Regimes' metrics separation across DGPs
:::

In the first line, MS and $\mu$ change, we can see that the average separates the regimes, and only it, as would be expected. Then, with a change in $\rho_1$, we can see that both the ACF and the SD differentiate the regimes. This is because the standard deviation depends on $\rho_1$. When $\sigma$ changes, only SD differentiates the regimes. This creates a mapping that connects which estimated metric shows differenciation into which parameter is changing in the DGP. This is not a silver bullet, as this mapping will not be the same as we consider other RGPs.

MS has the cleanest result because its RGP doesn't depend directly on $y$, so it interacts less with the RN. With (SET, $\mu$), the average is separated, but also the (big) ACF and SD. This is because when in the 'big' regime and have a higher average, there is have a feedback loop of keeping having higher values. With a $\rho_1$ change, have also an average increase, because in the 'big' regime and we have a multiplication of high $\rho_1$ with high values increasing the average. With $\sigma$ a similar effect occours, as in the 'big' regime we have a bigger variation and reach bigger maximums, but in the 'small' regime we have a smaller variation and reach smaller minimums, increasing the average difference across regimes.

With ST, we have a similar result, as its RGP is very similar to the SET. Notably, the ST has a higher separation in the 'small' RNs, otherwise non-significant for SET. Overall this table exemplifies the importance of the RGP and RN interaction. This will be relevant to understand the models' performance and how the metrics' profiles vary with it.


## Regime separation across $t$ {#sec-sep-across}

It is intuitive that the sample size is an important factor in the ability of the estimated metrics to denote separations in the regimes. The ability to learn regime dynamics depends not only on which parameter changes (RN), but also on how quickly the induced regime separation becomes statistically visible as $T$ increases. To study the interaction between RGP, RN and $T$, I calculate each metric with the data up to each time-point ($1:2$, $1:3$, $\dots$, $1:T$). By graphing the last time-point considered on the x-axis, and the difference of the RC metric between regimes on the y-axis, we can see how the separation evolves across sample size.

The figures [-@fig-rs-ms, @fig-rs-set, and -@fig-rs-st] presents the results. The x-axis is the last time point included in the "effective sample", used to calculate the line and ribbon at that x value; Each line is the average (across simulations) of the metrics' dispersion, and the ribbon is calculated with the standard error (across simulations) times 1.96. Both are stratified by RGP symmetry, indicated via color. The graphs use only the big RNs, and 20 simulation indexes.

We have two main results. The first is the effect of asymmetry. The average of the values across asymmetric and symmetric is very similar, but the standard deviation is not. This is because the standard deviation is constrained by the small amount of observations in the less frequent regime, and this number grows very slowly across time.

The second result is that the values and their standard deviations seem to be more or less stabilized around $T = 60$, which means that for a series of this size, between 60 and 100, which is the observation size of this work, we possibly would have similar results on the usefulness of these metrics. But for series smaller than that, the metrics would be significantly less informative.

![Regime separation - MS](../../outputs/exploratory/metrics_sep_ms.pdf){#fig-rs-ms height=40%}

![Regime separation - SET](../../outputs/exploratory/metrics_sep_set.pdf){#fig-rs-set height=40%}

![Regime separation - ST](../../outputs/exploratory/metrics_sep_st.pdf){#fig-rs-st height=40%}


## Regime separation and models {#sec-sep-models}

To study how the models estimation relate to the regime separation, one could compare the estimated metrics with the true ones. While this is interesting, it is more in line with an identification exercise, and not with the learning focus of this work.

More usefulness comes from relating the model output (estimated fit, $r$, and parameters) to the performance of the models. The fit and parameters, continuous variables, are studied in @sec-perf-id. The binarized regime assignment has special importance, as it is directly related to regime separation, and can be used to split the forecasting errors into "correctly identified" and "incorrectly identified" observations. The figures [-@fig-rp-nors, @fig-rp-ms, -@fig-rp-set, and -@fig-rp-st] compare the distribution of forecasting errors, conditional on whether the underlying regime was correctly identified. Each panel corresponds to a DGP configuration, and presents the distribution of forecasting stratified by regime correctness; Each figure corresponds to one model. The graphs are done with big RNs, symmetric RGPs, and 100 simulation indexes.

In general terms, the 'correct' distributions have slimmer tails, as would be in line with the literature. But it is important to note it is not always the case, specially for the MS model, a result that will be discussed in @sec-perf-fe. For the ST model with no-RS RGP, we have overall high errors with bi-modal distributions, which will be discussed in @sec-perf-mis.

![Regime and series prediction - no-RS](../../outputs/exploratory/rmse_regimes_r1_nors.pdf){#fig-rp-nors height=40%}

![Regime and series prediction - MS](../../outputs/exploratory/rmse_regimes_r2_ms.pdf){#fig-rp-ms height=40%}

![Regime and series prediction - SET](../../outputs/exploratory/rmse_regimes_r2_set_x.pdf){#fig-rp-set height=40%}

![Regime and series prediction - ST](../../outputs/exploratory/rmse_regimes_r2_st.pdf){#fig-rp-st height=40%}



# Performance analysis {#sec-perf}

In this section, the goal is to obtain systematic results on the performance of models, and how the regimes' distributions might affect and inform about it.

First, I analyze the overall performance of each model through their fixed effects. I (i) add controls to understand what component of the models is most related with their performance; and (ii) Stratified the DGPs to understand how these performances change in different scenarios.

Then, I look into the effects of misspecification in performance. I (i) analyze the effect of model-RGP mismach, and interact it with the DGP options, to see if the mismatch effect change across scenarios; (ii) analyze specific model-DGP pairs; and (iii) consider a different way to describe the DGPs, in terms of the RC metrics that they generate, and check how each model fares against each profile of series, trying to generate practical information for the econometrician.

Finally, I analyzed which model component (estimated fit, $r$, parameters, or metrics) is more associated with each models' performance. For each model, there are different points of interest for the econometrician.

All the regressions have RMSE as the dependent variable, so higher coefficient values imply a worse performance associated with the given variable. The metrics and parameters are normalized $|x - median(x)| / mad(x)$, except for the RMSE. Some metrics are not available for all observations, such as the ACF that requires at least one length-2 instance of each regime in the series, thus the number of observations in each regression can vary.


## Models' fixed effects {#sec-perf-fe}

The fixed effects are presented in @tbl-fe_base. It excludes the no-RS model. The first column without controls, indicates that the SET and ST are similarly better than MS. This can be due to the higher prevalence of threshold-based DGPs than Markov-based in the DGP pool.

Tto understand how the qualities of the models compose each of these fixed effects, let's add controls for matching the fit, $r$, parameters, and metrics' dispersion, then remove them one by one. A better FE without a control means that part of the FE is explained by the ability of the model to match that aspect of the DGP. Comparing (2) and (3), SET and ST are fixed, while MS has an improvement, given its flexible structure that can fit very general data, compared to the rigidity of the treshold-based models.

With Columns (2) and (4), we can see the counterpart effect: MS has a bad time matching the regimes, given its geometric distribution that is often not in line with threshold-based regimes. ST is slightly worse than SET, but this is because the regimes measure is binarized, doesn't utilize the continuity of ST regimes.

Comparing (2) and (5), none of the models' performance is improved by matching the parameters, specially ST, which can often generate very unreasonable estimates. In opposition, matching the metrics is a big part of the models' FEs, specially for ST. This is and important realization, as at a first glance, the effects of parameters and metrics could seem interchangeable, but they are not.

::: {#tbl-fe_base tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/fe_base.tex}
```
Models fixed effects
:::

To understand how these fixed effects vary across different DGPs, @tbl-fe_strat shows the fixed effects with no controls, but with stratifications in the dataset. It excludes the no-RS model. Colum (1) is the same as before. While the MS model showed a poorer relation with regime matching, in column (2) we can see that its flexiblity pays off, as it performs better in assymetric regimes. In Column 3, ST deals better with small regime natures than SET. This is related with @@tbl-metrics_sep_t, where SET series were not fully separated in small regime natures, while in the ST ones were, and it is reflected in the fixed effects.

Changes in $\mu$ (4) generate a better setup for separating the series via threshold, improving SET and ST. Changes in $\rho$ (5) are poorly captured by ST, but nicely captured by MS. In a $\sigma$ change (6), everyone is worse, as it is overall harder to forecast, and none of them does a specific better job at it.

::: {#tbl-fe_strat tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/fe_strat.tex}
```
Models fixed effects - across stratifications
:::


## Mis-specification and performance {#sec-perf-mis}

### Overall effect

To study the effect of misspecification, @tbl-mis_is defines mis-specification as "the family of the model being different than the family of the RGP". The baseline effect of mis-specification is an RMSE increase of $0.547$. To further understand how this effect changes across stratifications, consider the columns (2-6). (2) MS models suffer more, in line with previous results, ST suffer less. (3) Mis-estimating a RS model in a non-RS RGP is disastrous, and in light of that, mis-specification across RS RGPs is not so relevant. This will be further explored below.

In (4), asymmetric regimes are harder to estimate, in such a way that sometimes misspecification helps. (5) Similar as before, $\sigma$ changes makes forecasting harder, and this effect is compounded by misspecification, while on the other two parameters is similar. Finally, (6) shows that in small RNs, estimating the wrong regime generates a smaller error, so mis-specifying the RGP is not too problematic.

::: {#tbl-mis_is tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_is.tex}
```
Mis-specification - overall effect
:::


### Across RGPs and RNs

Misspecification in general is bad. But let's stratify against each of the RGP families in @tbl-mis_rgp. We can see that each model (line) has equal values between the different RGPs (columns). This confirms that correctly-specifying the RGP family is not the most relevant quality for performance.

With SET, the values are positive, meaning that the model does well with no-RS series. For ST, the values are highly negative, meaning that no-RS series generate disastrous results, and this is carrying the result seen in @tbl-mis_is column (3). For MS, RS or no-RS matters less.

::: {#tbl-mis_rgp tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_rgp.tex}
```
Mis-specification - across RGPs
:::

I've talked about how the RGP and RN interact, and maybe the model-RGP mis-specification varies across RN. @tbl-mis_rgp_full shows that this also not the case.

::: {#tbl-mis_rgp_full tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_rgp_full.tex}
```
Mis-specification - across RGPs and RNs
:::


### Across RC metrics

Fortunately, the point of this work is that the RC metrics are another way to characterize the DGP, and in some ways, a more general one than the RGP family 'label'. I construct similar tables interacting each model with the profiles of (dispersion of) metrics in the series. As the whole profile of metrics is important, all of the interactions of $\mod \cdot d(\mu(.)) \cdot d(\rho_1(.)) \cdot d(\sigma(.))$ are included as controls.

@tbl-mis_metrics_sim uses the true values of the metrics. SET and ST have a negative effect on being estimated in series with high average separation, given the easier time to separate series via thresholds. For MS, we have an improvement given average, but a worsening given ACF separation, which can be because of the more-frequent-then-ideal regime switches in MS. The oposite is true for ST, but without statistical significance. Finally, high SD has insignificant effects, but negative for ST and MS.

::: {#tbl-mis_metrics_sim tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_metrics_sim.tex}
```
Mis-specification - across true RC metrics
:::

It is important to check if the estimated metrics generate similar results, as they are observable by the econometrician. This is done in @tbl-mis_metrics_est. The average results are similar, and the SD have even more variance. The ACF results are different, conditioning worse results for all models, specially for ST.

::: {#tbl-mis_metrics_est tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_metrics_est.tex}
```
Mis-specification - across estimated RC metrics
:::

This table could be considered as a practical recommendation to the econometrist, e.g. "if your estimated MS model' regimes have low average separation and/or high acf separation, than you should be weary of the possible forecast performance". A substitute for belief would be a more general measure of variation of the metric across time, such as the SD of the rolling metric. But, this is currently very limited, because: (i) while the metrics are general, the results are still conditional on the population of DGPs considered; (ii) more metrics could be usefull to increment the analysis; (iii) these general results are not invariant of other non-observable DGP characteristics.

On the last point, @tbl-mis_metrics_est_strat show the values of interactions between models, metrics, and the parameter of the RN. The ommited group is the interaction with $\mu$. We can see, for example, that a big ACF difference is only a bad indicator when the parameter changing was $\sigma$, not $\rho_1$.

::: {#tbl-mis_metrics_est_strat tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/mis_metrics_est_strat.tex}
```
Mis-specification - across estimated RC metrics + RN interactions
:::


## Identification and performance {#sec-perf-id}

In the Models' Fixed Effect section, we talked about the qualities of the models. It can be interesting to analyze separately how matching each of the characteristics of the DGPs relates to the RMSE of an estimation. This is done in @tbl-match.

There is a very high relationship between $R^2$ and RMSE in (1), as expected. Regime matching as an inverse relationship, but this is carried by the no-RS model, as will be seen later. The number of switches and the duration don’t seem to be very good guides of performance. The second column shows that $\rho_1$ can have an adverse effect, which is probably related to the 'messy' relationship between it and the $\sigma$ parameter, as seen before. A similar result is seen in the matching of metrics.

::: {#tbl-match tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/match.tex}
```
RMSE and identification
:::

There are other match-able characteristics of the models, such ash the $\tau$ for threshold models, the $\gamma$ for ST, and transition probabilities for MS. They are not included here, as they are not directly comparable across models, not generating interesting analysis.

To relate with the RC metrics, we can analyze the effect of matching each metric's dispersion (row) to the true value, stratified by model (column). This is a more direct way to connect the RC metrics to performance, and thus, to the econometrician's model selection process. This is done in @tbl-match_metrics. For SET, while a highly separated by average series is good, actually matching the means can be detrimental, but matching the ACF and SD is relevant. The ST has the main effect of detrimental ACF matching. MS model is more behaved, with matching the average and ACF being good for performance.

These results could also be taken as practical recommendations, e.g. "if the econometrician has a belief about the true average dispersion, and the SET estimated one doesn't match it, it might not be a problem". But again, this is very limited, as the results are conditional on the population of DGPs and on the other non-observable characteristics of the DGPs.

::: {#tbl-match_metrics tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/match_metrics.tex}
```
RMSE and metrics identification across models
:::

Finally, anoter observable value is the $R^2$, which is interacted with models in @tbl-match_r2. ST model has the biggest relationship, while MS and no-RS the lowest. The table also expands the interactions with regime matching, to show the aforementioned negative effect between no-RS and mis-matching regimes.

::: {#tbl-match_r2 tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/systematic/match_r2.tex}
```
RMSE and $R^2$ across models
:::



# Conclusion {#sec-conc}

This work studies regime switching models from a learning perspective. The objective is not to evaluate identification per se, but to understand how RS models learn under mis-specification, and how that learning relates to the information carried by the regime distributions implied by the estimated models.

The general and expandable framework plus implementation is an contribution in itself. I represent any RS DGP as a combination of a regime generating process (RGP) and a series generating process (SGP), and I formalize regime-conditional (RC) metrics as functions that characterize the regime distributions. This makes it possible to discuss model behavior both in terms of labels (RGP and regime nature) and in terms of observable regime characteristics computed from $(\hat{y},~\hat{r})$.

The framework was implemented with a constrained subset of its full capacity. I restrict attention to stationary Gaussian $AR(1)$ SGPs, two regimes, a MS, SET and ST of RGPs, and a minimal set of RC metrics (the first two moments and the lag-1 autocorrelation), summarized via distance across regimes.

Some facts about the DGPs' regime separation were presented. For the MS RGP, the metrics define a clear-cut profile: in $\mu$ changes only the average separates the regimes, in $\rho_1$ changes it is both the ACF and SD, and in $\sigma$ changes, only the SD. For SET and ST, the interaction between the RGP and the RN creates more complex profiles, where all metrics are different across regimes, and more metrics would be required to fully identify the DGP. The assymetric RGPs have a similar separation, but require larger sample size to estabilish it. With approximately 60 observations, the separation of these stationary $AR(1)$ series with $\sigma = 1$ converged. The RMSE distributions of observations with correctly identified regimes have slimmer tails, but not always, specially for the MS model.

On the performance of the models, the MS model has worse overall fixed effect, but its flexibility allows it to overall commit less egregious erros, performing better in assymetric regimes, and having the smallest relationship between $R^2$ and RMSE. The SET and ST models have better fixed effects, and fare better in intercept changes. ST is more robust to small parameter changes, but commit huge errors when estimated with a no-RS RGP.

The baseline effect of mis-specification is an RMSE increase of $0.547$. The $\sigma$ changes makes forecasting harder, and this effect is compounded by misspecification. In small RNs mis-specifying the RGP is not too problematic. Mis-specification across RS RGPs is not so relevant, and no specific model-RGP pair has different fixed effect than the correct pair.

Interactions of model-RGP-RN are similarlly insignificant, but the RC metrics are another way to characterize the DGP. SET and ST have a negative effect on being estimated in series with high average separation. For MS, we have an improvement given average, but a worsening given ACF separation. The oposite is true for ST, but without statistical significance. Finally, high SD has insignificant effects, but negative for ST and MS. With the estimated metrics, the results are mostly the same. On top of the universal limitations, these general results are not invariant of non-observable DGP characteristics.

Several analysis can be done to expand the results. A main one is to understand how the effect of mis-specifying the number of regimes relates to the regime separation. Series with lower regime separation might be able to be estimated with fewer regimes without a big performance loss, generating a recommendation for this hyperparameter selection.

The main limitation of the current implementation is external validity. Even when the analysis is phrased in terms of RC metrics, it remains conditional on a narrow population of DGPs and on a limited set of regime descriptors. A more robust assessment of the usefulness of RC metrics for the econometrician requires expanding both sides: richer DGPs (more SGP functional forms, more regimes, additional regime mechanisms, non-Gaussian errors, and possibly non-stationary settings) and richer metrics (higher moments, regime-instance aware dependence measures, and distribution-distance or separation criteria). This is the natural next step to turn the RC-metric approach into a more general model-selection and diagnostic tool.

Some results are expected, but interesting light has been shed on (i) the relationship between RGPs and RNs, specially via the metrics separation discussion; (ii) the differences between MS and SET/ST; and (iii) the effects of mis-specification. The performance of models given regimes characteristics could be of high practical use for the econometrician, but must be taken with caution and asks for further investigation.



# References {.unnumbered .unlisted}

::: {#refs}
:::

AI disclaimer: this work was generated generally without the help of large language models, except sparingly as a research tool and code autocompletions during the implementation phase.

<!-- > É necessário colocar algo assim? Talvez como nota de rodapé na primeira página? -->



{{< pagebreak >}}

```{=tex}
\appendix
\addcontentsline{toc}{section}{Appendix}
\renewcommand{\thesubsection}{\Alph{section}.\arabic{subsection}}
```

# DGPs, models, and metrics {#sec-app-cons}

## RGPs and models

### No RS (noRS)

**Hypothesis:** No regime switching, always at regime '1'.

\begin{equation}
\begin{array}{ll}
    &r^1_t(.) = 1\\
    &r^s_t(.) = 0, ~ s \in \{2, \dots, S\}
\end{array}
\end{equation}

**Empirical model:** a simple $AR(1)$ model, estimated via OLS, with no regime-switching component.


### Structural break (SB)

**Hypothesis:** Regime changes at specific time points $\tau \in (1:T)^{S-1}$. Formally:

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ \tau) = \mathbb{1}(\tau'_{s-1} < t \leq \tau'_s), ~~ \forall s \in \{1, \dots S\}\\
    &\tau \in \mathbb{N}^{S-1}, ~~ \tau_{s} > \tau_{s-1} ~\forall s, ~~ \tau' = (0, \tau, T)\\
\end{array}
\end{equation}

**Empirical model:** Given $\tau$, the model estimates $\mu$ and $\rho_1$ via OLS in each regime. $\tau$ is chosen by minimizing the sum of squared residuals over a grid search of breakpoints.

Similarly defined by @Bai1998. Review of other options by @Casini2018.


### Self-exciting threshold (SET)

**Hypothesis:** Regime changes when the series, possibly at a lag $d \in \mathbb{N}^*$, crosses specific threshold values $\tau \in \mathbb{R}^{S-1}$. Transformations of the variable can be considered[^g_abs]. Formally:

[^g_abs]: For example, $g(x) = |x|$ or $g(x) = \Delta x$.

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ (\tau, d, g)) = \mathbb{1}(\tau'_{s-1} < g(y)_{t-d} \leq \tau'_s), ~~ \forall s \in \{1, \dots S\}\\
    &\tau \in \mathbb{R}^{S-1}, ~~ \tau_{s} > \tau_{s-1} ~\forall s, ~~ \tau' = (-\infty, \tau, \infty), ~~ d \in \mathbb{N}^*
\end{array}
\end{equation}

**Empirical model:** Given $\tau$ and $d$, the model estimates $\mu$ and $\rho_1$ via OLS in each regime. $\tau$ and $d$ are chosen by minimizing the sum of squared residuals over a grid search of breakpoints and lags. One can also leave $d$ fixed. 

Similarly defined by @Tong1980. Review of other options by @Chen2011.


### Smooth transition (ST)

**Hypothesis:** Regime changes smoothly, with a continuous function $g$, often a CDF, based on the difference between the series and the threshold $\tau \in \mathbb{R}$, possibly at a lag $d \in \mathbb{N}^*$. @Medeiros2000 has shown that a generalization to $S$ regimes is a neural network, but currently, I only consider $S = 2$. Formally:

\begin{equation}
\begin{array}{ll}
    &r^1_t(. ~;~ (\tau, d, g)) = g(y_{t - d} - \tau), ~~~ r^2_t(. ~;~ (\tau, d, g)) = 1 - r^1_t(. ~;~ (\tau, d, g))\\
    &\tau \in \mathbb{R}, ~~ d \in \mathbb{N}^*
\end{array}
\end{equation}

Often, the function $g$ depends on a smoothness parameter $\gamma$, i.e., when $\gamma \to \infty$, $g \to \mathbb{1}$. This parameter can be jointly estimated with the others.

**Empirical model:** Estimated via non-linear squares of the residuals, over $\mu$, $\rho_1$ (for each regime), $\tau$, and $\gamma$. Uses some numerical optimization, which depends on starting values and does not guarantee a global optimum.

Similarly defined by @Terasvirta1994. Review of other options by @Dijk2002


### Markov-Switching (MS)

**Hypothesis:** Regime changes stochastically, following a Markov process with transition matrix $\Gamma \in [0, 1]^{S \times S}$. The probability of being in regime $s$ at time $t$ depends only on the regime at time $t-1$, often with $\Gamma$ implying some persistence. Formally:

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ \Gamma) \sim P(r^s_t = 1 | r_{t-1}) \eqqcolon \Gamma_{s, r_{t-1}}\\
    &\Gamma \in [0, 1]^{S \times S}, ~~ \sum_{i=1}^S \Gamma_{s, i} = 1 ~\forall s\\
\end{array}
\end{equation}

**Empirical model:** There are multiple algorithms, including maximum likelihood estimation, expectation maximization, and Markov chain Monte Carlo methods. The EM algorithm uses Kalman to find smoothed probabilities of $r$, then the conditional probabilities given the current guess of parameters, then the guess of parameters is updated via maximizing the likelihood given the probabilities. These two steps are iterated until convergence.

Similarly defined by @Hamilton1989. Review of other options by @Song2021.


<!-- ### Unsupervisioned clustering (UC)

**Hypothesis:** no hypothesis on the RGP.

**Model:** Unsupervised clustering techniques, such as K-Means, can be used to estimate the regimes based on $y_t$, its lags, and rolling moments. Given the regimes, $\mu$ and $\rho_1$ are estimated via OLS. This hybrid approach yields non-standard asymptotic properties. Other clustering techniques could be used, but I focus on the general K-Means clustering problem is as below:

\begin{equation}
\begin{array}{ll}
    &\hat{r}^s_t(. ~;~ (\text{norm}, \text{centroid})) = \mathbb{1}(y_t \in R_s)\\
    &R = \argmin_{R'} \sum_{s=1}^{\hat{S}} \sum_{y_t \in R'_s} \text{norm}(y_t - \text{centroid}(R'_s))
\end{array}
\end{equation}

Similarly defined by @Akioyamen2020. More clustering techniques reviewd by @Paparrizos2024. -->


<!-- ### Random forests (RF)

**Hypothesis:** there is no RS, the non-linearity is captured by the tree and ensamble structure of the RF. @Hu2022 presents a review of the time series RF literature.

**Model:** a RF is estimated based on $y_t$, its lags, and rolling moments. -->


## Metrics {#sec-app-metrics}

The estimated conditional mean and standard deviation can be calculated as, respectively:

\begin{align*}
    \hat{\mu}(y, r | s) & \coloneqq \sum_{t = 1}^T r^s_t \cdot y_t\\
    \hat{\sigma}(y, r | s) & \coloneqq \sqrt{\frac{1}{1 - \sum_{t = 1}^T{(r^s_t)^2}}\sum_{t = 1}^T r^s_t \cdot (y_t - \hat{\mu}(y, r | s))^2}
\end{align*}

Note the bias correction factor in the denominator of the RC SD, which is necessary given the estimated mean.

As noted, in the case of binary $r_t$, only the observations of regime $s$ have non-zero weights, and the formulas are respectively equivalent to:

\begin{align*}
    &\frac{1}{|R_s|} \sum_{y_t \in R_s} y_t, &\sqrt{\frac{1}{|R_s| - 1} \sum_{y_t \in R_s} (y_t - \hat{\mu}(y | s))^2}
\end{align*}

For a regime-conditional moments of ($y_{t}$, $y_{t-j}$), we must define the notion of 'being in the same regime'. Consider $r^s_t \cdot r^s_{t-j}$, which has a correct 'truth table' for binary regimes, but also has an interpretation for continuous ones: when closer to $1$, the higher the weight of both $y_t$ and $y_{t-j}$ being in regime $s$. But, this ignores that fact that $y_t$ and $y_{t-j}$ can be in the same regime, but in different regime instances. To account for that, the correct weighting should consider the whole window of $y_{t-j}, \dots, y_t$:

\begin{align*}
    &\hat{\rho}_j(y, r | s) = \frac{\sum_{t = 1 + j}^T \left(\prod_{k = 1}^j r^s_k\right) \cdot (y_t - \hat{\mu}(y, r | s)) \cdot (y_{t-j} - \hat{\mu}(y, r | s))}{\sum_{t = 1}^T \left(\prod_{k = 1}^j r^s_k\right) \cdot (y_{t-j} - \hat{\mu}(y, r | s))^2}
\end{align*}

Note the absence of bias correction. While it could be present, it can generate larger-than-one correlations, and is often omitted.

For binary regimes, this is equivalent to calculating the unweighted autocorrelation of every concurrent window of regime $s$.

In this work, I am currently using the binary version of the RC metrics, calculated after binaryzing $r$.

Recall that a RC metric returns a sequence with entries for each regime, so when describing the e.g. RC mean $\mu(y, r)$, I am refering to:

\begin{align}
    &\mu(y, r) \coloneqq \mu(y, r | S) = \left(\mu(y, r | S)\right)_{s \in 1:S}
\end{align}


### True moments of the considered DGPs

Given the weakly stationary within regimes assumption, the regime-conditional moments are independent of the RGP, and are the simple $AR(1)$ moments:

\begin{equation}
\begin{array}{ll}
    \mu(y_t | s) &\equiv E[y_t | y_t \in R_s] = \frac{\mu^s}{1 - \rho^s_1}\\
    \sigma(y_t | s) &\equiv Var[y_t | y_t \in R_s] = \sqrt{\frac{(\sigma^s)^2}{1 - (\rho^s_1)^2}}\\
    \rho_j(y_t | s) &\equiv Corr[y_t, y_{t-1} | y_t \in R_s] = (\rho^s_1)^j, ~~ j \in \mathbb{N}^*
\end{array} \label{eq-app-ar1moments}
\end{equation}

As described in @sec-theory-usage, there can be better estimators for populational RC metrics than the ones defined in the above section. One can simply plug in the estimated parameters in the equation above to get a better estimator of the moments. In this work, I use this approach, which is also more computationally efficient.


# Diagnostics {#sec-app-diag}

## Random errors

The @fig-diag-errors-dependence shows the correlation of the errors across the parallelization structure. A simple visual check shows no evident patterns and an overall low correlation, as expected.

![Random errors - Correlation across parallelization structure](../../outputs/diagnostics/error_dependence.pdf){#fig-diag-errors-dependence height=30%}

The @fig-diag-errors-distribution shows the distribution of a size 3000 sample of the errors, via the usual histogram and QQ-plot. The distribution is very close to normal, as expected.

![Random errors - Distribution](../../outputs/diagnostics/error_distribution.pdf){#fig-diag-errors-distribution height=30%}


## Estimated errors

The figure @fig-residuals_distribution shows the distribution of the estimation errors (residuals) across models, while figure @fig-forecast_errors_distribution shows the distribution of forecasting errors. Overall the distributions are as expected, and we can see that the former has fatter tails than the latter. In the former, approximately 10k observations are outside the range of the x-axis, some of which were considered outliers.

![Residuals - Distribution](../../outputs/diagnostics/residuals_distribution.pdf){#fig-residuals_distribution height=35%}

![Forecasting errors - Distribution](../../outputs/diagnostics/forecast_errors_distribution.pdf){#fig-forecast_errors_distribution height=35%}


## Regime proportions

The figure @fig-regimes_est shows the distribution of the proportion of the lowest frequent estimated regime, separated per model. Estimations below the dashed line had only two observations in the regime and had to be removed.

![Regime proportion - Distribution](../../outputs/diagnostics/regimes_est.pdf){#fig-regimes_est height=25%}


## Parameters and model metadata

Figure @fig-parameters_distribution shows the distribution of the estimated parameters across models and parameter. All the by-regime values of a parameter are clumped together in the same panel. Overall, the distributions are as expected, with approximately TODO values being outside the range of the x-axis, some of which were considered outliers.

![Parameters - Distribution](../../outputs/diagnostics/parameters_distribution.pdf){#fig-parameters_distribution height=40%}

Figure @fig-metadata_distribution shows the distribution of RGP-related metadata, such as the MS transition probabilities, ST $\gamma$, and $SET$ $\tau$. Overall, the distributions are as expected.

![Model metadata - Distribution](../../outputs/diagnostics/metadata_distribution.pdf){#fig-metadata_distribution height=35%}

Table @tbl-coefs_table compares the models parameters to the true values of the DGPs. Each group of lines corresponds to the moments of a DGP. The first two columns relate to the values conditional on regime 1 and 2, the third column gives the unconditional values. Each cell has the value of the moment, and in brackets the p-value of the null hypothesis that the moment is equal to its true value. The table uses only symmetric RGPs and big SGPs. Note that the moments don't need to be exactly the same, since all the models allow for all the parameters to change, a different assumption than that of the regime natures.


## Metrics calculation

The calculated metrics can be seen in @tbl-metrics_table. Each group of lines corresponds to the moments of a DGP. The first two columns relate to the values conditional on regime 1 and 2, the third column gives the unconditional values. Each cell has the value of the moment, and in brackets the p-value of the null hypothesis that the moment is equal to its true value. The table uses only symmetric RGPs and big SGPs. Note that the moments don't need to be exactly the same, since all the models allow for all the parameters to change, a different assumption than that of the regime natures.

```{=tex}
\begin{landscape}
```

::: {#tbl-coefs_table tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/diagnostics/coefs_table.tex}
```
Estimated coefficients across DGPs
:::

::: {#tbl-metrics_table tbl-pos="!htbp"}
```{=tex}
\input{../../outputs/diagnostics/metrics_table.tex}
```
Estimated metrics across DGPs
:::

```{=tex}
\end{landscape}
```
