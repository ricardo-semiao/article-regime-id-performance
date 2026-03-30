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
        include-in-header:  
            - text: |
                \usepackage[a4paper, left=2cm, right=2cm, top=2.5cm, bottom=2.5cm]{geometry}
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

<!-- > Dei uma grande reorganizada no texto seguindo as mudanças que falamos. Na minha visão tem duas coisas separadas: uma é framework do trabalho, seja a parte teórica de definir o DGP geral e o conceito de regime-conditional metrics, seja o framework das simulações; outra é a parte aplicada, seja os DGPs, modelos, e métricas específicas que eu vou considerar, seja os parâmetros e diagnósticos das simulações realizadas. Então organizei dessa forma, mas também daria para organizar como "Theoretica framework" -> "Considered DGPs, ..." -> "Simulation framework" -> "Simulation implementation". Coloquei as definições matemáticas no apêndice.

> Várias outras mudanças, te conto na reunião. A introdução mudou mais o linguajar (learning e não inferência). O framework teorico mudou só a parte de métricas, onde adicionei definições de estacionariedade necessárias. As seções seguintes eu principalmente reorganizei como dito no comentário acima. A "Simulation implementation" é nova, e é aquela versão mais focada dos diagnósticos.

> Fiz a revisão de literatura. -->



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

<!-- ARCHIVE: Additionally, I briefly present the literature on RS models and how my work contributes to it. -->


## Basic methodology and hypothesis {#sec-intro-method}

The methodology follows a common setup. The first step is to establish a theoretical framework that describes all RS models in a unified way. Here, I denote the separate 'ingredients' in an RS DGP: the _series generating process_ (SGP) and the _regime generating process_ (RGP). By varying these 'ingredients', I define a diverse set of DGPs to be considered. Then, Monte Carlo simulations are used to generate series, each being fitted by all RS models. As many questions can arise from the broad motivation of this research, creating a very general and expandable setup and implementation is a goal in itself. To restrain the focus, I will consider only stationary $AR(1)$ processes (SGPs), and when regime changes are assumed to exist.

For the first part of the work, processing the Monte Carlo results starts with visualizing the generated series, understanding how each DGP 'works' and how RGP and SGP interact. With this in hand, the fit of the models can be visualized, checking which models captured the dynamics in which contexts. Then, more systematic regression analysis is done, explaining the performance of each estimated model by the DGP and model used, as well as interactions between the two, which can capture measures of mis-specification.

The second part builds on the theoretical framework. I hypothesize which characteristics of regimes can be relevant for model performance in each context, e.g., the conditional average for DGPs with intercept changes. Then, these distribution metrics are calculated for the Monte Carlo results. The processing follows the same steps as before, with initial visual diagnostics of whether the metrics indeed characterize the simulated series' regimes, and if the same pattern is found in the mis-specified models' results. Finally, regression analysis is done, now including the metrics as explanatory variables.

<!-- > Aqui adicionarei o texto das perguntas mais concretas que foram pesquisadas, e os resultados. -->

<!-- TODO: Aqui falar das perguntas mais concretas de pesquisa

Some of these possible relationships are direct and expected. In the $AR(1)$ example above, it is expected that an RS model that yields regimes with different metrics on _conditional averages_ will perform better than one that does not, while a metric of _conditional volatility_ should not carry much meaning. In addition to listing and testing these expected relationships, there are further questions to be answered: which metric for a given characteristic can the models best match with the true one? Which is a better predictor of performance? How do these relationships change across different regime generating processes? For a given model, does the performance within a regime change with its characteristics?

In parallel, there are more specific questions: How does the effect of mis-specifying the number of regimes change with the degree of difference between regimes' characteristics? How does the ability to identify regimes' characteristics change with the sample size across regimes?

- Parada de ter 2 focos: Poderiamos ir alem no de considerar DGPs e modelos complexos, estudar mais a sensibilidade a má especificação, mas vamos deixar o framework/código pronto para isso, mas dar foco no segundo foco, que é a parada das métricas. Justificação: tbm é util para usar as métricas para identificar regimes, pré-modelagem, de maneira mais agnóstica. Poderia ter exercício específico pra isso
-->

The rest of this work is divided as follows: @sec-lit presents the literature review. the general framework is presented in @sec-theory and @sec-sim, while the specific implementation chosen is presented in @sec-impl. The results are split into an exploratory section ([-@sec-exp]) and a systematic one ([-@sec-sys]). Finally, @sec-conclusion concludes. <!-- UPDATE -->



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

Second, which series to use: the true or estimated ones. One can use the true values $(y, r)$ and get the characteristics of the true DGP, or the estimated values $(\hat{y}, \hat{r})$ and get the characteristics of the estimated model[^dimension]. Another option is to calculate the difference between the former and the latter[^order]. Another option is to calculate the metric of the difference $(y - \hat{y}, r)$ or $(y - \hat{y}, \hat{r})$.

[^dimension]: Note that the value of $S$ and $\hat{S}$ can be different, and thus, so the dimension of the metric's output.

[^order]: This is only possible if $S = \hat{S}$ and there is an unambiguous way to match the estimated and true regimes.

This framework allows for mixing and matching these options, each being useful to answer different questions. In this work, I focus on the estimated series, as they are the only thing available to the econometrician in practice, and on using the dispersion of RC metrics, as it is more comparable across DGPs and models.

<!-- > Não sei se foi uma tangente muito grande falar dessas outras opções que não vou usar, ainda mais porque pode não estar claro pro leitor qual tipo de pergunta cada ajuda a responder. Eu gostaria de informar essa flexibilidade do framework, mas talvez valha mais colocar isso num apêndice. -->

Less generally, sometimes there are other possible estimators for the same population RC metric, instead of simply using $(\hat{y}, \hat{r})$. A special case is when the metric is a moment of the (joint) distribution, and the SGP is simple: one can simply plug the estimated parameters into the analytical formula for the moment, and generally have a better estimator. This is further discussed in @sec-app-metrics.

Let the set of metrics $(\disp \circ \met)$ be $C$ (for 'criteria'). These will be defined in @sec-impl-metrics, but are mostly based on the moments of $y_t$ and of the pair $(y_t, y_{t-j})$, $j \in \mathbb{N}$, and the performance metrics for the dependent variable.

One can also be interested in describing the RGP, with information such as the average duration of each regime instance, the transition probabilities and measures derived from it, amongst others. I'll use these as control variables in the regression analysis.



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
            \State $D_{(p, i, m),~ c} \gets C_c(\hat{Y}_{p, i, m},~ \hat{R}_{p, i, m})$
        \EndFor
        \State $\hat{\Pi}_{p, i, m}$ is appended to $D_{p, i, m}$.
        \State Categorical variables $(p, i, m)$ are appended to $D_{p, i, m}$.
    \EndFor
\end{algorithmic}
\end{algorithm}

Recall the discussion in @sec-theory-metrics about the two different aspects of RC metrics usage. With different choices regarding the usage of true or estimated series, the function $C_c$ could receive different inputs  (e.g. $Y$ and $R$). Additionally, the function could return the whole sequence of RC metrics, not a single value, then, each row would be identified by $(p, i, m, s)$.

The dataset $D$ is already in a friendly format for analyzing the relationship between the performance of each observation and the characteristics of the regimes, as well of considering stratifications by DGP and model.



# Implementation {#sec-impl}

The framework described in the last two sections is very general, and allows for a lot of different exercises. In this specific work, I focus on a specific set of DGPs, models, and metrics. These are described here.

First of all, I focus only on DGPs where there are regime switching (or structural breaks), and specifically two regimes ($S = 2$). More information about the ability of the models to identify regime dynamics with different (or zero) number of regimes is an interesting topic.

The choices of hyperparametrization were made to balance the 'population' of DGPs. Each of the four RGPs have equal representation, each with a symmetric and an asymmetric variation. There are two RNs for each of the three $AR(1)$ parameters, a big and a small change. The related hyperparametrizations were chosen guided by the concept of "regime separation", described in @sec-exp-sep.

The choice of models and their hyperparametrization is more flexible, as they do not affect the 'population' of the experiments. Each of the RGPs' empirical model counterparts is used, with a 'generic' hyperparametrization. But it would be interesting to increase the diversity of models.

The metrics are limited to the most essential descriptors of the regime distributions, the 1st, 2nd moments, and the lag 1 autocorrelation. This is another set that could be expanded easily. Performance and RGP-related metrics were also defined for the regression analysis.

<!-- UPDATE: all -->

Finally, some diagnostics on the series generation and model estimation are included.


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

The next 'ingredient' is the RGP. I will consider the options Self-Exciting Threshold (SET), Smooth-Transition (ST), and Markov-Switching (MS). Structural Break (SB) is included to study how RS models perform in the case of breaks without reocurring regimes.

Each of these RGPs has empirical model counterparts, which are also considered. There is an additional model with an unsupervised approach where the regimes are defined by some clustering technique and each regimes' AR is estimated independently afterwards (Clustering + AR, CAR). Finally, a non-RS Random Forest (RF) model is included as a benchmark.

The formal definition of each RGP/model is presented in @sec-app-cons, first the RGP hypothesis, then the empirical model's estimation strategy.

For all RGPs, an option with equally likely regimes and an asymmetric variation is considered.

- **Structural Breaks:**
    - A single break at $T / 2$, and a single break at $2T / 3$.
- **Self Exciting Threshold:**
    - Fixed hyperparameters: switching based on $y_{t-1}$. Different lags are often specific to timing-related issues, and not considered here.
    - A single threshold at $0$, and a single threshold at $0.5$.
- **Smooth Transition:**
    - Fixed hyperparameters: switching based on $y_{t-1}$, logistic's CDF as transition function.
    - A single threshold at $0$, and a single threshold at $0.5$.
- **Markov Switching:**
    - Symmetric matrix, high persistence ($P(s | s) = 0.9$), symmetric matrix, low persistence ($P(s|s) = 0.6$).
    - Asymmetric matrix, high persistence ($P(1 | 1) = 0.9$, $P(1 | 2) = 0.7$), asymmetric matrix, low persistence ($P(1 | 1) = 0.8$, $P(1 | 2) = 0.6$).

<!-- UPDATE -->

<!-- > Tem várias outras parametrizações ja feitas no código e com o texto escrito, mas deixei só essas aqui caso algo mude. Alguma sugestão de formatação melhor do que a atual? -->

For the models, most hyperparameters are as follows:

- All the coefficients are assumed to change across regimes, as this is a common assumption, especially in the face of possible mis-specification.
- The number of regimes $\hat{S}$ is fixed, not estimated. Models are estimated with 2 regimes. <!-- UPDATE -->
- The values of model-specific hyperparameters are the same as the related RGP's values.


## Considered regime natures {#sec-impl-rn}

The following regime natures are considered, each representing a different way in which the SGP parameters change across regimes:

- **Mean ($\mu$) change:**
    - Small difference: ($\mu^1 = 0$, $\mu^2 = 0.5$)
    - Large difference: ($\mu^1 = 0$, $\mu^2 = 2$)
- **Persistence ($\rho_1$) change:**
    - Small difference: ($\rho_1^1 = 0.6$, $\rho_1^2 = 0.4$)
    - Large difference: ($\rho_1^1 = 0.9$, $\rho_1^2 = 0.1$)
- **Volatility ($\sigma$) change:**
    - Small difference: ($\sigma^1 = 1$, $\sigma^2 = 2$)
    - Large difference: ($\sigma^1 = 1$, $\sigma^2 = 4$)

<!-- UPDATE: possibly with:
- **Sign Switching ($\rho_1$):**
    - Small difference: ($\rho_1 = 0.3$, $\rho_1 = -0.3$)
    - Large difference: ($\rho_1 = 0.7$, $\rho_1 = -0.7$)
- **New Lag ($\rho_2$) introduction:**
    - Positive, small: ($\rho_2 = 0$, $\rho_2 = 0.2$)
    - Positive, large: ($\rho_2 = 0$, $\rho_2 = 0.5$)
    - Negative, small: ($\rho_2 = 0$, $\rho_2 = -0.2$)
    - Negative, large: ($\rho_2 = 0$, $\rho_2 = -0.5$) -->

<!-- > Idem, existem outras opções. Alguma sugestão de formatação melhor do que a atual? -->

Note that the regimes are always ordered increasingly by the parameter of interest. In general, the large vs. small differences will be interesting to analyze in relation to each other. To compare different types of changes, only the large differences will be considered, for simplicity.


## Considered metrics {#sec-impl-metrics}

The goal with RC metrics is to capture the change in the series characteristics across regimes. One important option is the estimated parameters of the model for each regime, e.g., $(\hat{\rho}_s)_{s \in \hat{S}}$, $(\hat{\mu}_s)_{s \in \hat{S}}$, etc. One might think that this would outshine all other metrics, but in more complex cases where more than one parameter changes, this becomes less useful. More general metrics generate benefits from their abstraction over the DGP. Additionally, in simple SGPs, there often is a metric that is directly connected to changes in parameters, such as the conditional average for changes in intercept.

In this work, I focus on the moments of the distribution of $y_t$ and $(y_t, y_{t-j})$. Specifically, the RC metrics considered are the RC mean, RC standard deviation, and RC autocorrelation of lag 1. Higher lags could be considered, but in the simple $AR(1)$ context this would bring little additional information.

<!-- > Talvez o 3ro e 4to momentos sejam interessantes, especialmente o 3ro porque alguns DGPs geram séries assimétricas. UPDATE: 3 and 4 moments if used -->

As stated before, the RC mean and RC SD are simply the mean and SD of each set $R_s$. The autocorrelation is similar, but must be calculated separately for each concurrent set of observations in $R_s$. The formal definitions are stated in the @sec-app-metrics.

As the focus is on the dispersion of RC metrics, two important measures to consider are the standard deviation and the average pairwise absolute difference. For only two regimes, they are very similar and the absolute difference is more intuitive. All the metrics are composed such that all $d \circ c \in C$ return a single real value, and $d(x) = |x_1 - x_2|$.

<!-- UPDATE: update with the chosen dispersion measures, and if more regimes are used -->

There are some possible expansions on this work's metrics calculation. One is to use non-standard weights for the empirical moments, giving more importance to observations near the edges of regimes' instances. Another is to use a cluster separation measure, such as the silhouette score, instead of a simple absolute distance between the RC metrics. Finally, one can use distribution distance metrics, such as the Earth Mover's Distance, on the empirical distribution of each regime.

The list of considered metrics is as below:

- First moment: RC mean $\hat{\mu}(y | S)$.
- Second moment: RC standard deviation $\hat{\sigma}(y | S)$.
- First autocorrelation: RC 1st autocorrelation $\hat{\rho_1}(y | S)$.


### Performance and RGP metrics

The performance metrics considered are $R^2$ for fit performance, and RMSE and MAPE for forecasting performance. The MSE is not included, following @Dacco1999.

Other metrics pertaining to the RGP will be included as controls in the regression analysis: the number of regime switches divided by $T$, as a measure of switching frequency; the absolute difference between the average duration of regime 1's instances and regime 2's instances, as a measure of regime asymmetry.

For works with more than two regimes, more complex measures of regime asymmetry can be used, ones that consider the whole matrix of transition probabilities.



## Simulation implementation

The implementation of the simulations, as well as their analysis in the next sections, is done with the R programming language, and the code can be found in [this paper's repository](https://github.com/ricardo-semiao/article-regime-id-performance). The code is highly modular and fully reproducible, following the intent of setting up an expandable framework.

Following @sec-sim-hyper, the chosen hyperparameters are as below. Some values are lower than they could be due to computational constraints.

- Number of simulations: $I = 500$.
- Forecast horizon: $H = 10$ predictions of $1$-step ahead values.
- Total number of observations: $T = 100$.
- Burn-in period: $B = 5$.

<!-- > Os valores finais podem mudar. UPDATE -->

The error sequences were generated in parallel, using [`rTRNG::rnorm_trng`](https://github.com/cran/rTRNG). The models were estimated with [`mbreaks::dofix`](https://github.com/cran/mbreaks), [`tsDyn::setar`](https://github.com/cran/tsDyn), [`tsDyn::lstar`](https://github.com/cran/tsDyn), [`MSwM::msmFit`](https://github.com/cran/MSwM).


## Simulation diagnostics {#sec-impl-diag}

The errors should be i.i.d. Gaussian with mean $0$ and should not present any pattern, especially across the parallelization structure. This is guaranteed by the TRNG library, and is checked in @sec-app-diag.

On top of visualizing the series, to further check for problems in the series generation, the regime-conditional and unconditional moments are estimated and tested against their true values. The regime-conditional true values are calculated as the standard $AR(1)$ moments. There is only an analytical formula for the unconditional moments of the SB and MS RGP, calculated via iterated expectations.

The table in @sec-app-diag shows the results. Each group of lines corresponds to the moments of a DGP. The first two columns relate to the values conditional on regime 1 and 2, the third column gives the unconditional values. Each cell has the value of the moment, and in brackets the p-value of the null hypothesis that the moment is equal to its true value.

It is expected that models with the same RGP assumption as the DGP return similar moments, so, to check the models' estimation, a similar analysis as above is done. Both diagnoses are generally consistent with the expectations.

To target the focus of this work, the forecast performance (RMSE) was regressed agains the index $i$ of the simulation. The table below shows that the coefficient is not statistically different from zero, even with the high power of the test, as expected.

<!-- \begin{equation}
    rmse_{p, i, m} = \beta_0 + \beta_1 i + \varepsilon_{p, i, m} \label{eq-rmse-sim}
\end{equation} -->

\begin{table}[!htbp] \centering
    \caption{Regression of forecast RMSE on simulation index}
\begin{tabular}{@{\extracolsep{5pt}}lc}
    \\[-1.8ex]\hline
    \hline \\[-1.8ex]
    & Forecast RMSE \\
    \hline \\[-1.8ex]
    $i$ & 0.006$^{*}$ (0.003) \\
    Constant & 0.00000 (0.003) \\
    \hline \\[-1.8ex]
    Observations & 95,739 \\
    R$^{2}$ & 0.00004 \\
    Residual Std. Error & 1.000 (df = 95737) \\
    \hline
    \hline \\[-1.8ex]
    \textit{Note:}  & \multicolumn{1}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\
\end{tabular}
\end{table}

As a final sanity check, the frequency of improbable events were annotated: $0.008\%$ of the datapoints generated were $3$ standard deviations away from the relevant mean; and $0.64\%$ of the predictions were $3$ standard deviations away from the true value.

<!-- UPDATE -->



# Exploratory analysis {#sec-exp}

The goal of this section is to explore how the information of the regime distributions, as captured by the RC metrics, tells us about the DGPs and models. This information will be useful to put the systematic results of the next section into perspective, and also present some theoretical results for the econometrician. The section is organized into two subsections.

First, @sec-exp-sep characterizes the simulated series generated by the DGPs using the regime-conditional metrics. This subsection has two objects: (i) a table that summarizes, for each DGP, how separated the regimes are in terms of each moment-based metric; and (ii) a set of graphs that show how that metric-based separation evolves with the sample size $T$, and how it interacts with the RGP and the regime nature (RN).

Second, @sec-exp-perf shifts from the true DGPs to the estimated models. It focuses on the two main sources of model error in regime-switching estimation—regime identification errors and parameter estimation errors -- and on how these errors propagate into forecasting errors. This subsection also has two objects: (i) a set of graphs comparing forecast-error distributions conditional on whether the regime was correctly identified; and (ii) a set of scatterplots relating coefficient estimation errors to forecasting errors.

The results are simply presented in the current state of this work, not discussed.


## Series characteristics and regime separation {#sec-exp-sep}

The first step is to understand what each DGP implies for the distribution of $y_t$ across regimes. A direct way to do this would be to plot the regime-conditional distributions (and, ideally, the joint distribution of $(y_t, y_{t-1})$). However, those visualizations quickly become hard to digest when repeated across many DGPs and hyperparameters.

The approach in this work is instead to characterize each regime distribution with regime-conditional metrics, and to summarize "how different the regimes are" by a dispersion across regimes. As stated before, more metrics than the ones considered here could be necessary to fully capture the differences between regimes.

The first object of this subsection is the table below. It should be read as a compact "profile" of the DGP in terms of regime separation.

- Each _row_ corresponds to one DGP configuration, grouped by the regime generating process (RGP) and the regime nature (RN).
- For each RC metric (mean, lag-1 autocorrelation, and standard deviation), there are two _columns_ corresponding to the "small" and "big" parameter changes defined in the RNs.
- Each _cell_ is the absolute difference of the corresponding RC metric across the two regimes, computed from the generated series and regimes for that DGP.

\begin{table}[t]
\fontsize{12.0pt}{14.0pt}\selectfont
\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}ll|rrrrrr}
\toprule
RGP & RN & \multicolumn{2}{c}{$\hat{\mu}(.)$} & \multicolumn{2}{c}{$\hat{\rho}_1(.)$} & \multicolumn{2}{c}{$\hat{\sigma}(.)$} \\ 
\cmidrule(lr){3-4} \cmidrule(lr){5-6} \cmidrule(lr){7-8}
\multicolumn{2}{c}{} & small & big & small & big & small & big \\
\midrule\addlinespace[2.5pt]
MS, symm. & $\mu$ & 3.35 & 0.83 & 0.00 & 0.01 & 0.03 & 0.01 \\
 & $\rho$ & 0.04 & 0.04 & 0.18 & 0.72 & 0.16 & 0.96 \\
 & $\sigma$ & 0.07 & 0.00 & 0.02 & 0.00 & 3.14 & 1.07 \\ [1.8ex]
SB, mid & $\mu$ & 3.93 & 0.97 & 0.01 & 0.01 & 0.04 & 0.00 \\
 & $\rho$ & 0.01 & 0.01 & 0.19 & 0.78 & 0.16 & 1.21 \\
 & $\sigma$ & 0.03 & 0.00 & 0.01 & 0.00 & 3.43 & 1.16 \\ [1.8ex]
SET, t = 0 & $\mu$ & 4.29 & 1.60 & 0.18 & 0.12 & 0.11 & 0.04 \\
 & $\rho$ & 0.94 & 1.74 & 0.13 & 0.68 & 0.06 & 0.58 \\
 & $\sigma$ & 2.23 & 1.33 & 0.22 & 0.10 & 2.96 & 0.99 \\ [1.8ex]
ST, t = 0 & $\mu$ & 0.18 & 0.70 & 0.16 & 0.08 & 0.00 & 0.01 \\
 & $\rho$ & 0.92 & 1.35 & 0.11 & 0.58 & 0.06 & 0.46 \\
 & $\sigma$ & 2.32 & 1.37 & 0.17 & 0.10 & 1.94 & 0.54 \\
\bottomrule
\end{tabular*}
\end{table}

The second object studies the interaction between the sample size $T$, the RGP, and the RN. There is an important interaction between these components that governs that separation, and thus, the models' ability to learn the regime dynamics. I calculate each metric with the data up to each time-point ($1:2$, $1:3$, $\dots$, $1:T$). By graphing the last time-point considered on the x-axis, and the difference of the RC metric between regimes on the y-axis, we can see how the separation evolves across sample size.

This is the main visualization used to motivate why the simulation design in @sec-impl balances RGP hyperparameters and considers "small" vs. "big" parameter changes: the ability to learn regime dynamics depends not only on which parameter changes (RN), but also on how quickly the induced regime separation becomes statistically visible as $T$ increases.

The regime-separation graphs should be read as follows.

- The x-axis is the last time point $\tau$ included in the prefix sample, which plays the role of "effective sample size".
- The y-axis is the dispersion of the relevant RC metric across the two regimes, computed on the prefix sample.
- Column-panels separate the different metrics, and the rows correspond to the different RNs (only 'big' versions considered); different colors/line types separate alternative hyperparameterizations within the same RGP (symmetric vs. asymmetric regime frequencies).

![Regime separation - SET](../../outputs/metrics/set.png){#fig-rs-set height=40%}

![Regime separation - ST](../../outputs/metrics/st.png){#fig-rs-st height=40%}

![Regime separation - MS](../../outputs/metrics/ms.png){#fig-rs-ms height=40%}


## Models' performance {#sec-exp-perf}

Estimating an RS model adds a second layer on top of the DGPs: even if the model class matches the RGP, the estimation is imperfect. In the context of this work, it is useful to separate two types of estimation error: (i) Regime identification error: the model assigns the current observation to the wrong regime (or, in smooth-transition contexts, assigns weights that do not align with the latent regime); (ii) Parameter estimation error: conditional on the inferred regimes, the model estimates $\mu$, $\rho_1$, and $\sigma$ with error. The focus here is not on identification per se (i.e., "how often the model is correct"), but on learning and forecasting: how these two types of error relate to the distribution of forecasting errors.

The first object of this subsection targets regime identification error. The graphs below compare the distribution of forecasting errors conditional on whether the underlying regime was correctly identified. They should be read as follows:

- Each panel corresponds to a DGP configuration (grouped by RGP and RN, as indicated in the figure layout).
- Within each panel, forecast errors are split by an indicator of regime correctness (correct vs. incorrect regime assignment), so the comparison is between two conditional error distributions.
- Each figure corresponds to one empirical model class (SET/ST/MS), making it possible to compare how the same diagnostic behaves across models.

![Regime and series prediction - SET](../../outputs/metrics/forecast_regime_threshold.png){#fig-rp-set height=40%}

![Regime and series prediction - ST](../../outputs/metrics/forecast_regime_stransition.png){#fig-rp-st height=40%}

![Regime and series prediction - MS](../../outputs/metrics/forecast_regime_ms.png){#fig-rp-ms height=40%}

The second object targets parameter estimation error. Since parameter errors and forecast errors are both continuous, the basic diagnostic is a scatterplot relating the two. These scatterplots should be read as follows.

- Each point corresponds to one estimated model in one simulation run (i.e., an observation indexed by $(p, i, m)$ in the simulation dataset).
- The x-axis is the estimation error of a given coefficient (e.g., $\hat{\mu}^s - \mu^s$ or $\hat{\rho}_1^s - \rho_1^s$, depending on the panel).
- The y-axis is the corresponding forecasting error (or an aggregated forecast-loss measure, depending on the figure definition).
- Colors and/or facets separate DGP features (RGP and RN), so that the same relationship can be compared across different regime-change mechanisms and regime natures.

![Prediction and coefficient errors - SET](../../outputs/metrics/scatter_threshold.png){#fig-scat-set height=40%}

![Prediction and coefficient errors - ST](../../outputs/metrics/scatter_stransition.png){#fig-scat-st height=40%}

![Prediction and coefficient errors - MS](../../outputs/metrics/scatter_markov.png){#fig-scat-ms height=40%}

A complementary identification-style visualization would be to plot the empirical distribution of the estimated parameters against the true parameter values. The scatterplot diagnostics used here are more directly aligned with the learning objective of this thesis, which is to connect estimation errors to forecasting performance.



# Systematic analysis {#sec-sys}

This section summarizes the main patterns of the simulations using standard regression summaries. The goal is to move from the descriptive diagnostics of @sec-exp to a more orthodox set of comparisons, first in terms of the DGP/model labels (RGP, RN, and model class), and then in terms of the regime-conditional metrics that an econometrician can compute from the estimated model.

The section is organized into four subsections. First, @sec-sys-general summarizes the average performance of each model across the full simulated population via simple model fixed effects.

Second, @sec-sys-rgp stratifies that comparison by the true DGP class. The goal is to quantify how model performance varies with the type of mis-specification, i.e., which empirical model is fitted under which RGP and RN. Third, @sec-sys-metrics repeats the same idea, but replaces DGP labels with the RC-metric dispersions defined in @sec-theory-metrics. This reframes the analysis in terms of observable features of the estimated regimes, rather than in terms of the (unknown) true DGP class. This is conceptually more general, but its external validity remains constrained by the limited population of DGPs considered in @sec-impl.

Finally, @sec-sys-id decomposes performance into components related to regime identification and parameter estimation, aligning with the learning motivation of @sec-intro.

The results are simply presented in the current state of this work, not discussed.


## Models' fixed effects {#sec-sys-general}

The first object is a simple fixed-effects regression, where forecast RMSE is regressed on model indicators. This summarizes how each model performs on average across the full population of simulated series.

\begin{table}[!htbp] \centering
  \caption{Simple fixed effects}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}}lc}
\\[-1.8ex]\hline 
\hline \\[-1.8ex]
 & Forecast RMSE \\
\hline \\[-1.8ex]
  SB & 0.012$^{*}$ (0.006) \\ 
  SET & $-$0.037$^{***}$ (0.006) \\ 
  ST & 0.076$^{***}$ (0.006) \\
  MS & $-$0.050$^{***}$ (0.006) \\ 
 \hline \\[-1.8ex]
Observations & 95,739 \\ 
R$^{2}$ & 0.002 \\ 
Adjusted R$^{2}$ & 0.002 \\
Residual Std. Error & 0.999 (df = 95735) \\
F Statistic & 58.259$^{***}$ (df = 4; 95735) \\ 
\hline 
\hline \\[-1.8ex]
\textit{Note:}  & \multicolumn{1}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\
\end{tabular}
\end{table}


## Mis-specification of RGP {#sec-sys-rgp}

The second object stratifies the comparison by the true regime generating process.

The next tables are presented as coefficient matrices. Each matrix is a compact representation of a regression with interactions between the true DGP class (rows) and the fitted model class (columns), under a fixed choice of hyperparameters (e.g. symmetric versus asymmetric regime frequencies, or small versus big RN). Off-diagonal cells correspond to mis-specification of the RGP, while diagonal cells correspond to the matched-RGP case.

The first table considers only the symmetric regime frequencies and the "big" version of the regime natures. The second table changes to assymetric regimes, while the third changes to "small" regime natures.

\begin{table}[!htbp] \centering
  \caption{Mis-specification model \& RGP - symmetric regimes}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} rllll}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & SET & ST & MS \\\hline
SET & 0.019 (0.019) & 0.007 (0.007) & 0.007 (0.007) \\
ST & 0.152 (0.152) & 0.001 (0.001) & 0.122 (0.122) \\
MS & 0.037 (0.037) & 0.012 (0.012) & 0.01 (0.01) \\
\hline \\[-1.8ex]
\end{tabular}
\end{table}

\begin{table}[!htbp] \centering
  \caption{Mis-specification model \& RGP - asymmetric regimes}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} rllll}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & SET & ST & MS \\\hline
SET & -0.003 (-0.003) & -0.004 (-0.004) & 0.004 (0.004) \\
ST & 0.193 (0.193) & 0.01 (0.01) & 0.145 (0.145) \\
MS & -0.002 (-0.002) & -0.001 (-0.001) & 0.012 (0.012) \\
\hline \\[-1.8ex]
\end{tabular}
\end{table}


\begin{table}[!htbp] \centering
  \caption{Mis-specification model \& RGP - small RN}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} rllll}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & SET & ST & MS \\\hline
SET & 0.014 (0.014) & 0.007 (0.007) & 0.013 (0.013) \\
ST & 0.193 (0.193) & -0.027 (-0.027) & 0.236 (0.236) \\
MS & 0.047 (0.047) & 0.003 (0.003) & 0.004 (0.004) \\ 
\hline \\[-1.8ex]
\end{tabular}
\end{table}


## Mis-specification via RC metrics {#sec-sys-metrics}

The previous subsection uses DGP labels (RGP and RN) to stratify performance. This is informative, but it is not directly usable by an econometrician, as the true DGP class is unknown. The motivation of the RC-metric framework (@sec-theory-metrics) is that, even when the DGP is unknown, the estimated model induces regime distributions, and those distributions can be summarized by metrics that are observable from $(\hat{y}, \hat{r})$.

In this subsection, the same idea of interacting "model" with "DGP" is reformulated as interacting "model" with the estimated regime characteristics, as proxied by the dispersion of RC metrics. This yields a more general description of where models perform well or poorly, but the conclusions remain conditional on the limited set of DGPs considered in @sec-impl.

\begin{table}[!htbp] \centering 
  \caption{Mis-specification via RC metrics}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} rllll}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & SET & ST & MS \\\hline
SET & -0.32 (-0.32) & -0.049 (-0.049) & 0.019 (0.019) \\
ST & -0.162 (-0.162) & -0.117 (-0.117) & 0.043 (0.043) \\
MS & -0.326 (-0.326) & -0.107 (-0.107) & 0.029 (0.029) \\
\hline \\[-1.8ex]
\end{tabular}
\end{table}

\begin{table}[!htbp] \centering
  \caption{Mis-specification via RC metrics - with interactions}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} rllll}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
 & SET & ST & MS \\\hline
SET & -1.213 (-1.213) & -1.002 (-1.002) & 0.105 (0.105) \\ 
ST & -0.105 (-0.105) & -0.716 (-0.716) & -0.006 (-0.006) \\
MS & 0.211 (0.211) & -1.026 (-1.026) & 0.067 (0.067) \\
\hline \\[-1.8ex]
\end{tabular}
\end{table}


## Identification of components and performance {#sec-sys-id}

The final object relates performance to the two estimation-error components highlighted in @sec-exp-perf: regime identification and parameter estimation. The motivation is not to evaluate identification per se, but to connect these errors to forecasting outcomes.

\begin{table}[!htbp] \centering
  \caption{}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}}lc}
\\[-1.8ex]\hline 
\hline \\[-1.8ex]
 & Forecast RMSE \\
\hline \\[-1.8ex]
  $R^2$ & $-$0.048$^{***}$ (0.003) \\ 
  $E[\text{error}(r)]$ & $-$0.040$^{***}$ (0.003) \\
  $\Delta sd(\mu)$ & 0.165$^{***}$ (0.003) \\
  $\Delta sd(\rho_1)$ & 0.249$^{***}$ (0.003) \\ 
  $\Delta sd(\sigma)$ & 0.0003 (0.003) \\
  Constant & $-$0.002 (0.003) \\
 \hline \\[-1.8ex]
Observations & 95,711 \\ 
R$^{2}$ & 0.106 \\
Adjusted R$^{2}$ & 0.106 \\
Residual Std. Error & 0.925 (df = 95705) \\
F Statistic & 2,272.862$^{***}$ (df = 5; 95705) \\ 
\hline
\hline \\[-1.8ex]
\textit{Note:}  & \multicolumn{1}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\
\end{tabular}
\end{table}

To relate with the RC metrics, we can analyze the effect of matching each metric's dispersion (row) to the true value, stratified by model (column). This is a more direct way to connect the RC metrics to performance, and thus, to the econometrician's model selection process.

\begin{table}[!htbp] \centering
  \caption{}
  \label{}
\begin{tabular}{@{\extracolsep{5pt}} ccccc}
\\[-1.8ex]\hline
\hline \\[-1.8ex]
SB & SET & ST & MS \\\hline
$\hat{\mu}$ & 0.236 (0.236) & 0.038 (0.038) & 0.213 (0.213) & -0.096 (-0.096) \\
$\hat{\rho}_1$ & 0.033 (0.033) & 0.193 (0.193) & 0.261 (0.261) & -0.021 (-0.021) \\
$\hat{\sigma}$ & 0.18 (0.18) & 0.254 (0.254) & -0.052 (-0.052) & -0.12 (-0.12) \\
\hline \\[-1.8ex]
\end{tabular}
\end{table}



# Conclusion {#sec-conclusion}

This work studies regime switching models from a learning perspective. The
objective is not to evaluate identification per se, but to understand how RS
models learn under mis-specification, and how that learning relates to the
information carried by the regime distributions implied by the estimated
models.

The main contribution is a general and expandable framework. I represent any RS DGP as a combination of a regime generating process (RGP) and a series generating process (SGP), and I formalize regime-conditional (RC) metrics as functions that characterize the regime distributions. This makes it possible to discuss model behavior both in terms of labels (RGP and regime nature) and in terms of observable regime characteristics computed from $(\hat{y},~\hat{r})$.

The framework was implemented with a constrained subset of its full capacity. I restrict attention to stationary Gaussian $AR(1)$ SGPs, two regimes, a small set of RGPs, and a minimal set of RC metrics based on the first two moments and the lag-1 autocorrelation, summarized via dispersion across regimes.

Exercises were done to answer some of the interesting questions that the framework can address. They were divided into an exploratory analysis to build intuition with metric-based diagnostics and visualizations, followed by a systematic analysis using regression summaries that (i) compare model classes in the simulated population, (ii) stratify performance by mis-specification in terms of DGP labels, and (iii) reframe the same comparisons in terms of RC metrics that an econometrician can compute from an estimated model. The results were simply presented, without discussion.

The main limitation of the current implementation is external validity. Even when the analysis is phrased in terms of RC metrics, it remains conditional on a narrow population of DGPs and on a limited set of regime descriptors. A more robust assessment of the usefulness of RC metrics for the econometrician requires expanding both sides: richer DGPs (more SGP functional forms, more regimes, additional regime mechanisms, non-Gaussian errors, and possibly non-stationary settings) and richer metrics (higher moments, regime-instance aware dependence measures, and distribution-distance or separation criteria). This is the natural next step to turn the RC-metric approach into a more general model-selection and diagnostic tool.



# References {.unnumbered .unlisted}

::: {#refs}
:::

AI disclaimer: this work was generated generally without the help of large language models, except sparingly as a research tool and code autocompletions during the implementation phase. The current state of sections @sec-exp and @sec-sys are drafts and have some placeholder texts generated with the help of LLMs.

<!-- > É necessário colocar algo assim? Talvez como nota de rodapé na primeira página? -->



{{< pagebreak >}}

```{=tex}
\appendix
\addcontentsline{toc}{section}{Apêndices}
\renewcommand{\thesubsection}{\Alph{section}.\arabic{subsection}}
```

# DGPs, models, and metrics {#sec-app-cons}

## RGPs and models

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


### Unsupervisioned clustering (UC)

**Hypothesis:** no hypothesis on the RGP.

**Model:** Unsupervised clustering techniques, such as K-Means, can be used to estimate the regimes based on $y_t$, its lags, and rolling moments. Given the regimes, $\mu$ and $\rho_1$ are estimated via OLS. This hybrid approach yields non-standard asymptotic properties. Other clustering techniques could be used, but I focus on the general K-Means clustering problem is as below:

\begin{equation}
\begin{array}{ll}
    &\hat{r}^s_t(. ~;~ (\text{norm}, \text{centroid})) = \mathbb{1}(y_t \in R_s)\\
    &R = \argmin_{R'} \sum_{s=1}^{\hat{S}} \sum_{y_t \in R'_s} \text{norm}(y_t - \text{centroid}(R'_s))
\end{array}
\end{equation}

Similarly defined by @Akioyamen2020. More clustering techniques reviewd by @Paparrizos2024.


### Random forests (RF)

**Hypothesis:** there is no RS, the non-linearity is captured by the tree and ensamble structure of the RF. @Hu2022 presents a review of the time series RF literature.

**Model:** a RF is estimated based on $y_t$, its lags, and rolling moments.


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

![Errors - Correlation across parallelization structure](../../outputs/errors/dependence.png){#fig-diag-errors-dependence height=45%}

The @fig-diag-errors-distribution shows the distribution of a size 3000 sample of the errors, via the usual histogram and QQ-plot. The distribution is very close to normal, as expected.

![Errors - Distribution](../../outputs/errors/distribution.png){#fig-diag-errors-distribution height=45%}


## Series generation and model estimation

The series diagnostics as described in @sec-impl-diag are shown in the first table. The second table is similar, but shows the estimated moments of the model that matched its line's RGP assumption. The values are the average across simulations of the moments calculated with the estimated parameters. Note that the moments don't need to be exactly the same, since all the models allow for all the parameters to change, a different assumption than that of the regime natures.

<!-- > Os valores finais dos diagnósticos podem mudar. Essas tabelas ainda não estão com o teste. UPDATE -->

\begin{landscape}

\begin{table}[t]
    \fontsize{12.0pt}{14.0pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}ll|llllllllll}
    \toprule
    & & \multicolumn{3}{c}{$\hat{\mu}(.)$} & \multicolumn{3}{c}{$\hat{\rho}_1(.)$} & \multicolumn{3}{c}{$\hat{\sigma}(.)$} \\ 
    \cmidrule(lr){3-5} \cmidrule(lr){6-8} \cmidrule(lr){9-11}
    RGP & RN & s = 1 & s = 2 & $\perp$s & s = 1 & s = 2 & $\perp$s & s = 1 & s = 2 & $\perp$s \\ 
    \midrule\addlinespace[2.5pt]
    MS, symm. & \ensuremath{\mu}, (0, 2) & .3 (.27) & 3.6 (.29) & 1.85 (.55) & .42 (.12) & .42 (.13) & .78 (.05) & 1.26 (.16) & 1.29 (.16) & 2.04 (.19) \\ 
    MS, symm. & \ensuremath{\rho}, (0.4, 0.6) & .01 (.21) & -.04 (.33) & -.02 (.2) & .32 (.13) & .48 (.14) & .48 (.09) & 1.07 (.12) & 1.2 (.17) & 1.14 (.11) \\ 
    MS, symm. & \ensuremath{\sigma}, (1, 4) & 0 (.29) & .05 (.97) & .03 (.49) & .43 (.12) & .4 (.14) & .47 (.11) & 1.35 (.21) & 4.4 (.55) & 3.18 (.55) \\ 
    SB, mid & \ensuremath{\mu}, (0, 2) & -.01 (.28) & 3.92 (.24) & 1.99 (.19) & .45 (.12) & .47 (.12) & .87 (.03) & 1.12 (.14) & 1.17 (.14) & 2.28 (.16) \\ 
    SB, mid & \ensuremath{\rho}, (0.4, 0.6) & .01 (.2) & 0 (.31) & .01 (.19) & .36 (.13) & .55 (.11) & .49 (.09) & 1.07 (.12) & 1.21 (.16) & 1.16 (.1) \\ 
    SB, mid & \ensuremath{\sigma}, (1, 4) & .02 (.27) & .05 (.95) & .03 (.5) & .46 (.11) & .46 (.12) & .47 (.11) & 1.13 (.13) & 4.5 (.48) & 3.33 (.33) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\mu}, (0, 2) & .17 (.79) & 3.96 (.19) & 3.87 (.22) & .04 (.71) & .47 (.08) & .55 (.11) & .93 (.4) & 1.15 (.09) & 1.3 (.19) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\rho}, (0.4, 0.6) & -.32 (.18) & .59 (.19) & .18 (.19) & .13 (.14) & .26 (.13) & .49 (.08) & 1.02 (.1) & 1.08 (.11) & 1.15 (.1) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\sigma}, (1, 4) & -.85 (.15) & 1.28 (.83) & .03 (.47) & .04 (.11) & .24 (.15) & .47 (.1) & 1.27 (.13) & 4.16 (.49) & 2.99 (.42) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\mu}, (0, 2) & 1.14 (.11) & .97 (.23) & 1.12 (.11) & .12 (.1) & -.07 (.24) & .15 (.09) & 1.02 (.07) & 1 (.18) & 1.02 (.07) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\rho}, (0.4, 0.6) & .37 (.16) & -.52 (.19) & -.11 (.2) & .15 (.14) & .24 (.14) & .48 (.08) & 1.03 (.1) & 1.08 (.11) & 1.14 (.09) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\sigma}, (1, 4) & .94 (.23) & -1.31 (.7) & -.05 (.46) & .08 (.1) & .23 (.15) & .47 (.1) & 1.77 (.16) & 3.66 (.47) & 2.97 (.37) \\ 
    \bottomrule
    \end{tabular*}
\end{table}

\begin{table}[t]
    \fontsize{12.0pt}{14.0pt}\selectfont
    \begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}ll|llllllllll}
    \toprule
    & & \multicolumn{3}{c}{$\hat{\mu}(.)$} & \multicolumn{3}{c}{$\hat{\rho}_1(.)$} & \multicolumn{3}{c}{$\hat{\sigma}(.)$} \\ 
    \cmidrule(lr){3-5} \cmidrule(lr){6-8} \cmidrule(lr){9-11}
    RGP & RN & s = 1 & s = 2 & $\perp$s & s = 1 & s = 2 & $\perp$s & s = 1 & s = 2 & $\perp$s \\ 
    \midrule\addlinespace[2.5pt]
    MS, symm. & \ensuremath{\mu}, (0, 2) & 1.97 (.73) & 1.94 (.71) & 1.95 (.58) & .52 (.28) & .55 (.28) & .77 (.05) & 1.54 (.33) & 1.49 (.31) & 1.58 (.24) \\ 
    MS, symm. & \ensuremath{\rho}, (0.4, 0.6) & -.02 (.26) & -.02 (.27) & -.01 (.21) & .36 (.23) & .14 (.27) & .47 (.1) & .54 (.17) & .53 (.17) & .55 (.15) \\ 
    MS, symm. & \ensuremath{\sigma}, (1, 4) & .04 (.89) & .02 (.64) & .02 (.54) & .35 (.27) & .04 (.33) & .46 (.11) & 1.65 (.76) & 1.68 (.62) & 1.54 (.52) \\ 
    SB, mid & \ensuremath{\mu}, (0, 2) & 3.69 (.65) & .09 (.74) & 2.19 (.2) & .73 (.1) & .46 (.15) & .96 (.01) & .89 (.2) & .52 (.21) & 2.05 (.19) \\ 
    SB, mid & \ensuremath{\rho}, (0.4, 0.6) & -.12 (.44) & .16 (.48) & 0 (.21) & .55 (.13) & .31 (.28) & .65 (.1) & .64 (.24) & .36 (.22) & .64 (.15) \\ 
    SB, mid & \ensuremath{\sigma}, (1, 4) & -.79 (1.22) & .92 (1.22) & .05 (.6) & .49 (.18) & .48 (.19) & .64 (.12) & 1.68 (.82) & 1.47 (.77) & 1.86 (.5) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\mu}, (0, 2) & 4.03 (.38) & 3.93 (.39) & 3.99 (.2) & .15 (.28) & .27 (.22) & .44 (.19) & .41 (.17) & .67 (.21) & .6 (.14) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\rho}, (0.4, 0.6) & .26 (.4) & .15 (.42) & .19 (.21) & .29 (.23) & .14 (.26) & .47 (.18) & .7 (.2) & .4 (.17) & .61 (.13) \\ 
    SET, \ensuremath{\tau} = 0 & \ensuremath{\sigma}, (1, 4) & .45 (1.07) & -.07 (1.12) & .02 (.51) & .24 (.21) & .1 (.29) & .44 (.18) & 1.8 (.72) & 1.16 (.66) & 1.6 (.45) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\mu}, (0, 2) & 18.65 (31.2) & 1 (.26) & 18.68 (39.19) & .3 (.28) & .19 (.3) & .35 (.17) & 65.36 (74.29) & .38 (2.06) & 65.31 (42.84) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\rho}, (0.4, 0.6) & .19 (.5) & -.42 (.48) & -.1 (.21) & .14 (.28) & .34 (.24) & .48 (.19) & .4 (.13) & .49 (.15) & .59 (.13) \\ 
    ST, \ensuremath{\tau} = 0 & \ensuremath{\sigma}, (1, 4) & -.15 (1.49) & 1.78 (5.89) & 2.61 (1.85) & .08 (.27) & .3 (.25) & .45 (.19) & 1.14 (.58) & 23.8 (3.17) & 21.9 (3.17) \\ 
    \bottomrule
    \end{tabular*}
\end{table}

\end{landscape}
