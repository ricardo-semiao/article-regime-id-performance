---
title: "Regimes' Characteristics and Time Series Forecasting"
subtitle: "FGV-EESP Masters' Thesis"
author: "Student: Ricardo Semião e Castro\nAdvisor: Prof. Marcelo Fernandes"
date: today

bibliography: ../references.bib

number-sections: true
fig-cap-location: top

format:
    pdf: 
        title-meta: "Thesis sketch"
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
                \makeatletter         
                \renewcommand\maketitle{
                    {\raggedright
                    \begin{center}
                    {\Large \bfseries \sffamily \@title }\\[4ex] 
                    { \@author}%\\[4ex] 
                    %\@date\\[8ex]
                    \end{center}}}
                \makeatother
                \setcounter{tocdepth}{2}
                \setcounter{secnumdepth}{3}
                \let\oldsection\section
                \renewcommand\section{\clearpage\oldsection}                
---

```{=tex}
\newcommand{\sgp}{\text{sgp}}
\newcommand{\rgp}{\text{rgp}}
\newcommand{\dgp}{\text{dgp}}
\renewcommand{\mod}{\text{mod}}

\renewenvironment{quote}
    {\list{}{\rightmargin\leftmargin}%
    \item\relax\color{red}}
    {\endlist}
```

> Dei uma grande reorganizada no texto seguindo as mudanças que falamos. Na minha visão tem duas coisas separadas: uma é framework do trabalho, seja a parte teórica de definir o DGP geral e o conceito de regime-conditional metrics, seja o framework das simulações; outra é a parte aplicada, seja os DGPs, modelos, e métricas específicas que eu vou considerar, seja os parâmetros e diagnósticos das simulações realizadas. Então organizei dessa forma, mas também daria para organizar como "Theoretica framework" -> "Considered DGPs, ..." -> "Simulation framework" -> "Simulation implementation".

> Mais comentários ...




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

The second focus is less orthodox and specific to RS models. These models are special in the sense that they not only identify the series in question but also its states -- its regimes -- thus allowing the econometrician to describe the distribution of each regime and how different they are from each other. This characterization of regimes' distributions might be informative for the model's performance: for example, if the DGP implies different intercepts across regimes, a model whose identified regimes have the same conditional average is probably not capturing that dynamic well; or some class of model can be good at capturing that dynamic but bad at capturing changes on the persistence. These examples might seem obvious, but I will show that there is many useful information to be taken from this kind of analysis.

The nature of this project is explorative. I will simulate a diverse set of DGPs and try to find stylized facts about how each RS model adjusts to them, and how the characteristics of the estimated regimes relate to this adjustment. To make things more concrete, in the remainder of this section I synthesize the methodology, describe the patterns I hope to find, and present some of the actual findings. Additionally, I briefly present the literature on RS models and how my work contributes to it.


## Basic methodology and hypothesis {#sec-intro-method}

The methodology follows a common setup. The first step is to establish a theoretical framework that describes all RS models in a unified way. Here, I denote the separate 'ingredients' in an RS DGP: the _series generating process_ (SGP) and the _regime generating process_ (RGP). By varying these 'ingredients', I define a diverse set of DGPs to be considered. Then, Monte Carlo simulations are used to generate series, each being fitted by all RS models. As many questions can arise from the broad motivation of this research, creating a very general and expandable setup and implementation is a goal in itself.

For the first part of the work, processing the Monte Carlo results starts with visualizing the generated series, understanding how each DGP 'works' and how RGP and SGP interact. With this in hand, the fit of the models can be visualized, checking which models captured the dynamics in which contexts. Then, more systematic regression analysis is done, explaining the performance of each estimated model by the DGP and model used, as well as interactions between the two, which can capture measures of mis-specification.

The second part builds on the theoretical framework. I hypothesize which characteristics of regimes can be relevant for model performance in each context, e.g., the conditional average for DGPs with intercept changes. Then, these distribution metrics are calculated for the Monte Carlo results. The processing follows the same steps as before, with initial visual diagnostics of whether the metrics indeed characterize the simulated series' regimes, and if the same pattern is found in the mis-specified models' results. Finally, regression analysis is done, now including the metrics as explanatory variables.

<!-- TODO: Aqui falar das perguntas mais concretas de pesquisa

Some of these possible relationships are direct and expected. In the $AR(1)$ example above, it is expected that an RS model that yields regimes with different metrics on _conditional averages_ will perform better than one that does not, while a metric of _conditional volatility_ should not carry much meaning. In addition to listing and testing these expected relationships, there are further questions to be answered: which metric for a given characteristic can the models best match with the true one? Which is a better predictor of performance? How do these relationships change across different regime generating processes? For a given model, does the performance within a regime change with its characteristics?

In parallel, there are more specific questions: How does the effect of mis-specifying the number of regimes change with the degree of difference between regimes' characteristics? How does the ability to identify regimes' characteristics change with the sample size across regimes?

- Parada de ter 2 focos: Poderiamos ir alem no de considerar DGPs e modelos complexos, estudar mais a sensibilidade a má especificação, mas vamos deixar o framework/código pronto para isso, mas dar foco no segundo foco, que é a parada das métricas. Justificação: tbm é util para usar as métricas para identificar regimes, pré-modelagem, de maneira mais agnóstica. Poderia ter exercício específico pra isso
-->


## Regime switching literature {#sec-intro-lit}

<!-- TODO: Com um outro nome, mas falar:

I start by familiarizing the reader with the literature of RS models and its seminal papers. Then, I present a review of the known factors that influence their performance, both to compare with my results and to contextualize the contribution of this work.

## Existing regime switching models

> Ainda não escrevi o texto final, mas é uma simples introdução de cada, mais sobre intuição e aplicações do que matemática. Para cada modelo: S-Breaks - teste de Chow e Bai-Perron; TAR/SETAR papers do Howell Tong; Markov Hamilton, inclusive HMM para business cycle; STAR - papers do Timo Teräsvirta.

Important papers: [@Chow1960], [@BaiPerron1998], [@Hamilton1989], [@Terasvirta1994], [@TongLim1980].

## Known factors influencing performance

> Ainda não escrevi o texto final, as conclusões vão na linha de: (i) muita análise em contextos econômicos, as simulações feitas aqui são úteis para poder isolar melhor as coisas; (ii) a análise das características dos regimes é pouco explorada, por mais que em parte por ser algo menos ortodoxo também. -->

The rest of this work is divided as follows: the general framework is presented in @sec-theory and @sec-sim, while the specific implementation chosen is presented in @sec-cons and @sec-impl. The exploratory analysis is done in @sec-exp, and the systematic analysis in @sec-exs. Finally, @sec-conclusion concludes. <!-- UPDATE -->



# Theoretical framework {#sec-theory}

In this section, I define the theoretical framework that guides the rest of this work. First, I define the general structure of RS DGPs, aligning all in a common mathematical representation, and relate the concepts of models and metrics to it. An important idea is the separation of the DGP into RGP and SGP.


## The general regime switching DGP {#sec-theory-dgp}

Let $y_t \in \mathbb{R}$ denote the series of interest at time $t \in 1:T$[^colon], $T \in \mathbb{N}$. Let $S \in \mathbb{N}$ denote the number of regimes. The _regime variable_ is a vector of 'weights' for each regime, indexed by $r^s_t$, $s \in 1:S$.

In this work, I consider only univariate series.

[^colon]: Let $a:b \coloneqq \{a, a+1, \dots, b\}$ for $a \leq b \in \mathbb{Z}$, and $y_{a:b} \coloneqq \{y_a, \dots, y_b\}$.

A DGP can be written in terms of a pair: _regime generating process_ (RGP) and _series generating process_ (SGP). These are functions with parameters $\Theta_r$ and $\Theta_y$, respectively, such that:

\begin{equation}
\begin{array}{rrlllll}
    r_t &= \rgp(&y_{1:(t-1)}, &r_{t-1}, &t &;~ \Theta_r &)\\
    y_t &= \sgp(&y_{1:(t-1)}, &r_t,     &t &;~ \Theta_y &)\\
        &= \sgp(&y_{1:(t-1)}, &\rgp(y_{1:(t-1)}, r_{t-1}, t; \Theta_r), &t &;~ \Theta_y &)
\end{array}
\end{equation}

Notably, the number of regimes $S$ is a parameter in $\Theta_r$, and $\Theta_y$ is actually a set of different parameters for each regime, each indexed by $\Theta^s_y$. This means that the SGP can be written as:

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

In the notation above I omitted the error term. For our purposes, it is more useful to write the DGP as a function that receives a sequence of random errors[^erros], and returns the series and the regimes:

[^erros]: This is a simplification, assuming the same error distribution across regimes.

\begin{equation}
    (y_{1:T},~ r_{1:T}) = \dgp(\varepsilon_{1:T};~ \Theta_r, \Theta_y)
\end{equation}

Consider the notation shorthand $y \coloneqq y_{1:T}$, and similarly for other variables, used for the rest of this work.

Let the set of considered DGPs be $P$ (for 'processes'). These are present in the literature, as discussed in @sec-intro-lit, and will be defined in @sec-cons.


## Models {#sec-theory-models}

Consider a model $\mod$ as a function with parameters $\Theta_m$ that generates the fitted values and $H$-step ahead predictions of the series and regimes. The model can also return a set $\hat{\pi}$ of general metadata, e.g. the estimated parameters.

\begin{equation}
    (\hat{y},~ \hat{r},~ \hat{\pi}) = \mod(y_{1:(T-H)} ~;~ \Theta_m)
\end{equation}

Notably, the number of estimated regimes $\hat{S}$ is a parameter in $\Theta_m$, which may or may not be equal to $S$.

Let the set of models be $M$ (for 'models'). Also present in the literature, they will be defined in @sec-cons-sgp.


## Metrics {#sec-theory-metrics}

A regime-conditional (RC) metric $c$ is function that receives a vector of series and a vector of regimes, and returns the value of the metric for each regime (a sequence).

\begin{equation}
    c: (y, r) \mapsto \mathbb{R}^{S}
\end{equation}

Many RC metrics simply map $(y, r)$ to the $S$ sets $R_s$ of regimes' observations[^regime_obs_set], then apply the relevant calculation to each set. For example, the RC mean returns the mean of each $R_s$. Others are more complex, as is the case of autocorrelation. It requires, for each regime, separating all of its instances. This will be further explained in @sec-cons-metrics-app.

[^regime_obs_set]: $R_s \coloneqq \{ y_t ~:~ r^s_t = \max\{r_t\} \}$.

In any case, RC metrics possibly lump together observations from different time windows. For them to be be meaningful, it is required that the series be stationary within each regime. This will impose restrictions on the DGPs that this work will consider.


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

As we intent to characterize the distributions with specific metrics, weaker assumptions can be made. If we restrict ourselves to the moments of the (joint) distribution, we can require the weak version. Formally, _within-regime weakly stationarity_ requires[^acf_stationarity], for all $s \in S$, that:

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

Processes that have a non-binary $r_t$, i.e. smooth transitions don't have truly separated regimes, and thus, generally don't satisfy the conditions above. The metrics can be calculated on the 'dominant'[^regime_obs_set] regime, but will not properly describe the varying distribution within each regime[^st_metrics]. Still, such information might be useful, as will be studied in this work.

[^st_metrics]: An alternative is to calculate regime-inconditional metrics, weighted by the regime variable, but this is not the focus of this work.


### Aspects of RC metrics usage

How the metrics will be used is going to be discussed later, but for now, it is important to note there are two aspects of their use. First is wether to use the whole sequence of values for each $s$, or to condensate it into a single value of dispersion across regimes. For example, the average pairwise distance between the RC means, a single value that describes how distant are the levels of the regimes. This is equivalent to composing an dispersion function $d$:

\begin{equation}
    d \circ c: (y, r) \mapsto \mathbb{R}^{S} \mapsto \mathbb{R}
\end{equation}

Second, which series to use, the true or estimated ones. One can use the true values $(y, r)$ and get the characteristics of the true DGP, or the estimated values $(\hat{y}, \hat{r})$ and get the characteristics of the estimated model[^dimension]. Another option is to calculate the difference between the former and the latter[^order]. Another option is to calculate the metric of the difference $(y - \hat{y}, r)$ or $(y - \hat{y}, \hat{r})$.

[^dimension]: Note that the value of $S$ and $\hat{S}$ can be different, and thus, so the dimension of the metric's output.

[^order]: This is only possible if $S = \hat{S}$ and there is an unambiguous way to match the estimated and true regimes.

This framework allows for mixing and matching these options, each being useful to answer different questions. In this work, I focus on the estimated series, as they are the only thing available to the econometrician in practice, and in using the dispersion of RC metrics, as it is more comparable across DGPs and models.

> Não sei se foi uma tangente muito grande falar dessas outras opções que não vou usar, ainda mais porque pode não estar claro pro leitor qual tipo de pergunta cada ajuda a responder. Eu gostaria de informar essa flexibilidade do framework, mas talvez valha mais colocar isso num apêndice.

Let the set of metrics $d \circ c$ be $C$ (for 'criteria'). These will be defined in @sec-cons-metrics, but are mostly based on the moments of $y_t$ and of the pair $(y_t, y_{t-j})$, $j \in \mathbb{N}$, and the performance metrics for the dependent variable.



# Simulation framework {#sec-sim}

One of the partial goals of this work was to create the theoretical framework described in the last section in a very general and expandable way, that easily allows for different exercises, even if they are not considered here. Similarly, the simulation structure was designed to follow the same concept.

There are the following steps to perform the simulations:

1. Generate random errors for all the DGPs.
2. For each DGP and simulation, generate ($y, r$).
3. For each DGP, simulation, and model, obtain $(\hat{y},~ \hat{r})$.
4. For each DGP, simulation, and model, compute each metric.
5. Aggregate the metrics, performance information, and DGP and model descriptors into a dataset.


## Forecast horizon

For the forecast performance, I focus on $1$-step ahead predictions. It would be interesting to expand that, be it with locally-projected models or not.

To obtain more than one prediction per simulation, I simulate a $T - H$-long series, and obtain $H$ predictions. There are two possible approaches:

1. For each iteration $h \in 1:H$, the model is estimated with the window $h:(T-H+h-1)$, and generates $\hat{y}_{T-H+h}$.
2. The model is estimated once with the window $1:(T-H)$, then for each $h$, $\hat{y}_{T-H+h}$ is generated using $y_{1:(T-H+h-1)}$.

The second approach is computationally cheaper, allowing for more simulations and DGPs to be considered. It is the one used in this work, but note that it is less accurate to what would be done in practice, as econometricians often re-estimate their models with new data.


## Simulation parameters

The parameters of the simulation are as follows:

- Number of simulations: $I$. Its main effect is on the the precision of the results, and diversity of series.
- Forecast horizon: $H$ predictions of $1$-step ahead values. Also affects the precision of the results, but does not change the diversity of series.
- Total number of observations: $T$. Its main effect is on the ability of the models to learn the dynamics and separate the regimes. Results for higher $T$'s are more relevant for contexts with a lot of data, such as high-frequency financial data, while lower $T$'s are more relevant for contexts with less data, such as macroeconomic data.
- Burn-in period: $B$. Its main effect is on reducing the dependence of the initial values, but with stationary processes, this is not too problematic.

Let $i \in 1:I$, $I \in \mathbb{N}$ be the simulation index.


## Simulating Series

I will only consider DGPs have the same error distribution -- but note that a DGP can have a volatility parameter multiplying its error. Thus, $I$ sets of random error vectors are created, each of size $1:T$. The errors were generated for each pair $(dgp, i)$. Let $\Epsilon$ denote the set of all errors.

For each $p \in 1:|P|$ and $i \in 1:I$, let $\Epsilon_{p, i}$ denote the vector of errors generated for the $p$-th DGP and the $i$-th simulation. Similar indexing definitions will be used for similar collections throughout this document.

Let $Y$ and $R$ denote the sets of generated series and regime variables. For each $p$ and $i$, their elements are denoted via the index notation $Y_{p, i}$ and $R_{p, i}$. They are computed given $\Epsilon_{p, s}$:

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


## Estimating Models

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


## Calculating Metrics

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

Recall the discussion in @sec-theory-metrics about the two different aspects of RC metrics usage. With different choices regarding the usage of true or estimated series, the function $C_c$ could recieve different inputs. Additionally, the function could return the whole sequence of RC metrics, not a single value, then, each row would be identified by $(p, i, m, s)$.

The dataset $D$ is already in a friendly format for analyzing the relationship between the performance of each observation and the characteristics of the regimes, as well of considering stratifications by DGP and model.



# Considered DGPs, models, and metrics {#sec-cons}

## Considered SGPs {#sec-cons-sgp}

The functional form of the SGP could be important in its interaction with the other ingredients of the DGP. Additionally, some topics are interested in specific SGPs, such as conditional volatility in finance and GARCH models. For now, however, this does not seem to be the main point of interest. I will consider only an $AR(1)$ process, for its simplicity, popularity, and ease of estimation.

Furthermore, considering only stationary processes is useful, as non-stationarity brings problems for calculating metrics across a series. Even though many interesting DGPs are non-stationary, this simplification will be adopted. Thus, this restricts the parameters to $|\rho_1| < 1$.

The only SGP functional form considered is the following:

\begin{equation}
\begin{array}{ll}
    &y_t(. ~;~ (\mu, \rho_1, \sigma)) = \mu + \rho_1 y_{t-1} + \sigma \cdot \varepsilon_t, ~~ \varepsilon_t \sim \mathcal{N}(0, 1)\\
    &|\rho_1| < 1, ~~ \sigma > 0
\end{array} \tag{SGP-AR(1)}
\end{equation}

Several others SGP's could be considered, such as ones with transformations of $y_t$ as regressors, non-linear regression forms, or even decision trees, as in the common model Markov-switching Random Forest. Still, the $AR(1)$ is an essencial building block, and its simplicity helps isolating the effects of the other ingredients.


## Considered RGPs and models {#sec-cons-rgp}

The next 'ingredient' is the RGP. I will consider the options Self-Exciting Threshold (SET), Smooth-Transition (ST), and Markov-Switching (MS). Structural Break (SB) is included to study how RS models perform in the case of breaks without reocurring regimes.

Each of these RGPs have empirical model counterparts, which are also considered. There is an additional model with an unsupervisioned approach where the regimes are defined by some clustering technique and each regimes' AR is estimated independently afterwards (Clustering + AR, CAR). Finally, a non-RS Random Forest (RF) model is included as a benchmark.

The formal definition of each RGP/model is presented in the @sec-cons-rgp-app, first the RGP hypothesis, then the empirical model estimation strategy.

For all RGPs, it is considered an option with equally likely regimes, and an assymetric variation.

- Structural Breaks:
    - A single break at $T / 2$, and a single break at $2T / 3$.
- Self Exciting Threshold:
    - Fixed parameters: switching based on $y_{t-1}$. Different lags are often specific to timing-related issues, and not considered here.
    - A single treshold at $0$, and a single threshold at $0.5$.
- Smooth Transition:
    - Fixed parameters: switching based on $y_{t-1}$, logistic's CDF as transition function.
    - A single treshold at $0$, and a single threshold at $0.5$.
- Markov Switching:
    - Symmetric matrix, high persistence ($P(s | s) = 0.9$), symmetric matrix, low persistence ($P(s|s) = 0.6$).
    - Asymmetric matrix, high persistence ($P(1 | 1) = 0.9$, $P(1 | 2) = 0.7$), asymmetric matrix, low persistence ($P(1 | 1) = 0.8$, $P(1 | 2) = 0.6$).

<!-- UPDATE -->

> Tem várias outras parametrizações ja feitas no código e com o texto escrito, mas deixei só essas aqui caso algo mude.

For the models, most hyperparameters are as follows:

- All the coefficients are assumed to change across regimes, as this is common assumption, especially in the face of possible mis-specification.
- The number of regimes $\hat{S}$ is fixed, not estimated. Models are estimated with 2 regimes. <!-- UPDATE -->
- The values of model-specific hyperparameters are the same as the related RGP's values.


## Considered regime natures {#sec-cons-rn}

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

> Idem, existem outras opções.

Note that the regimes are always ordered increasingly by the parameter of interest. In general, the large vs. small differences will be interesting to analyze in relation to each other. To compare different types of changes, only the large differences will be considered, for simplicity.



## Considered metrics {#sec-cons-metrics}

The goal with RC metrics is to capture the change in the series characteristics across regimes. One important option is the estimated parameters of the model for each regime, e.g. $(\hat{\rho}_s)_{s \in \hat{S}}$, $(\hat{\mu}_s)_{s \in \hat{S}}$, etc. One might think that this would outshine all other metrics, but in more complex cases where more than one parameter changes, this becomes less useful. More general metrics generate benefits from their abstraction over the DGP. Additionally, in simple SGPs, there often is a metric that is directly connected to changes in parameters, such as conditional average for changes in intercept.

In this work, I focus on the moments of the distribution of $y_t$ and $(y_t, y_{t-j})$. Specifically, the RC metrics considered are the RC mean, RC standard deviation, and RC autocorrelation of lag 1. Higher lags could be considered, but in the simple $AR(1)$ context this would bring little additional information.

> Talvez o 3ro e 4to momentos sejam interessantes, especialmente o 3ro porque alguns DGPs geram séries assimétricas. <!-- UPDATE: 3 and 4 moments if used -->

As stated before, the RC mean and RC SD are simply the mean and SD of each set $R_s$. The autocorrelation is similar, but must be calculated separately for each concurrent set of observations in $R_s$. The formal definitions are stated in the @sec-cons-metrics-app.

As the focus is on the dispersion of RC metrics, two important measures to consider are the standard deviation and the average pairwise absolute difference. For only two regimes, they are very similar and the absolute difference is more intuitive. All the metrics are composed such as all $d \circ c \in C$ return a single real value, and $d(x) = |x_1 - x_2|$.

<!-- UPDATE: update with the chosen dispersion measures, and if more regimes are used -->

There are some possible expansions on this work's metrics calculation. One is to use non-standard weights for the empirical moments, giving more importance to observations near the edges of regimes' instances. Another is to use a cluster separation measure, such as the silhouette score, instead of a simple absolute distance between the RC metrics. Finally, one can use distribution distance metrics, such as the Earth Mover's Distance, on the empirical distribution of each regime.

The list of considered metrics is as below:

- First moment: RC mean $\hat{\mu}(y | S)$.
- Second moment: RC standard deviation $\hat{\sigma}(y | S)$.
- First autocorrelation: RC 1st autocorrelation $\hat{\rho_1}(y | S)$.


### Performance and RGP metrics

The performance metrics considered are $R^2$ for fit performance, and RMSE and MAPE for forecasting performance.

Other metrics pertaining the RGP will be included as controls in the regression analysis, the average duration and number of instances of a regime, and transition probabilities.

<!-- TODO: falar mais -->



# Simulation implementation {#sec-impl}

## Implementation

The implementation of the simulations, as well as their analysis in the next sections, is done with the R programming language, and the code can be found in [this paper's repository](https://github.com/ricardo-semiao/article-regime-id-performance). The code is highly modular and fully reproducible, following the intent of setting up an expandable framework.

The error sequences were generated in parallel, using [TRNG](https://www.numbercrunch.de/trng/).


## Diagnostics {#sec-impl-diag}

The errors should be i.i.d. Gaussian with mean $0$ and should not present any pattern, especially across the parallelization structure. This is guaranteed by the TRNG library, and is checked in @sec-impl-error-app.

On top of visualizing the series, to further check for problems in the series generations, the regime-conditional and inconditional moments are estimated and tested against their true values. Additionally, the ANOVA test of equal moment on all regimes is done. The regime-conditional true values are calculated as the standard $AR(1)$ moments. There is only an analytical formula for the unconditional moments of the SB and MS RGP, calculated via iterated expectations.

Table **TODO** shows the results. Each group of lines corresponds to the moments of a DGP. The first two columns relate to the values conditional in regime 1 and 2, the third column gives the unconditional values. Each cell has the value of the moment, and in brackets the p-value of the null hypothesis that the moment is equal to its true value. The last column shows the ANOVA p-value.

> Os valores finais dos diagnósticos podem mudar. Coloco aqui uma tabela de placeholder.

```{=tex}
\input{../../outputs/simulations/table_sgps.tex}
```

<!-- TODO: Table -->

We can see that the results are generally consistent with the expectations. The table is also useful to better understand the difference between the DGPs. Some useful analysies to be done are: as expected, the first moment is often informative about regime natures with changes in intercept, and similarly for the other moments; the RGP and regime nature interact in a non-trivial way, e.g. the TAR regime with higher intercept generates an assymetric process, as its 'hard to escape' that regime; How each DGP differs in terms of separation between their regimes, but more on that in the next section.

To theck the models, it is expected that models with the same RGP assumption as the DGP return similar moments, so a similar analysis as above is done in the @sec-impl-mod-app. The results are generally consistent with the expectations.

As a final sanity check, the frequency of improbable events were annotated: TODO% of the datapoints generated were $10$ standard deviations away from the relevant mean; and TODO% of the predictions were $10$ standard deviations away from the true value.


## Regime separation {#sec-impl-sep}

There are several aspects in which each DGP's generated series differ, and how the hyperparameters contribute to that difference. I propose that one of the most interesting aspects to analyze is the degree of separation of regimes, in terms of the metrics. This could be seen in table TODO, but I argue a better visualization is to graph how the separation progresses across the sample windows $1:2$, $1:3$, $\dots$, $1:T$.

By graphing the latest time-point considered in the calculation of the metrics on the x-axis, and the value of the metric on the y-axis, we can see how the separation evolves across sample size. This is useful because the sample size is one of the most important factos for the models to learn how to separate the regimes.

> Item. Coloco aqui a análise antiga só para referência.

@fig-sim-m1 shows the metrics for the SB-AR(1) model, with a break at the middle. The left-hand side shows the metrics calculated with the data up to the time on the x-axis, yielding a 'rolling' metric that can be used to analyze convergence. The right-hand side shows the distribution of the metrics calculated with the full series, for all simulations. We can see how the second regime only has information starting from $T/2$. Each row represents a different regime nature, thus a different conditional metric (mean, ACF(1), and SD, respectively).

![SGP metrics of SB-AR(1) model](../../outputs/simulations/stats_sgp-r2_sbreak_mid.png){#fig-sim-m1}

@fig-sim-m2 shows a metric for the RGP itself, for the MS-AR(1). Specifically, the empirical (non-)transition probability for each regime.

![RGP metrics of MS-AR(1) model](../../outputs/simulations/stats_rpg-r2_markov_symm_high.png){#fig-sim-m2}

In a more systematic way, the table below shows the average and standard deviation of the metrics and DGPs, and the columns ANOVA present the p-values of the null hypothesis that the metrics do not vary across regimes. Note that the power of this test is directly related to the number $I$ of simulations.



# Exploratory Analysis {#sec-exp}

> Creio que explorar tudo que contei na motivação poderia ser sua própria seção. Gera alguns resultados 'fatos estilizados' sobre os DGPs e como os modelos interagem com cada, mas também gera insumos pra seção seguinte. Ainda assim, não está desenvolvida. Coloquei apenas alguns exemplos de gráficos e interpretações, pra ter uma ideia do que poderia ser feito.

<!-- Todo: initial text -->

Before doing the systematic analysis and focusing on the results that could generate practical recommendations, it is important to explore the data, learn how each DGP behaves, and how the models interact with them. This will yield facts relevant in their own right, but also motivate the modeling decisions for the systematic analysis.

A benefit of having only one parameter changing in the DGP at a time is that regimes can be ordered by it, even the estimated ones. As noted in TODO, regimes are always ordered increasingly by the parameter of interest, and the same is done for the estimated regimes in the figures of this section.


## Series

Processing the Monte Carlo results starts with visualizing the generated series, understanding how each DGP 'works' and how RGP and SGP interact.


### Values and Distribution

@fig-sim-v1 show the series for the MS-AR(1) model, with a symmetric high-persistence transition matrix. The left-hand side shows a single simulated series, while the right-hand side shows the distribution of all simulations. Each row represents a different regime nature, with only $\mu$ changing, only $\rho_1$ changing, and only $\sigma$ changing, respectively. The grey area is the burn-in period.

We can see how the high persistence of regimes is indeed present in the data, with long periods in each regime. The change in intercept and volatility are clear, while the higher $\rho_1$ conditions a more volatile regime, as the past errors have a bigger impact.

![Values of MS-AR(1) model](../../outputs/simulations/values-r2_markov_symm_high.png){#fig-sim-v1}

@fig-sim-v2 shows the series for the SET-AR(1) model, with a threshold at 0. Here, the interaction between RGP and the regime nature is evident: the higher $\mu$ makes the series stray away from the threshold and very likely stay in regime 2. The higher volatility and $\rho_1$, which happen only when the series is above $0$, also end up conditioning a higher level for the series in these regimes.

![Values of SET-AR(1) model](../../outputs/simulations/values-r2_threshold_x_0.png){#fig-sim-v2}

Many other observations could be made. For now, the most important information to note is how the RGP and regime nature interact, creating different, non-obvious patterns in the series.



## Models

The processing follows the same steps as before, with initial visual diagnostics of whether the metrics indeed characterize the simulated series' regimes, and if the same pattern is found in the mis-specified models' results.


### Residuals

The first question is about the model's fit. @fig-mod-v1 shows the residuals and their distribution for the MS-AR(1) model, estimated on top of an SB RGP. While figure @fig-mod-v1 indicates the estimated regimes in the colors, figure @fig-mod-v2 indicates whether the regime was correct or not.

![Residuals of MS-AR(1) estimating a SB-AR(1)](../../outputs/estimations/residuals-r2_markov_symm_high-r2_sbreak-a.png){#fig-mod-v1}

![Residuals of MS-AR(1) estimating a SB-AR(1)](../../outputs/estimations/residuals-r2_markov_symm_high-r2_sbreak-na.png){#fig-mod-v2}

It appears that the estimated regime relates more to the residual level than its actual correctness. The differences in average and volatility are expected, and no difference can be seen in the autocorrelation change.

Again, in a more systematic way, the table below shows the average and standard deviation of the residuals across estimated regimes.

```{=tex}
\input{../../outputs/estimations/table_residuals.tex}
```

> Sinto que os gráficos dos resíduos ajudam a resumir a coisa, em vez de mostrar os pares fit-real.


### Coefficients

The next step is to analyze if the models are able to capture the coefficients and their difference across regimes. Note that this might not be a necessary condition for a good approximation.

Figure @fig-mod-c1 follows the same model from before and shows the distribution of each estimated coefficient ($\mu$ and $\rho_1$), while the dotted lines give the true values. Now the rows are separated by the big-and-small changes in each regime nature.

![Coefficients of MS-AR(1) estimating a SB-AR(1)](../../outputs/estimations/coefs-r2_markov_symm_high-r2_sbreak.png){#fig-mod-c1}

It is important to note both how the coefficients that _should_ change do (or don't), and how the coefficients that _shouldn't_ change might be compensating for the mis-specification.

> Eventualmente gostaria de colocar uma terceira coluna com a volatilidade estimada. Vou ajeitar o eixo x também.


### Metrics

> Similar à seção de métricas dos DGPs, seria possível fazer análises similares para os modelos. Ver se as mesmas métricas seguem caracterizando os regimes ou não, etc.

<!-- ### Series Diagnostics

The first step is to plot some series, to visually check if they look as expected. We can plot várias DGPs para analisá-las em relação às outras. Analyzing all the plots is out of the scope of this document, but the full simulations are present in the repository, and the appendix contains the numeric diagnostics for all DGPs.

The @fig-diag-series-one shows a single simulation for the MS DGPs (columns) and the $\mu$ regime natures (rows). Between columns, we can indeed see that the left has a higher prevalence of regime 1, while the right is more balanced. Between rows, we can see how the level of the series changes more drastically in the bottom one. The grey area is the burn-in period.

This could have been by chance, so in @fig-diag-series-mult I present 7 random, overlapping, simulations. While harder to read, we can still see the same patterns.

![Series - Single example](../figures/diag_series_one.png){#fig-diag-series-one height=45%}

![Series - Multiple examples](../figures/diag_series_one.png){#fig-diag-series-mult height=45%}

A different way to analyze basically the same information is @fig-diag-series-paths, a traceplot of $r \times y$, which shows the 'path' of the joint distribution of $y$ and $r$. Jitter was added to $r$ for visualization purposes.

![Series - Single example's path](../figures/diag_series_paths.png){#fig-diag-series-paths height=45%}

Finally, we can focus attention on the regimes themselves. The @fig-diag-regimes shows the path of regimes across time.

![Series - Single example's regimes](../figures/diag_regimes.png){#fig-diag-regimes height=45%}


### Metrics Diagnostics

On top of the general structure of the series, we can analyze specific metrics. This is useful as it is more systematic, and can be presented as a table for all DGPs, to complement the graphs. To also get information about convergence, we can compute the 'rolling' metrics with fixed initial point at $t = 1$.

> Essa noção de convergência é para uma dada simulação, convergência no tempo. Também deveria pensar em convergência ao longo de $I$, e/ou ao longo de $T \times I$.

For the regimes, some options are the average duration of a regime, the number of observations of a regime, and the transition probabilities (for $S = 2$). View these in @fig-diag-stat-transmat and @fig-diag-stat-nobs. We can see, for example, the assymetry on the first column.

![Metrics - Regime's transition probabilities](../figures/diag_stat_transmat.png){#fig-diag-stat-transmat height=45%}

![Metrics - Regime's number of observations](../figures/diag_stat_nobs.png){#fig-diag-stat-nobs height=45%}

For the series, in this case of $\mu$ changing, we can compute the conditional mean, and standard deviation (as a placebo test). The results are in @fig-diag-stat-mean, and indeed show differences in mean, bigger in the bottom row, and no real differences in volatility.

![Metrics - Regime's $\mu$ change](../figures/diag_stat_mean.png){#fig-diag-stat-mean height=45%} -->



# Systematic Analysis {#sec-exs}

> Eu já rodei uma versão inicial dessas regressões, só não coloquei as tabelas aqui, dado que ainda precisamos filtrar o que vai ser realmente mantido. Similarmente, também criei scatterplots para as regressões. As interpretações também são bem iniciais.
>
> As regressões abaixo podem ser rodadas a nível de regime também ($p, i, m, s$). A interpretação só muda para "em qual regime o modelo costuma errar mais".

<!-- Todo: initial text -->

## Stylized Facts about DGPs

In the first exercise, I run regressions with fixed effects for DGP and model, separate the model effects into RGP, SGP, and regime nature effects, and analyze these effects. Also, I study the sensitivity to mis-specification, via an indicator variable $\mathbb{1}(p = m)$ or with a full interaction $m \times p$.

An important placebo test is to check if the simulation index $i$ has no effect, via the regression below:

\begin{equation}
    rmse_{p, i, m} = \beta_0 + \beta_1 i + \varepsilon_{p, i, m}
\end{equation}

We in fact find no effect, as expected.

Next, we turn to the DGPs. Consider the categorical variables $\rgp_{p, i}$, i.e., vector of dummies, that indicates which RGP was used, and similar definitions for the RGP and the models. The regression below analyzes the fixed effects of RGP, SGP, and interactions between them, as was shown to be relevant in the exploratory analysis.

\begin{equation}
    rmse_{p, i, m} = \beta_0 + \beta_1 \rgp_{p, i} + \beta_2 \sgp_{p, i} + \beta_3 \rgp_{p, i} \cdot \sgp_{p, i} + \varepsilon_{p, i, m}
\end{equation}

Compared to the omitted group of $\mu$ change and Markov RGP, only the volatility change has higher RMSE. But all the interactions, except volatility with threshold, have positive coefficients.


## Stylized facts about models

Following the same logic, we can analyze the fixed effects of the models. To capture their sensitivity to mis-specification, we can add interactions between it and the DGP. As all the parameters change in all models, the most important interaction is between the model and the RGP.

Then, the first idea might be to add an indicator of correct specification, but this loses information about the type of mis-specification. Thus, I will also use the full interaction between model and RGP. These are the regressions below.

\begin{multline}
    rmse_{p, i, m} = \beta_0 + \beta_1 \rgp_{p, i, m} + \beta_2 \sgp_{p, i} + \beta_3 \rgp_{p, i} \cdot \sgp_{p, i} + \beta_4 \mod_{p, i, m}\\ + \beta_5 \mathbb{1}(\mod_{p, i, m} = \rgp_{p, i}) + \varepsilon_{p, i, m}
\end{multline}

\begin{multline}
    rmse_{p, i, m} = \beta_0 + \beta_1 \rgp_{p, i, m} + \beta_2 \sgp_{p, i} + \beta_3 \rgp_{p, i} \cdot \sgp_{p, i} + \beta_4 \mod_{p, i, m}\\ + \beta_5 \mod_{p, i, m} \cdot \rgp_{p, i} + \varepsilon_{p, i, m}
\end{multline}

One of the most significant results was a particularly bad interaction between a smooth transition model estimating a structural break RGP.


## Coefficients and performance

The regression above can be expanded to include the dispersion of the estimated coefficients across regimes. This is a similar exercise as the next one, as the coefficients can also be seen as regime-conditional metrics themselves.


## Regimes characteristics and performance

As motivated in the introduction, the characteristics of the regimes, especially the differences between them, could be related to model performance. To analyze this, I propose adding the metrics values as an additional regressor, and an interaction between it and the model. As each metric is specific to each SGP, the regressions will be run separately for each regime nature.

\begin{multline}
    rmse_{p, i, m} = \beta_0 + \beta_1 \rgp_{p, i, m} + \beta_2 \sgp_{p, i} + \beta_3 \rgp_{p, i} \cdot \sgp_{p, i} + \beta_4 \mod_{p, i, m}\\ + \beta_5 \mod_{p, i, m} \cdot \rgp_{p, i} + \beta_6 c_{p, i, m} + \varepsilon_{p, i, m}
\end{multline}

In general, the true vs. estimated difference of the conditional metric's dispersion has a positive relation with RMSE.


## Other exercises

The exploratory analysis obtained some 'first step' results, i.e., results on whether the metrics indeed characterize the regimes, and if the models are able to capture that. These could be formalized via regressions too.

The regime variable can be further studied. Its identification performance can be treated as a dependent variable, or also as a control.

An important hyperparameter of the models is the number of regimes $\hat{S}$. To study the sensitivity to mis-specification of this parameter, I can create an indicator variable for $\hat{S} < S$, and check for interactions between it and regime characteristics. The idea is that if the dispersion across regimes is low, mis-specifying the number of regimes should not be as harmful.

If there is time, test if the practical recommendations help in a real-world example, and if the patterns found in the simulations are observed in real data.



# Conclusion {#sec-conclusion}

Here I start by summarizing the motivation and methodology.

Then, I focus on the main results. First, with the more descriptive findings about properties of the models, then, the practical recommendations of metrics an econometrician should look at when choosing a model.



# References {.unnumbered .unlisted}

AI disclaimer: this work was generated generally without the help of large language models, the only relevant exception being code autocompletion during the coding implementation phase.

> É necessário colocar algo assim? Talvez como nota de rodapé na primeira página?

::: {#refs}
:::



{{< pagebreak >}}

```{=tex}
\appendix
\addcontentsline{toc}{section}{Apêndices}
\renewcommand{\thesubsection}{\Alph{section}.\arabic{subsection}}
```

# Considered DGPs, models, and metrics {#sec-cons-app}

## RGPs and models {#sec-cons-rgp-app}

### Structural Break (SB)

**Hypothesis:** Regime changes at specific time points $\tau \in (1:T)^{S-1}$. Formally:

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ \tau) = \mathbb{1}(\tau'_{s-1} < t \leq \tau'_s), ~~ \forall s \in \{1, \dots S\}\\
    &\tau \in \mathbb{N}^{S-1}, ~~ \tau_{s} > \tau_{s-1} ~\forall s, ~~ \tau' = (0, \tau, T)\\
\end{array}\tag{RGP-SB}
\end{equation}

**Empirical model:** Given $\tau$, the model estimates $\mu$ and $\rho_1$ via OLS in each regime. $\tau$ is chosen by minimizing the sum of squared residuals over a grid search of breakpoints.


### Self-Exciting Threshold (SET)

**Hypothesis:** Regime changes when the series, possibly at a lag $d \in \mathbb{N}^*$, crosses specific threshold values $\tau \in \mathbb{R}^{S-1}$. Transformations of the variable can be considered[^g_abs]. Formally:

[^g_abs]: For example, $g(x) = |x|$ or $g(x) = \Delta x$.

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ (\tau, d, g)) = \mathbb{1}(\tau'_{s-1} < g(y)_{t-d} \leq \tau'_s), ~~ \forall s \in \{1, \dots S\}\\
    &\tau \in \mathbb{R}^{S-1}, ~~ \tau_{s} > \tau_{s-1} ~\forall s, ~~ \tau' = (-\infty, \tau, \infty), ~~ d \in \mathbb{N}^*
\end{array}\tag{RGP-SET}
\end{equation}

**Empirical model:** Given $\tau$ and $d$, the model estimates $\mu$ and $\rho_1$ via OLS in each regime. $\tau$ and $d$ are chosen by minimizing the sum of squared residuals over a grid search of breakpoints and lags. One can also leave $d$ fixed.


### Smooth Transition (ST)

**Hypothesis:** Regime changes smoothly, with a continuous function $g$, often a CDF, based on the difference between the series and the threshold $\tau \in \mathbb{R}$, possibly at a lag $d \in \mathbb{N}^*$. [@Medeiros2000] has shown that a generalization to $S$ regimes is a neural network, but currently, I only consider $S = 2$. Formally:

\begin{equation}
\begin{array}{ll}
    &r^1_t(. ~;~ (\tau, d, g)) = g(y_{t - d} - \tau), ~~~ r^2_t(. ~;~ (\tau, d, g)) = 1 - r^1_t(. ~;~ (\tau, d, g))\\
    &\tau \in \mathbb{R}, ~~ d \in \mathbb{N}^*
\end{array}\tag{RGP-ST}
\end{equation}

Often, the function $g$ depends on a smoothness parameter $\gamma$, i.e., when $\gamma \to \infty$, $g \to \mathbb{1}$. This parameter can be jointly estimated with the others.

**Empirical model:** Estimated via non-linear squares of the residuals, over $\mu$, $\rho_1$ (for each regime), $\tau$, and $\gamma$. Uses some numerical optimization, which depends on starting values and does not guarantee a global optimum.


### Markov-Switching (MS)

**Hypothesis:** Regime changes stochastically, following a Markov process with transition matrix $\Gamma \in [0, 1]^{S \times S}$. The probability of being in regime $s$ at time $t$ depends only on the regime at time $t-1$, often with $\Gamma$ implying some persistence. Formally:

\begin{equation}
\begin{array}{ll}
    &r^s_t(. ~;~ \Gamma) \sim P(r^s_t = 1 | r_{t-1}) \eqqcolon \Gamma_{s, r_{t-1}}\\
    &\Gamma \in [0, 1]^{S \times S}, ~~ \sum_{i=1}^S \Gamma_{s, i} = 1 ~\forall s\\
\end{array}\tag{RGP-MS}
\end{equation}

**Empirical model:** The MSAR DGP can be written in terms of a state-space model, which can then be related to filtering and smoothing techniques. The EM algorithm uses Kalman to find smoothed probabilities of $r$, then the conditional probabilities given the current guess of parameters, then the guess of parameters is updated via maximizing the likelihood given the probabilities. These two steps are iterated until convergence.


### Clustering + AR (CAR)

**Hypothesis:** no hypothesis on the RGP.

**Model:** Unsupervised clustering techniques, such as K-Means, can be used to estimate the regimes based on $y_t$, its lags, and rolling moments. Given the regimes, $\mu$ and $\rho_1$ are estimated via OLS. This hybrid approach yields non-standard asymptotic properties.

**Considered parametrizations:** Basic K-Means will be used.


### Random forests (RF)

**Hypothesis:** there is no RS, the non-linearity is captured by the tree and ensamble structure of the RF.

**Model:** a RF is estimated based on $y_t$, its lags, and rolling moments.

<!-- TODO: cite aquele lá de hybrid approach, e um de RF pra TS -->

<!-- **Hypothesis:** Can assume either a SET or an MS RGP.

**Model:** In the case of SET, the first node is slit in $\hat{S}$ nodes based on $y_{t-1}$, with each branch corresponding to a regime, then, the rest of the algorithm is the same as a standard RF. In the case of MS, the algorith is similar to the MS-AR, but with each regime having its own tree, following [this paper](https://www.econstor.eu/bitstream/10419/315185/1/10182_2024_Article_501.pdf). Both algorithms can be done in an ensamble fashion, generating random forests. -->


## Metrics {#sec-cons-metrics-app}

Given the weakly stationary within regimes assumption, the regime-conditional moments are independent of the RGP, and are the simple $AR(1)$ moments:

\begin{align*}
    \mu(y_t | s) &\coloneqq E[y_t | y_t \in R_s] = \mu^s\\
    \sigma(y_t | s) &\coloneqq Var[y_t | y_t \in R_s] = \sqrt{\frac{\sigma}{1 - (\rho^s_1)}}\\
    \rho_j(y_t | s) &\coloneqq Cov[y_t, y_{t-1} | y_t \in R_s] = (\rho^s_1)^j, ~~ j \in \mathbb{N}^*
\end{align*}

The estimated conditional mean and standard deviation can be calculated as, respectively:

\begin{align*}
    \hat{\mu}(y | s) & \coloneqq \frac{1}{|R_s|} \sum_{y \in R_s} y\\
    \hat{\sigma}(y | s) & \coloneqq \sqrt{\frac{1}{|R_s|} \sum_{y \in R_s} (y - \hat{\mu}(y | s))^2}
\end{align*}

For moments of the joint distribution of $y_{t}$ and $y_{t-j}$, we must consider only concurrent windows contained within the given regimes. Let $W_s$ be $R_s$ but separated into concurrent instances:

\begin{equation*}
    W_s \coloneqq \{ (t_{\text{start}}, t_{\text{end}}) ~:~ (t = 0 \vee r_{t_{\text{start}} - 1} \neq s) ~\wedge~ (t = T \vee r_{t_{\text{end}} + 1} \neq s) ~\wedge~ (t \in R_s) \}
\end{equation*}

Then, the estimated autocorrelation of lag $j$ can be calculated as:

$$\begin{equation*}
    \hat{\rho}_j(y | s) = \frac{\sum_{(t_{\text{start}}, t_{\text{end}}) \in W_s} \sum_{t = t_{\text{start}} + j}^{t_{\text{end}}} (y_t - \hat{\mu}_s)(y_{t-j} - \hat{\mu}_s)}{\sum_{(t_{\text{start}}, t_{\text{end}}) \in W_s} \sum_{t = t_{\text{start}} + j}^{t_{\text{end}}} (y_{t-j} - \hat{\mu}_s)^2}, \quad j \in \mathbb{N}^*
\end{equation*}$$

Also consider the following notation:

\begin{equation*}
    \mu(y_t | S) \coloneqq \left(\mu(y_t | S)\right)_{s \in 1:S}
\end{equation*}


### Metrics for smooth transition models




# Diagnostics {#sec-impl-app}

## Random errors {#sec-impl-error-app}

The @fig-diag-errors-dependence shows the correlation of the errors across the parallelization structure. A simple visual check shows no evident patterns and an overall low correlation, as expected.

![Errors - Correlation across parallelization structure](../../outputs/errors/dependence.png){#fig-diag-errors-dependence height=45%}

The @fig-diag-errors-distribution shows the distribution of a size 3000 sample of the errors, via the usual histogram and QQ-plot. The distribution is very close to normal, as expected.

![Errors - Distribution](../../outputs/errors/distribution.png){#fig-diag-errors-distribution height=45%}


## Series generation and model estimation {#sec-impl-mod-app}

Table TODO is similar as table TODO, but shows the estimated moments of the model that matched its line's RGP assumption. The values are the average across simulations of the moments calculated with the estimated parameters. Note that the moments don't need to be the exactly the same, since all the models allow for all the parameters to change, a different assumption than of the regime natures.

<!-- TODO: tables -->
