---
title: "Regimes' Characteristics and Time Series Forecasting"
subtitle: "FGV-EESP Masters' Thesis"
author:
    - name: |
        Ricardo Semião e Castro \
        Orientador: Marcelo Fernandes
      email: ricardo.semiao@outlook.com
      url: ricardo-semiao.github.io
      affiliation:
        - name: Economics Masters' student at FGV-EESP
date: today
date-format: "D MMM YYYY"
keywords:
  - Time series
  - Regime Switching

bibliography: ../references.bib
csl: ../abnt.csl

fig-cap-location: top
tab-cap-location: top

format:
    beamer:
        title-meta: Semião 2026
        subject: Master's Thesis in Economics at FGV-EESP
        author-meta: Ricardo Semião e Castro
        date-meta: today
        keep-tex: true
        theme: Berlin
        outerthemeoptions: [subsection=false]
        include-in-header:
            text: |
                \input{../main/configs/rspalette.tex}

                \usepackage{amsmath}
                \usepackage{mathtools}
                \usepackage{float}
                \usepackage{multirow}
                \usepackage{multicol}

                \usepackage{tikz}
                \usetikzlibrary{positioning}
                \usetikzlibrary{decorations.pathreplacing}
                \usepackage{algorithm}
                \usepackage{algpseudocode}

                %\usepackage{caption}
                \captionsetup[table]{name=Tabela}
                \captionsetup[figure]{name=Figura}
                \captionsetup[table]{skip=0pt, belowskip=0pt}
                \captionsetup[figure]{skip=0pt, belowskip=0pt}
                \setlength{\belowcaptionskip}{1pt plus 1pt minus 3pt}

                \newcommand{\sgp}{\text{sgp}}
                \newcommand{\rgp}{\text{rgp}}
                \newcommand{\dgp}{\text{dgp}}
                \renewcommand{\mod}{\text{mod}}

                \setbeamertemplate{footline}{}
---


# Introdução

## Objetivo

Pergunta de pesquisa:

- Como modelos de RS performam em diferentes DGPs?
- Como essa performance se relaciona com características das distribuições dos regimes?

. . .

Mudança de regime (RS): comportamento que alterna entre dois ou mais padrões; DGP alterna entre conjuntos de parâmetros.

. . .

Comportamento observável nos dados, e útil para modelar não-linearidades, outliers, etc.


## Exemplos de mudanças de regime

![Regimes monetários do BC](../../personal/tests/selic_reservas.png){#fig-selic_reservas height=60%}

\small Lima et al. (2007) "Monetary policy regimes in Brazil" ([link](http://repositorio.ipea.gov.br/handle/11058/1884)). \normalsize


## Exemplos de mudanças de regime

![Produção de energia no noroeste dos EUA](../../personal/tests/ws_forecast.png){#fig-ws_forecast height=60%}

\small Fonte: Fonte: Gneiting et al. (2012) "Calibrated Probabilistic Forecasting at the Stateline Wind Energy Center" ([link](https://doi.org/10.1198/016214506000000456)). \normalsize


## Características dos regimes

Estudar a performance desses modelos é importante, e as características (métricas) dos regimes são relevantes.

. . .

![Produção de energia no noroeste dos EUA - Distribuições](../../personal/tests/ws_boxplots.png){#fig-ws_boxplots height=40%}

\small Fonte: Gneiting et al. (2012). \normalsize


## Resumo da metodologia

- Definir a estrutura dos DGPs, modelos, e métricas dos regimes.
- Gerar séries, previsões, e métricas via simulação de Monte Carlo.
- Estudar os DGPs e performance dos modelos.


## Sumário

```{=tex}
\tableofcontents
```


## Literatura relacionada

Literaturas similares:

- Modelos de estado-espaço.
- Modelos de quebras estruturais.

. . .

Literatura de RS:

- Quebras abruptas vs. suaves.
- Processos determinísticos vs. estocásticos.
- _Markov Switching_, _Self-Exciting Threshold_, _Smooth Transition_.
- Modelos 'agnósticos': K-means e Random Forest.



# Metodologia

## Processos de mudança de regime

::: {#fig-tik fig-pos="H"}
```{=tex}
\begin{tikzpicture}[font=\sffamily]
% Styles:
\tikzset{mybrace/.style={decorate, decoration={brace, amplitude=10pt, raise=1.3ex}}}
\tikzset{node distance = 0.25cm and 0.1cm}

% Main nodes:
\node[] (dgp) {DGP};
\node[] (e)   [right = 0.25cm of dgp] {$=$};
\node[] (sgp) [right = 0.25cm of e] {SGP};
\node[] (p)   [right = 1.95cm of sgp] {\&};
\node[] (rgp) [right = 1.95cm of p] {RGP};

% Lower nodes:
\node[] (p2)   [below = 0.5cm of sgp] {\&};
\node[] (csgp) [left  = of p2] {functional form};
\node[] (rsch) [right = of p2] {regime nature};

% Math:
\node[] (msgp)  [above = of sgp] {$y_t = \sgp(y_{1:(t-1)}, r_{1:t} ~;~ \Theta_y^s) \cdot r_t^s$};
\node[] (mrgp)  [above = of rgp] {$r_t = \rgp(y_{1:(t-1)}, r_{1:(t-1)} ~;~ \Theta_r)$};
\node[] (mcsgp) [below = 0.35cm of csgp] {$f_{\sgp}$};
\node[] (mrsch) [below = of rsch] {$\{\Theta^1_y, \dots, \Theta^s_y\}$};

% Braces using the defined style, add mirror in place:
\draw[mybrace]         (csgp.west) -- (rsch.east);
\draw[mybrace, decoration={mirror}] (mrgp.west) -- (mrgp.east);
\draw[mybrace, decoration={mirror}] (msgp.west) -- (msgp.east);
\draw[mybrace, decoration={amplitude=8pt}]         (mcsgp.west) -- (mcsgp.east);
\draw[mybrace]         (mrsch.west) -- (mrsch.east);
\end{tikzpicture}
```
A estrutura de um processo de RS
:::

. . .

Exemplos:

- RGP: regime 1 se a série passada foi alta, 2 c.c.
- SGP: $AR(1)$ com interceptos diferentes em cada regime.


## SGPs consideradas

**Forma funcional:** $AR(1)$, estacionário, com erros Gaussianos.

. . .

**Naturezas dos regimes:**

- Mudança em $\mu$: ($\mu^1 = 0$, $\mu^2 = 0.5$) e ($\mu^1 = 0$, $\mu^2 = 1$).
- Mudança em $\rho$: ($\rho_1^1 = 0.4$, $\rho_1^2 = 0.6$) e ($\rho_1^1 = 0.2$, $\rho_1^2 = 0.8$).
- Mudança em $\sigma$: ($\sigma^1 = 1$, $\sigma^2 = 1.5$) e ($\sigma^1 = 1$, $\sigma^2 = 2$).


## RGPs e modelos considerados

**RGPs:**

- Sem RS: sempre no regime 1.
- SET: determinístico, baseado em $y_{t-1}$, transição abrupta.
- ST: determinístico, baseado em $y_{t-1}$, transição suave.
- MS: estocástico, transição abrupta.
- Todos com $S = 2$ regimes, uma versão simétrica ($\approx$ 50-50), e outra assimétrica ($\approx$ 75-25).

. . .

**Modelos:**

- Recebem $y$, geram parâmetros, $\hat{r}$ e $\hat{y}$.
- MS, SET, e ST.
- K-means não supervisionado e Random Forest. Com 4 lags de $y$, média, desvio-padrão, e $ACF(1)$ móveis.
- Todos os parâmetros mudam; assume-se 2 regimes.


## Métricas dos regimes

Métrica condicional nos regimes: $(y, r) \mapsto \mathbb{R}^{S}$.

. . .

**Métricas consideradas:**

- Média, desvio-padrão, e $ACF(1)$.
- Condensadas pela distância média: $(y, r) \mapsto \mathbb{R}^{S} \mapsto \mathbb{R}$.
- Outras métricas: RMSE, $R^{2}$, e ajuste do regime.

. . .

Questões técnicas:

- Calculadas analiticamente, via $(y, r)$, ou $(y, \hat{r})$.
- Podem requerer estacionariedade dentro de cada regime.


## Simulação de Monte Carlo

Estrutura:

- Para cada DGP e simulação:
    - Gerar erros aleatórios.
    - Gerar $(y, r)$ via $r_1 \to y_1 \to r_2 \to y_2 \to \dots$.
    - Para cada modelo:
        - Obter $\hat{y}$ e $\hat{r}$.
        - Calcular as métricas, gerando um dataset.

. . .

Hiperparâmetros:

- 6 RNs, 7 RGPs e 5 modelos.
- Número de simulações: 500.
- Tamanho da série: 100, +4 descartes e +10 previsões.
- Previsões com janela fixa: $E_{1:t}[y_{t+h} | y_{1:(t + h - 1)}]$.

Implementação em R ([link](https://github.com/ricardo-semiao/article-regime-id-performance)), paralelizado, reproduzível e expansível.


## Diagnósticos

::: {#tbl-estimation_issues tbl-pos="!htbp"}
```{=tex}
\vspace{-0.4cm}
\resizebox{\textwidth}{!}{%
    \input{../../outputs/diagnostics/estimation_issues.tex}%
}
```
Problemas de estimação
:::

Outros: aleatoriedade dos erros; comparação verdadeiro vs. estimado de parâmetros e métricas; independência do índice de simulação.


## Proporções dos regimes

![Proporções dos regimes](../../outputs/diagnostics/regimes_est.pdf){#fig-regimes_est height=75%}



# Distribuições dos regimes

## Visão geral

Estudar, sobre cada DGP:

- Como são as distribuições dos regimes?
- Elas são diferentes em termos de cada métrica?
- Como a intensidade dessas diferenças se relacionam com $T$?
- Os modelos conseguem capturar essas diferenças?


## Diferenças entre os regimes

::: {#tbl-metrics_sep_t tbl-pos="!htbp"}
```{=tex}
\resizebox{\textwidth}{!}{%
    \input{../../outputs/exploratory/metrics_sep_t.tex}%
}
```
Separação dos regimes em cada DGP
:::

<!--
- MS: mudança em $\mu$ $\to$ separação na média; mudança em $\rho$ $\to$ separação em SD e $ACF(1)$; mudança em $\sigma$ $\to$ separação em SD.
- Mudança grandes $\to$ separação mais intensa.
- SET e ST: 'perfis' mais complexos.
- Estabilização ao redor de $T = 60$, RGPs assimétricos convergem mais devagar.
-->


## Diferenças entre os regimes

![Separação dos regimes variando $T$ - MS](../../outputs/exploratory/metrics_sep_ms.pdf){#fig-rs-ms height=75%}


## Separação estimada

![Diferença verdadeira vs. estimada](../../outputs/exploratory/metrics_diff.pdf){#fig-metrics_diff fig-pos="!htbp" height=75%}

<!--
- KM and ST do well with the mean when $\mu$ is changing, but not otherwise, while SET has a more balanced result, and MS performs worst.
- The distribution for any given metric varies widely across RNs: depending on the unobservable RN, the correctness of the estimated metrics varies.
-->


# Análise de performance

## Efeitos fixos dos modelos

<!-- 
- All regressions use RMSE as the dependent variable, so higher coefficient values imply worse performance associated with the given variable.
- The metrics and parameters are normalized as $|x - \text{median}(x)| / \text{mad}(x)$, except for the RMSE.
- Some metrics are not available for all observations, such as the ACF, which requires at least one length-2 instance of each regime in the series. Thus, the number of observations in each regression can vary.
-->

::: {#tbl-fe_strat tbl-pos="!htbp"}
```{=tex}
\vspace{-0.7cm}
\resizebox{\textwidth}{!}{%
    \input{../../outputs/systematic/fe_strat.tex}%
}
```
Efeitos fixos dos modelo
:::


## Má-especificação e performance

Má especificação da família do RGP diminui a performance:

- Efeito base de $0.515$ ($0.013$).
- MS tem efeito $\approx 1.3$ vezes maior que SET e ST
- Quando o RGP é sem-RS, o efeito é de $3.531$ ($0.014$).
- RN de $\sigma$ tem efeito $\approx 1.9$ vezes maior do que de $\mu$ ou $\rho$.
- Mudança grande tem efeito $\approx 1.4$ vezes maior que uma pequena.
- Nenhum par modelo-RGP tem efeito significativamente diferente de outro, fora com o RGP sem-RS

<!--
- SET e KM gostam, ST detesta, MS tudo igual
- Inclusive olhando pro RN
-->


## Modelos e características dos regimes

::: {#tbl-mis_metrics_sim tbl-pos="!htbp"}
```{=tex}
\vspace{-0.5cm}
\resizebox{\textwidth}{!}{%
    \input{../../outputs/systematic/mis_metrics_sim.tex}%
}
```
Modelos e 'perfis' de métricas condicionais
:::

Com as métricas estimadas, os resultados mudam, e variam entre RNs.


## Identificação e performance

::: {#tbl-match_r2 tbl-pos="!htbp"}
```{=tex}
\vspace{-0.4cm}
\resizebox{0.8\textwidth}{!}{%
    \input{../../outputs/systematic/match_r2.tex}%
}
```
RMSE e identificação do fit e $r$
:::


## Identificação e performance

::: {#tbl-match_metrics tbl-pos="!htbp"}
```{=tex}
\vspace{-0.4cm}
\resizebox{\textwidth}{!}{%
    \input{../../outputs/systematic/match_metrics.tex}%
}
```
RMSE e identificação das métricas condicionais
:::

<!-- 
- A $\Delta$ symbol represents the absolute difference between the estimated value (often with $y_{1:(T - H)}, \hat{r}_{1:(T - H)}$) and the analytical true value.
-->


## Má-especificação do número de regimes

::: {#tbl-regimes tbl-pos="!htbp"}
```{=tex}
\vspace{-0.3cm}
\resizebox{0.8\textwidth}{!}{%
    \input{../../outputs/systematic/regimes.tex}%
}
```
Má especificação do número de regimes
:::

<!-- KM gosta, ST detesta, MS e SET são iguais -->



# Conclusão

## Conclusão

- Objetivo: estudar os processos e modelos de RS, através das características das distribuições de seus regimes.

- Defini uma estrutura teórica e de Simulações de Monte Carlo, geral e expansível, mas foquei em objetos e exercícios específicos.

. . .

Resultados gerais:

- RGPs e RNs interagem significantemente, e as métricas condicionais recuperam algumas relações.
- Modelos erram as métricas condicionais, a depender do modelo e RN.
- Subestimar $S$ é menos danoso com regimes similares, e superestimar $S$ é menos danoso com regimes diferentes.


## Conclusão

Resultados dos modelos:

- K-means é um bom aproximador: melhor desempenho, consistente em diferentes DGPs, e boas previsões mesmo com $r$s imprecisos.
- Modelos de _threshold_ são similares, 'gostam' de mudanças na média.
- ST captura mudanças pequenas, é sensível às séries sem-RS, a mudanças em $\rho$, e erra a volatilidade condicional.
- MS lida com regimes assimétricos, sofre com má-especificações do RGP e erra métricas condicionais.

. . .

Resultados apresentam fatos úteis para entender processos e modelos. A análise de modelos e métricas tem potencial para recomendações práticas.


## Limitações

**Validade externa:** resultados condicionais na população de processos geradores.

Melhoria: adicionar MS-ST, MS+T, outras distribuições para MS, e transformações da variável de _threshold_.

**Conjunto de métricas:** insuficiente para descrever interações entre todos RGPs e RNs; e difícil para os modelos estimarem.

Melhoria: mais métricas (ex. outros momentos), mais medidas de dispersão, e versões ponderadas das métricas.


---

```{=tex}
\begin{center}
\Huge Obrigado!
\end{center}
```
