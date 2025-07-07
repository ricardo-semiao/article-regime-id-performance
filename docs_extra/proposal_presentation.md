---
title: "Tema de Tese - Mestrado EESP"
author: "Ricardo Semião e Castro"
date: "2025-04-22"
format:
    revealjs:
        fontsize: 26pt
        embed-resources: true
---

# Overview

## Motivação

**Objetivo geral:** estudar modelos de previsão de séries de tempo com mudanças de regime (RS).

**Interesses:**

- **_Workflow_ de trabalhos de previsão**: montar base de dados, análise e tratamentos, especificação e comparação de modelos, etc. 
- **Modelos de mudança de regimes**: importante considerar padrões variáveis, mas modelos de time-varying parameters costumam ser difíceis de implementar. Variedade de modelos, de interessante comparação.
- **Adicionais:** criação de dados, dados em alta dimensão, criação de métricas de comparação de modelos, sensibilidade a priors.


## Objetivos

**Objetivo metodológico:**

- Comparar modelos de RS, tanto em termos de performance, mas especificamente em termos dos regimes gerados.
  - Como os regimes alinham com conhecimento institucional e literatura.
  - Definir e aplicar métricas sobre diferenças de regimes (_research project_ de Econometria II).
- Estudar como as características dos regimes se relacionam com a performance dos modelos. (definir uma hipótese?).

**Objetivo aplicado:** estudar profundamente a previsão de uma variável menos estudada, prescrever uma recomendação de modelo.


## Escolha de Variável

**Características:**

- Existir em um contexto de mudança de regimes.
- Ainda existir espaço na literatura para estudos sobre sua previsão.
- Na área de finanças ou macroeconomia.
- Idealmente, no contexto de dados de maior dimensão.

**Opção recente:** índices de sentimento macroeconômicos.



# Métricas de Comparação de Regimes

## Estatísticas Básicas

- Momentos da(s) variavel(is) de interesse:
    - Média, variância, covariância, etc.
    - ACF, PACF, CCF.
- Duração média.
- Qualidade do fit: R^2, MSE, critérios de informação.
- Força de causalidade de Granger.
- Específicas de modelo: probabilidades de transição.

Estacionariedade é relevante.


## Específicas de Tema

- Relação com regimes institucionais:
    - Dominância fiscal ou monetária, crescimento ou depressão, etc.
    - Correlação (ou mais geral) com variáveis de interesse. <!--mutual info-->


## Estatísticas Específicas

- Métricas com base nas observações de cada regime:
    - Distâncias dos centroides: Davies–Bouldin index, silhueta.
    - Distâncias de distribuição das variáveis: Earth mover's. <!--Kullback–Leibler divergence-->
- Relevância de variáveis em cada regime: pesos de PCA, % explicação de variância, etc.


## Estatísticas Específicas

- Métricas com base nos parâmetros de cada regime:
    - Testes de diferença dos parâmetros.
    - Comparação de ACFs e IRFs implicadas pelos parâmetros (RP de Econometria II).
        - $\Delta^{r_2}_{r_1} P = \Phi^{r_1} - \Phi^{r_2}$, possibly calculated in row/column-wise fashion.
        - $\Delta^{r_2}_{r_1} \rho_{i}(l; \Phi^{r})$, accumulated alternative: $\sum_{l = 1}^H \Delta^{r_2}_{r_1} \rho_{i}(l; \Phi^{r})$.
        - $\Delta^{r_2}_{r_1} IRF_{ij}(h; \Phi^{r})$, accumulated, discounted, alternative: $\sum_{h = 1}^H \beta^h \Delta^{r_2}_{r_1} IRF_{ij}(h; \Phi^{r})$.
    - Comparação de distribuições em modelos Bayesianos.
