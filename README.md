# gamm-psplines-longitudinal-modeling
Statistical modeling of longitudinal data using generalized additive mixed models (GAMM) with P-spline smoothing and correlation structures.

Paper: https://doi.org/10.4025/actascihealthsci.v42i1.51437

# Nonlinear Longitudinal Modeling with GAMM and P-Splines

## Overview

This repository presents a statistical framework for modeling longitudinal data using Generalized Additive Mixed Models (GAMM) combined with P-spline smoothing techniques.

The main objective is to capture nonlinear trajectories over time while accounting for subject-specific variability through random effects and correlation structures.

This approach is useful when the relationship between the response variable and time cannot be adequately described by linear models.

## Statistical Problem

In longitudinal studies, repeated measurements collected over time often exhibit nonlinear temporal patterns, between-subject variability, and within-subject correlation. Traditional linear mixed models may fail to capture these complexities. Therefore, a semiparametric approach is adopted.

## Model Specification

Let $y_{ij}$ be the response for subject $i$ at time $j$.

The GAMM model can be written as:

$$
y_{ij} = \beta_0 + \sum_{k=1}^{K} f_k(t_{ij}) + b_i + \varepsilon_{ij}
$$

where:

- $f_k(\cdot)$ represents smooth nonparametric functions estimated by P-splines
- $b_i \sim N(0, \sigma_b^2)$ represents subject-specific random effects
- $\varepsilon_{ij} \sim N(0, \sigma^2)$ represents the residual error

The smooth function is represented as a linear combination of B-spline basis functions:

$$
f(t) = \sum_{m=1}^{M} \theta_m B_m(t)
$$

with a roughness penalty:

$$
\lambda \sum_{m} \left(\Delta^d \theta_m\right)^2
$$

where:

- $B_m(t)$ represents the B-spline basis functions
- $\theta_m$ represents the spline coefficients
- $\lambda$ is the smoothing parameter
- $d$ is the order of the difference penalty

This penalization controls overfitting and ensures smooth trajectories.

## Mixed Model Representation

The GAMM can be represented in mixed-model form as:

$$
y = X\beta + Zb + \varepsilon
$$

where:

- $X$ is the fixed-effects design matrix
- $Z$ is the random-effects design matrix
- $b \sim N(0, G)$ is the vector of random effects
- $\varepsilon \sim N(0, R)$ is the vector of residual errors

This representation allows estimation using standard mixed-model frameworks, such as REML.

## Correlation Structures

To account for temporal dependence, different covariance structures were evaluated:

- No correlation structure
- AR(1)
- Compound Symmetry (CS)
- ARMA(1,1)

Model selection was performed using the Akaike Information Criterion:

$$
AIC = -2 \log L(\hat{\theta}) + 2p
$$

The AR(1) structure provided the best fit, indicating that correlations between repeated measurements decrease as the time lag increases.

## Model Estimation

The models were estimated using Restricted Maximum Likelihood (REML). The smoothing terms were estimated through penalized likelihood, and the implementation was performed in R using packages such as `mgcv`, `nlme`, and `splines`.

## Key Findings

The temporal effect was nonlinear, confirming that a simple linear model would not adequately describe the observed trajectories. The smooth terms were statistically significant, with p-values lower than 0.001 across treatment groups.

The effective degrees of freedom varied across groups, indicating different levels of complexity in the estimated trajectories. The inclusion of random effects improved model flexibility, while the AR(1) correlation structure reduced residual dependence.

## Why This Approach Matters

This framework combines nonparametric smoothing, mixed-effects modeling, penalization, likelihood-based inference, and correlation structures. Therefore, it is particularly useful for longitudinal data, growth curves, biomedical trajectories, and repeated-measures analysis.

## Reference

Silva, B. G., Guedes, T. A., Janeiro, V., Ferreira, É. C., Araújo, S. M., & Ciupa, L. (2020).  
Analyzing weight evolution in mice infected by *Trypanosoma cruzi*.  
*Acta Scientiarum. Health Sciences*, 42, e51437.  
https://doi.org/10.4025/actascihealthsci.v42i1.51437
