# gamm-psplines-longitudinal-modeling
Statistical modeling of longitudinal data using generalized additive mixed models (GAMM) with P-spline smoothing and correlation structures.

Paper: https://doi.org/10.4025/actascihealthsci.v42i1.51437

# Nonlinear Longitudinal Modeling with GAMM and P-Splines

## Overview

This repository presents a statistical framework for modeling longitudinal data using Generalized Additive Mixed Models (GAMM) combined with P-spline smoothing techniques.

The main objective is to capture nonlinear trajectories over time while accounting for subject-specific variability through random effects and correlation structures.

This approach is particularly useful when the relationship between the response and time is not adequately described by linear models.

## Statistical Problem

In longitudinal studies, repeated measurements collected over time often exhibit:

- Nonlinear temporal patterns  
- Between-subject variability  
- Within-subject correlation  

Traditional linear mixed models may fail to capture these complexities. Therefore, a semiparametric approach is adopted.

## Model Specification

Let \( y_{ij} \) be the response for subject \( i \) at time \( j \).

The GAMM model is defined as:

$$
y_{ij} = \beta_0 + \sum_{k=1}^{K} f_k(t_{ij}) + b_i + \epsilon_{ij}
$$

where:

- \( f_k(\cdot) \): smooth nonparametric functions (P-splines)
- \( b_i \sim N(0, \sigma_b^2) \): random effects (subject-specific)
- \( \epsilon_{ij} \sim N(0, \sigma^2) \): residual error

The smooth functions are estimated using P-splines, defined as:

$$
f(t) = \sum_{m=1}^{M} \theta_m B_m(t)
$$

with a roughness penalty:

$$
\lambda \sum (\Delta^d \theta_m)^2
$$

where:

- \( B_m(t) \): B-spline basis functions  
- \( \lambda \): smoothing parameter  
- \( d \): order of the difference penalty  

This penalization controls overfitting and ensures smooth trajectories.

## Mixed Model Representation

The GAMM can be rewritten in mixed model form:

$$
y = X\beta + Zb + \epsilon
$$

where:

- \( X \): fixed effects design matrix  
- \( Z \): random effects design matrix  
- \( b \sim N(0, G) \)  
- \( \epsilon \sim N(0, R) \)  

This representation allows estimation using standard mixed-model frameworks (REML).

## Correlation Structures

To account for temporal dependence, different covariance structures were considered:

- No correlation
- AR(1)
- Compound symmetry (CS)
- ARMA(1,1)

Model selection was performed using Akaike Information Criterion (AIC):

$$
AIC = -2 \log L(\hat{\theta}) + 2p
$$

The AR(1) structure provided the best fit (lowest AIC), indicating that correlations decay with time lag. :contentReference[oaicite:0]{index=0}

## Model Estimation

- Estimation method: Restricted Maximum Likelihood (REML)
- Smoothing selection: Penalized likelihood
- Implementation: R (mgcv, nlme, splines)

## Key Findings (Statistical Perspective)

- The temporal effect is **nonlinear**, confirming the inadequacy of linear models
- Smooth terms were statistically significant (p < 0.001)
- Effective degrees of freedom (e.d.f.) varied across groups, indicating different levels of complexity in trajectories
- The inclusion of random effects improved model flexibility and fit
- AR(1) correlation structure reduced residual dependence issues

## Why This Approach Matters

This framework combines:

- **Flexibility (nonparametric smoothing)**
- **Interpretability (mixed models)**
- **Statistical rigor (penalization + likelihood-based inference)**

making it highly suitable for:

- Longitudinal data
- Growth curves
- Biomedical trajectories
- Repeated measures analysis

## Reference

Silva, B. G., Guedes, T. A., Janeiro, V., Ferreira, É. C., Araújo, S. M., & Ciupa, L. (2020).  
Analyzing weight evolution in mice infected by *Trypanosoma cruzi*.  
*Acta Scientiarum. Health Sciences*, 42, e51437.  
https://doi.org/10.4025/actascihealthsci.v42i1.51437
