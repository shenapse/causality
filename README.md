# Sample code estimating ATT and ATE with propensity score

- Contents

  - Matching and balancing diagnosis
  - Estimation of ATT by matching
  - Estimation of ATE by doubly robust estimator (using GBDT for outcome model)
  - Cluster-Robust std
  - Boot-strapping std
  - with IHDP dataset

- Main Packages
  - matching: Matching and MatchIt, WeightIt package
  - balancing diagnosis: cobalt package

## References

- Imbens, G., & Rubin, D. (2015). Causal Inference for Statistics, Social, and Biomedical Sciences: An Introduction. Cambridge University Press.
- [Covariate Balance Tables and Plots: A Guide to the cobalt Package](https://cran.r-project.org/web/packages/cobalt/vignettes/cobalt.html#comparing-balancing-methods)
- [Estimating Effects After Matching - MatchIt](https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html)
- 岩崎 学 (2015). 統計的因果推論. 朝倉書店.
- 高橋 将宜 (2022). 統計的因果推論の理論と実装. 共立出版.
