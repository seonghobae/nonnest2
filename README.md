# nonnest2

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/ContextualWisdomLab/nonnest2)

**Evidence-based comparison of nested and non-nested statistical models in R.**

`nonnest2` helps analysts answer two questions that ordinary fit indices do not answer by themselves: whether two fitted models are empirically distinguishable, and which model is better supported when they are distinguishable. The package implements tests based on Vuong (1989) and also provides confidence intervals for differences in AIC and BIC.

The package works with several established R model classes and exposes a small adapter boundary for additional classes. It is a statistical comparison library; it does not choose a substantive model for you, establish causal validity, or turn information criteria into proof of scientific truth.

## When to use it

Use `nonnest2` when you have two fitted models for the same observations and need a casewise-likelihood comparison rather than a simple comparison of aggregate fit statistics.

Typical jobs include:

- testing whether two models are distinguishable from the observed data;
- comparing relative model fit using Vuong-style tests;
- obtaining interval estimates for differences in AIC and BIC; and
- extending the comparison machinery to another model class that can provide the required casewise contributions.

Built-in `llcont()` methods currently cover model classes from ecosystems including `lavaan`, `mirt`, `OpenMx`, base/generalized linear models, and several other regression-model families. The exact exported API is `vuongtest()`, `icci()`, and `llcont()`.

## Install from this source tree

The package metadata requires R 3.0.0 or later. From a checked-out revision:

```bash
R CMD INSTALL .
```

For development or release verification, use the repository's R package workflow rather than inferring quality from source presence alone:

```bash
R CMD build .
R CMD check nonnest2_*.tar.gz
```

The repository also contains an `R-CMD-check` GitHub Actions workflow. Its current result should be verified on the exact revision you intend to use.

## Quick start

The example below compares two factor models that are not nested because the indicator assignments differ.

```r
library(lavaan)
library(nonnest2)

m1 <- '
  visual  =~ x1 + x2 + x3 + x4
  textual =~ x4 + x5 + x6
  speed   =~ x7 + x8 + x9
'
fit1 <- cfa(m1, data = HolzingerSwineford1939)

m2 <- '
  visual  =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6 + x7
  speed   =~ x7 + x8 + x9
'
fit2 <- cfa(m2, data = HolzingerSwineford1939)

vuongtest(fit1, fit2)
icci(fit1, fit2)
```

Interpret the distinguishability result before treating a relative-fit result as meaningful, and interpret both in the context of the models' assumptions and the scientific question.

## Integration model

`nonnest2` keeps the public comparison layer small. Model-specific integration happens through casewise likelihood/derivative contributions exposed by `llcont()` methods. New model classes should provide the statistical quantities expected by that adapter contract rather than embedding package-specific branching into `vuongtest()` itself.

For the exact supported S3 methods, see [`NAMESPACE`](NAMESPACE). Package history is recorded in [`NEWS`](NEWS), and longer worked material lives under [`vignettes/`](vignettes/).

## Statistical basis

The package metadata cites:

> Vuong, Q. H. (1989). Likelihood ratio tests for model selection and non-nested hypotheses. *Econometrica, 57*(2), 307–333. https://doi.org/10.2307/1912557

The software implements statistical procedures based on that theory; using the software does not remove the need to check identification, estimator assumptions, data quality, model misspecification, and the substantive meaning of the compared models.

## Project status and verification

Current package metadata declares version `0.5-9` dated 2026-03-31. Treat that as source/package metadata, not by itself as evidence of a particular published artifact, deployment, benchmark, or certification. For any consequential analysis, bind results to the exact package revision and preserve the fitted-model inputs and software environment needed to reproduce the comparison.

## Contributing

Keep changes focused on the statistical contract and supported model adapters. New adapters should include realistic tests for casewise contributions and comparison behavior. Changes to numerical formulas, supported model semantics, or public return values should be documented and verified through the repository's ordinary R package checks.

## License and commercial-use boundary

The authoritative package metadata declares **`GPL-2 | GPL-3`** and identifies upstream/external authors and contributors. This repository therefore does **not** claim a new MIT or Apache-2.0 grant for the inherited package source.

GPL licenses permit commercial use under their terms, but GPL-family source is not an approved default inbound component under ContextualWisdomLab's current commercial ecosystem policy. Accordingly, this repository should not be presented as approved for incorporation into a ContextualWisdomLab commercial product unless the exact provenance, copyright-holder rights, distribution model, and copyleft obligations are resolved through an explicitly approved repository-level path.

See [`docs/commercial-license-boundary.md`](docs/commercial-license-boundary.md) for the evidence and closure conditions. Dependency licenses are separate from the license of `nonnest2` itself and do not relicense this package.

The original NSF acknowledgement remains part of the package history: material in this package is partially based on work supported by NSF grant SES-1061334, and the package contents do not represent NSF views.
