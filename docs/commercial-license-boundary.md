# nonnest2 commercial-license boundary

## Purpose

This note records why the current `nonnest2` source cannot be silently converted to the normal ContextualWisdomLab Apache-2.0/MIT repository baseline, and what evidence would be required before the package could be treated as an approved commercial-ecosystem component.

It is a provenance and engineering-control record, not legal advice and not a change to the package's existing license.

## Current evidence

Protected `master` package metadata currently states:

- `Package: nonnest2`;
- `Version: 0.5-9`;
- `License: GPL-2 | GPL-3`;
- Edgar Merkle and Dongjun You are package authors, with additional contributors including Lennart Schneider, Mauricio Garnier-Villarreal, Seongho Bae, and Phil Chalmers; and
- the package URL is `https://github.com/qpsy/nonnest2`.

The repository also contains historical and current ContextualWisdomLab maintenance commits. Those maintenance contributions do not establish ownership of every inherited copyright interest and therefore do not establish unilateral relicensing authority over the complete package.

No root MIT/Apache license is added by this documentation lane. The R package `DESCRIPTION` license remains the authoritative source-license declaration unless and until an evidence-backed licensing change is approved by the relevant rights holders.

## ContextualWisdomLab policy boundary

GPL licenses permit commercial use under their terms. The blocker here is not a claim that GPL is noncommercial; it is that ContextualWisdomLab's current inbound policy does not accept GPL/LGPL/AGPL-family source as the normal incorporation baseline for its intended commercial distribution model.

Therefore:

- do not describe this repository as MIT-, Apache-2.0-, or otherwise permissively licensed;
- do not treat hosting or maintaining this repository as proof of relicensing authority;
- do not present the package as an approved dependency for incorporation into another ContextualWisdomLab commercial product under the normal permissive baseline;
- do not use dependency licenses to infer a different license for `nonnest2` source; and
- do not remove copyright, attribution, or source-license obligations in order to make the repository appear policy-clean.

## Closure paths

The commercial-policy blocker is resolved only by an evidence-backed repository-level outcome such as one of the following:

1. **Rights-backed permissive relicensing.** Obtain authoritative provenance and copyright-holder/contributor rights sufficient to relicense all relevant source under a commercially compatible permissive license, then update package metadata, root licensing/NOTICE material, source headers where applicable, and release evidence together.
2. **Independent compatible replacement.** Replace GPL-covered implementation with independently authored, commercially compatible source without copying or deriving from GPL implementation details that would carry derivative obligations. Preserve the validated public statistical contract through black-box/reference testing and independent implementation evidence.
3. **Explicit approved GPL distribution model.** Adopt a repository-specific commercial distribution model that is demonstrably compatible with the exact GPL obligations and explicitly approved as an exception to the organization baseline.

A README disclaimer, a new dependency, a process boundary, or the absence of a root `LICENSE` file is not sufficient closure.

## Scientific continuity required by any replacement

A licensing repair must not silently change the statistical product. At minimum, the surviving implementation must re-establish evidence for:

- `vuongtest()` distinguishability and relative-fit behavior;
- `icci()` AIC/BIC difference interval behavior;
- `llcont()` casewise contribution semantics;
- supported adapter behavior for the model classes the repository continues to advertise; and
- edge cases, numerical tolerances, and failure behavior required by existing tests and documented examples.

Release or incorporation claims should be bound to one exact source revision and ordinary package/security verification rather than inferred from this documentation record.
