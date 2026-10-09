# LandRCBM_fireRisk 0.0.1 (03 June 2026)

- initial module version

# LandRCBM_fireRisk (unreleased)

- **Breaking change**: the module no longer estimates burn probability itself.
  Removed the `iterations` parameter and the `pIgnition`/`pEscape`/`pSpread`
  inputs, along with the internal `calculateFireProbability()`/`simulateFire()`
  Monte Carlo simulation (and the `SpaDES.tools` dependency it needed).
- Added a `fireModel` parameter (`"fireSense"`, `"scfm"`, or `NA` to
  auto-detect) and two new inputs, `fireSense_BurnProbability` and
  `scfm_BurnProbability`, read from the new `fireSense_burnProbability` and
  `scfm_burnProbability` modules respectively. `sim$fireProbability` is now set
  directly from whichever of these two is supplied.
