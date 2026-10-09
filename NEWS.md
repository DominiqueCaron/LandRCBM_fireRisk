# LandRCBM_fireRisk 0.0.1 (03 June 2026)

- initial module version

# LandRCBM_fireRisk (unreleased)

- **Breaking change**: the module no longer estimates burn probability itself.
  Removed the `iterations` parameter and the `pIgnition`/`pEscape`/`pSpread`
  inputs, along with the internal `calculateFireProbability()`/`simulateFire()`
  Monte Carlo simulation (and the `SpaDES.tools` dependency it needed).
- Added a single new input, `burnProbability`, read from either the
  `fireSense_burnProbability` or `scfm_burnProbability` module (both now write
  a standardized `sim$burnProbability` output). `sim$fireProbability` is set
  directly from `sim$burnProbability`.
