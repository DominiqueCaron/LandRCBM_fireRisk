# LandRCBM_fireRisk 0.0.1 (03 June 2026)

- initial module version

# LandRCBM_fireRisk (unreleased)

- **Breaking change**: the module no longer estimates burn probability itself.
  Removed the `iterations` parameter and the `pIgnition`/`pEscape`/`pSpread`
  inputs, along with the internal `calculateFireProbability()`/`simulateFire()`
  Monte Carlo simulation (and the `SpaDES.tools` dependency it needed).
- Added a single new input, `burnProbability`, read from either the
  `fireSense_burnProbability` or `scfm_burnProbability` module (both now write
  a standardized `sim$burnProbability` output). `sim$fireRisk` is computed
  directly from `sim$burnProbability`.
- Removed the `fireProbability` output; it only ever passed `sim$burnProbability`
  through unchanged, so downstream code should read `sim$burnProbability` directly.
- Removed the `getBurnProbability()` helper; `calculateFireRisk` now reads
  `sim$burnProbability` directly (it is a required input, so `simInit()` already
  errors if it is missing).
