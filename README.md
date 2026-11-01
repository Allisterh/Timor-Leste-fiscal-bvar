# Timor-Leste Fiscal BVAR

This repository contains the data-processing pipeline, local projections (the preferred baseline), Bayesian Vector Autoregression (BVAR) robustness estimates, impulse-response functions (IRFs), forecast error variance decompositions (FEVDs), and visual outputs used to study fiscal transmission and fiscal leakage in Timor-Leste.

The central question is how public spending affects non-oil GDP and what portion of that spending is related to imports. The paper compares two samples:

- **INETL quarterly indicator, 2014Q1–2023Q4 (40 observations)**, built from INETL Table 7.1. This is the baseline, estimated with local projections; the BVAR is a robustness check.
- **Long interpolated sample, 2002Q1–2023Q4 (88 observations)**, built from annual ADB data with Denton–Cholette temporal disaggregation. This is the long-sample benchmark.

The main finding is that the strong import-leakage result obtained from the long interpolated sample does not survive with the INETL quarterly indicator. Local projections estimate small import responses in 2014–2023, and a common-window 2x2 decomposition shows that interpolation alone is not sufficient: neither interpolating the fiscal series nor interpolating the outcome series reproduces the leakage over that window. The long-sample result is therefore fragile and sample-window-dependent, not a mechanical consequence of temporal disaggregation; the pre-2014 subsample is stable but small, and the long-sample estimate is sensitive to the event-dummy specification. We recommend using indicator-based quarterly data for inference and treating long interpolated samples as historical benchmarks.

## Temporal disaggregation methodology

Timor-Leste has a relatively short macroeconomic history, and most national accounts and monetary indicators are reported annually. The long-sample comparison therefore disaggregates the annual series using univariate temporal-disaggregation methods. The baseline uses local projections and does not use our univariate interpolation, but it is not raw quarterly data either: INETL builds its quarterly indicator by replicating annual GDP and disaggregating it with quarterly indicators. The comparison therefore isolates the incremental effect of univariate interpolation and the sample window, not the difference between raw and constructed data.

Initially, quarterly import data from Trade Map were tested as a high-frequency indicator for Chow–Lin disaggregation. The external indicator was not statistically significant, so the univariate proportional Denton–Cholette method (Denton, 1971; Cholette, 1984) is applied to all long-sample series.

Flow variables (real non-oil GDP, government expenditure, and real imports) are disaggregated subject to sum constraints ($\sum_{q=1}^4 x_{t,q} = X_t$). Index and stock variables (CPI and private credit) are subjected to quarterly average constraints. For the CPI, the historical disaggregated series (2002–2012) is integrated with official monthly observed data (2013–2025), aggregated to quarterly frequency via arithmetic averages.

## Variables and units

The baseline local projections and the robustness BVAR using the INETL indicator include three endogenous variables:

| Variable     | Description                                      |
|--------------|--------------------------------------------------|
| `ln_gov_exp` | General government final consumption expenditure |
| `ln_gdp_non` | Real non-oil GDP                                 |
| `ln_imports` | Real imports of goods and services               |

The five-variable long-sample comparison and the five-variable INETL robustness check add `ln_cpi` (consumer price index) and `ln_credit` (private credit). Variables are in natural logarithms. Monetary variables (except CPI) are measured in millions of US dollars; CPI is an index.

The INETL sample uses two exogenous indicators: `shock_2016` and `shock_covid`. The long sample uses five: `shock_2006`, `shock_2009`, `shock_2012`, `shock_2016`, and `shock_covid`.

## Data sources

- INETL quarterly indicator: [INETL National Accounts 2004–2023](https://www.laohamutuk.org/DVD/DGS/NatlAccts2023en.pdf), Table 7.1, constant 2015 prices.
- Annual macroeconomic indicators for the long sample: [Asian Development Bank Key Indicators - Timor-Leste](https://kidb.adb.org/economies/timor-leste), stored in `Data/data-source/`.
- Quarterly import indicator for the long sample: [Trade Map - countries/economy groups](https://www.trademap.org), stored in `Data/data-source/`.
- Consumer price index: DNE monthly CPI, stored in `Data/data-source/`.
- Private credit: BCTL monetary survey, monthly end-of-quarter series.

The raw source files are retained in `Data/data-source/`. Intermediate cleaned and model-ready files are written to `Data/csv/`.

## Repository structure

``` text
Data/data-source/ Raw source files and source notes
Data/csv/ Cleaned, quarterly, source-data, IRF, and FEVD data
graphics/ PDF figures generated by the analysis
scripts/ Reproducible R analysis scripts
dashboard/ Shiny dashboard and dashboard-ready IRF data
```

## Analysis workflow

The baseline uses the INETL quarterly indicator for 2014Q1–2023Q4. The 2002–2023 series built by univariate temporal disaggregation is retained as a long-sample comparison. The main finding is that the strong import-leakage result from the long interpolated sample does not survive with the INETL indicator, and a common-window 2x2 shows that interpolation alone does not reproduce it over 2014–2023. Local projections are the preferred baseline; the BVAR is a robustness check.

Run the scripts in the following order:

1.  `scripts/00_build_quarterly_official.R` downloads the INETL National Accounts table, builds the official quarterly GDP, imports, government expenditure, CPI, and credit series, applies X-13ARIMA-SEATS, and writes the seasonally adjusted and NSA datasets.
2.  `scripts/01_cleaning.R` reads the ADB workbook, selects the five annual macroeconomic series, cleans missing values, and converts the Trade Map import file into a quarterly import indicator for the long sample.
3.  `scripts/02_dessagregation.R` creates quarterly levels and log series for the long sample from the annual data and the quarterly import indicator.
4.  `scripts/03_structural_failures.R` estimates Bai-Perron breakpoints under the BIC criterion and creates the shock-window dummies for the long sample.
5.  `scripts/04_stationary_test.R` runs ADF, KPSS, and Zivot-Andrews tests.
6.  `scripts/05_graphics.R` creates the macroeconomic level and log-scale figures.
7.  `scripts/06_hyperparameters_lag_bvar_test.R` compares lag lengths using BVAR log-marginal likelihoods for the long sample.
8.  `scripts/07_irf_fevd.R` estimates the BVAR robustness check, generates IRFs, cumulative IRFs, FEVDs, and stability diagnostics for two Cholesky orderings. Set `SAMPLE` to `official_3var`, `official_5var`, `official_nsa`, `long_3var`, `long`, `long_3var_2dum`, `official_private_demand`, `official_gdp_ex_gov`, `official_broad`, or `official_broad_dp`.
9.  `scripts/08_local_projections.R` estimates the main local projections for the official, NSA, and long samples with `lpirfs::lp_lin_iv`. The three core responses use two lags of the core variables; CPI and private credit use the same estimator with those lags as exogenous controls.
10. `scripts/09_hybrid_robustness.R` estimates the 2x2 hybrid design on the common 2014-2023 window and a period decomposition for the long sample, including the long sample with only two event dummies.
11. `scripts/10_first_difference_bvar.R` estimates the three-variable BVAR in first differences and reports cumulative IRFs scaled to a 1% government expenditure increase. Set `N_DRAW`, `N_BURN`, and `N_THIN` to change the sampler for this robustness check.
12. `scripts/11_prior_sensitivity.R` compares hierarchical and fixed-lambda BVAR estimates with an OLS-VAR benchmark.
13. `scripts/12_power_analysis.R` runs the Monte Carlo power analysis for the official local projections and reports BVAR precision from the posterior intervals.
14. `scripts/13_residual_seasonality.R` tests the X-13-adjusted official series for residual seasonality using joint F tests on quarterly dummies.
15. `scripts/build_dashboard_data.R` converts the existing BVAR and LP outputs into compact app data in the `dashboard/` folder.

The complete sequence is also defined in `.github/workflows/reproducibility.yml` and is used by the repository's reproducibility workflow.

## Estimation: local projections and BVAR

The baseline estimation uses the INETL 2014Q1–2023Q4 sample with three variables (government expenditure, imports, and non-oil GDP), two lags, and a Minnesota prior. The sampler is run with 100,000 draws, 20,000 burn-in draws, and thinning every 20 draws. Impulse responses are calculated for 12 quarters with 68% and 90% credible intervals. `BVAR::bvar` does not accept exogenous regressors, so event and seasonal dummies are partialled out of the endogenous variables by OLS before estimation; local projections include the dummies directly as controls.

Two recursive Cholesky orderings are estimated to assess sensitivity to the identification assumption:

1.  Government expenditure, imports, non-oil GDP.
2.  Non-oil GDP, government expenditure, imports.

The five-variable INETL model adds the CPI and private credit but is unstable with 40 observations (maximum companion modulus 1.033); it is reported only as a robustness check. The long-sample 2002–2023 models are estimated with the same prior and sampler settings, both with three and five variables; the five-variable maximum modulus is 0.999, close to the unit circle. Local projections use a recursively identified government-spending shock from a three-variable VAR and Newey-West standard errors. All responses are estimated with `lpirfs::lp_lin_iv`; the auxiliary regressions for CPI and credit keep the two core lags as exogenous controls.

## Outputs

- `Data/csv/timor_quarterly_official.csv`: official quarterly levels, 2014–2023.
- `Data/csv/official_data_validation.csv`: official–annual reconciliation for GDP, imports, government expenditure, and credit.
- `Data/csv/gov_fce_decomposition.csv`: official GFC, Development & TA, and ADB broad government expenditure.
- `Data/csv/timor_quarterly_official_log_and_dummys.csv`: official seasonally adjusted log data plus dummies.
- `Data/csv/timor_quarterly_official_nsa_log_and_dummys.csv`: official NSA log data plus quarterly dummies.
- `Data/csv/timor_quarterly_hybrid_log_and_dummys.csv`: hybrid data with the interpolated ADB government series and official outcomes.
- `Data/csv/timor_quarterly_hybrid_outcomes_log_and_dummys.csv`: hybrid data with the official government series and interpolated outcomes.
- `Data/csv/timor_quarterly_long_2014_log_and_dummys.csv`: interpolated long-sample data restricted to 2014-2023.
- `Data/csv/timor_quarterly_long_pre_2014_log_and_dummys.csv`: interpolated long-sample data restricted to 2002-2013.
- `Data/csv/timor_annual_macro.csv`: cleaned annual macroeconomic data for the long sample.
- `Data/csv/timor_quarterly_data.csv`: long-sample quarterly variables in levels.
- `Data/csv/timor_quarterly_log_and_dummys.csv`: long-sample log data plus exogenous dummies.
- `Data/csv/irf_*_order_*_<sample>.csv`: standard and cumulative IRF quantiles.
- `Data/csv/fevd_order_*_<sample>.csv`: FEVD quantiles.
- `Data/csv/lp_irf_<sample>.csv`: local-projection impulse responses and HAC bands.
- `Data/csv/hybrid_2x2_summary.csv`: common-window 2x2 hybrid responses.
- `Data/csv/period_decomposition_summary.csv`: official, 2014-2023, 2002-2013, and 2002-2023 comparisons.
- `Data/csv/bvar_diff_summary.csv`: cumulative first-difference BVAR responses.
- `Data/csv/residual_seasonality.csv`: joint F tests for residual seasonality in the adjusted official series.
- `Data/csv/stability_order_*_<sample>.csv`: companion-matrix stability diagnostics.
- `Data/csv/stationarity_<sample>.csv`: ADF, KPSS, and Zivot-Andrews tests.
- `dashboard/bvar_irf.csv` and `dashboard/lp_irf.csv`: compact BVAR and LP data used by the Shiny app.
- `graphics/`: macroeconomic figures, IRF figures, FEVD figures, and stability diagnostics.

## Interactive dashboard

The Shiny dashboard reports local projections as the preferred baseline and BVAR results as a robustness check for the official and long samples. The LP tab opens first. It allows users to switch between the two samples and select LP response variables; the BVAR tab allows users to choose the response and compare standard and cumulative IRFs:

[Open the Timor-Leste fiscal transmission dashboard](https://connect.posit.cloud/em3r/content/01a068c1-99da-638d-1ba0-1254293e5a61)

The dashboard source (`dashboard/app.R`) and its two compact CSV inputs live inside `dashboard/`, which is the folder uploaded to Posit Connect. The generator is `scripts/build_dashboard_data.R`; it is a local helper and does not need to be uploaded. To rebuild the dashboard data from existing analysis outputs and run the app locally from the repository root:

``` r
Rscript scripts/build_dashboard_data.R
shiny::runApp("dashboard")
```

## Reproducing the project

The analysis requires R 4.4 or later and the R packages used in the scripts (including `tidyverse`, `tempdisagg`, `BVAR`, `lpirfs`, `seasonal`, `x13binary`, `strucchange`, `tseries`, `urca`, `shiny`, and `plotly`). Poppler is required to extract the INETL PDF table.

From the repository root, the workflow can be run with:

``` bash
Rscript scripts/00_build_quarterly_official.R
Rscript scripts/01_cleaning.R
Rscript scripts/02_dessagregation.R
Rscript scripts/03_structural_failures.R
Rscript scripts/04_stationary_test.R
Rscript scripts/05_graphics.R
Rscript scripts/06_hyperparameters_lag_bvar_test.R
SAMPLE=official_3var Rscript scripts/07_irf_fevd.R
SAMPLE=official_5var Rscript scripts/07_irf_fevd.R
SAMPLE=official_nsa Rscript scripts/07_irf_fevd.R
SAMPLE=long_3var Rscript scripts/07_irf_fevd.R
SAMPLE=long Rscript scripts/07_irf_fevd.R
SAMPLE=long_3var_2dum Rscript scripts/07_irf_fevd.R
SAMPLE=official_private_demand Rscript scripts/07_irf_fevd.R
SAMPLE=official_gdp_ex_gov Rscript scripts/07_irf_fevd.R
SAMPLE=official_broad Rscript scripts/07_irf_fevd.R
SAMPLE=official_broad_dp Rscript scripts/07_irf_fevd.R
SAMPLE=official Rscript scripts/08_local_projections.R
SAMPLE=official_nsa Rscript scripts/08_local_projections.R
SAMPLE=long Rscript scripts/08_local_projections.R
Rscript scripts/09_hybrid_robustness.R
Rscript scripts/10_first_difference_bvar.R
Rscript scripts/11_prior_sensitivity.R
Rscript scripts/12_power_analysis.R
Rscript scripts/13_residual_seasonality.R
Rscript scripts/build_dashboard_data.R
```

The estimates depend on the source data, the temporal-disaggregation assumptions, the selected prior, and the recursive ordering. Results should therefore be interpreted as model-based evidence for a data-limited economy, not as a substitute for a full structural analysis of the Timorese economy. The comparison between the INETL indicator and the long interpolated sample is the paper's main methodological result: the strong leakage estimated from the long interpolated sample is not robust to the indicator-based quarterly observations, and the common-window 2x2 shows that interpolation alone does not reproduce it over 2014-2023. We therefore recommend indicator-based quarterly data for inference and long interpolated samples only as historical benchmarks. The pre-2014 subsample is stable but small, and the long-sample estimate is sensitive to the event-dummy specification.

## How to Cite

If you use this code or the data generated in your research, please cite this repository. Below is an example entry in APA format. For a more formal citation, please also check the "How to Cite" section of the associated paper (if available).

**Repository Citation:**

> López, E., & Gomez, J. (2026). *The Fragility of Fiscal Leakage Estimates: Data Construction and Sample Windows in Timor-Leste*. GitHub. <https://github.com/Emerson-nic/Timor-Leste-fiscal-bvar-lp>

**Note:** This repository is part of ongoing research. Citation information will be updated upon publication of the associated paper. For the final paper, please cite the published version with all co-authors
