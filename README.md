# Forecasting next-day S&P 500 returns: OLS against principal component regression

Rolling-window forecasts of next-day S&P 500 returns from the returns of ten of
its largest constituents, comparing ordinary least squares against principal
component regression, and evaluating both by annualised Sharpe ratio under a
long-short strategy.

The short version: neither method produced a reliably positive out-of-sample
Sharpe, and the more interesting result is *why* OLS looked better than it
deserved to.

## Setup

Ten constituents (AAPL, TSLA, V, MSFT, JNJ, AMZN, JPM, AVGO, MA, NVDA) plus the
index, daily adjusted closes from 2020-11-25 to 2025-11-30, roughly 1,257
observations, pulled with `quantmod`.

Split 50 / 25 / 25 into training, validation and test, in time order.

Each of the eleven return series is assumed to follow an IGARCH(1,1) process,
with the exponential smoothing parameter lambda estimated by maximum likelihood
on the training set only and reused unchanged on validation and test. Returns are
then normalised by the resulting conditional volatilities.

The strategy takes a unit long position when the forecast is positive and a unit
short when negative, so it is always fully invested and always at maximum size.
Sharpe is annualised as `sqrt(250) * mean(return) / sd(return)`, ignoring
transaction costs.

Window length `D` runs from 50 to 250 in steps of 20.

## Findings

**OLS.** With no index lags (`q = 0`), Sharpe is positive across the validation
set, peaking near 5.4, and mostly negative on test, ranging from 2.88 down to
about −0.53. Adding one lag (`q = 1`) does not change the picture: validation
looks strong, test does not. A validation set that flatters a model this much
while test does not is the usual signature of a window length chosen against
noise.

**Principal component regression.** One factor produced far more moderate
forecasts than OLS. The reason is multicollinearity: the constituent log-price
paths move together closely enough that the design matrix is badly conditioned,
and OLS responds by taking enormous offsetting positions. PCR removes that by
construction.

But moderating the forecasts did not improve the Sharpe. One-factor PCR was
negative across the whole test range, roughly −0.11 to −2.48. Two factors were
mixed, from −3.77 to 2.26, and still produced unreasonable positions.

**The uncomfortable conclusion.** Under this specific strategy, OLS scores better
than PCR despite making visibly worse forecasts. That is an artefact of the
strategy rather than a fact about the models: because the position is always
±1 unit regardless of forecast magnitude, only the *sign* of the forecast is ever
used. OLS's wild magnitudes are therefore free, and PCR's main advantage,
producing sensible position sizes, is thrown away before it can help.

So the honest reading is not "OLS beats PCR". It is that this evaluation cannot
distinguish them, because a sign-only strategy discards exactly the information
PCR improves. A size-scaled strategy would be the natural next step and would
probably reverse the ranking.

## Running it

```r
install.packages(c("tidyverse", "quantmod", "reshape2"))
source("ST326_coursework.r")
```

Yahoo Finance data via `quantmod`, so no API key needed and no data is committed.
Numbers will shift slightly as the adjustment factors behind adjusted closes get
revised.

Note the file is exploratory research code rather than a package. It runs
top to bottom but carries scratch work and intermediate checks, and functions are
defined alongside the analysis that uses them. It is preserved as written rather
than tidied.

## Attribution

`pred.r.prepare`, `thresh.reg`, `rolling.thresh.reg` and `sharpe.curves` are
adapted from templates supplied with the LSE ST326 Financial Statistics course.
The MLE estimation, the volatility normalisation, the modified long-short
strategy, and the factor-model functions (`fctr.mdl.train`, `fctr.mdl.valid`,
`fctr.mdl.test`, `sharpe.curves.fctr`) are mine.

Written for ST326 assessed coursework, LSE, December 2025.

## What is missing

No test suite. The code depends on a live data pull and was written as a
single analysis script, so there is nothing here that runs offline against fixed
inputs. That is a real gap rather than an oversight.

The obvious extensions: scale position size by forecast magnitude so PCR's
advantage can actually register; include transaction costs, which the brief
excluded and which a sign-flipping daily strategy would be very sensitive to;
select window length on validation and report only the resulting test figure,
rather than reporting the whole test curve; and use a scree plot or explained
variance to choose the factor count instead of capping it at two.
