## Test environments
* local Windows 10 install, R 3.4.3
* ubuntu 12.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Hello, this is an update. Same warning on local check than for previous release. OK on win-builder.


---

* I have run R CMD check and have one warning that does not appear in R CMD check results :
  * checking data for ASCII and uncompressed saves ... OK
  WARNING
  'qpdf' is needed for checks on size reduction of PDFs
