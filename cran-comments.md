## R CMD check results

── R CMD check results ─────────────────────────────────── gtfs2gps 2.1-2 ────
Duration: 4m 47.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

* The package was removed from CRAN because it depends on another package {gtfstools} that had been removed from CRAN. The {gtfstools} package has been restablished on CRAN, so this submission is intented to restablish {gtfs2gps} on CRAN.
* In addition to that, this version fixes a couple minor issues, namely:
  * removes {magrittr} from Suggests
  * Clarifies in the documentation the default behavior of the parameter `ncores`. closes #271. When `parallel = FALSE`, this argument is ignored. When `parallel = TRUE`, then by default the function uses all available cores minus one.

