## Test environments

* Local PC:
  - Windows 10, R 4.3.1

* [GitHub Actions](https://github.com/jamestsakalos/climenv/actions/workflows/R-CMD-check.yml)
  - Ubuntu 20.04
    - R 4.1
    - R release
    - R devel
  - Mac OS X 12.7, R release
  - Microsoft Windows Server 2022 10.0.20348, R release
  
* R-hub, with `rhub::check_for_cran()` and `devtools::check_win_devel()`

## R CMD check results

There were no ERRORs or WARNINGs.

There were two NOTEs:

> New submission

This is a new submission.
 
> Possibly misspelled words in DESCRIPTION:
>  CHELSA (20:56)
>  Holdridge (25:21, 27:22)
>  Lieth (24:63)
>  SRTM (20:75)
>  WorldClim (20:43)
>  climatological (18:43)
>  geospatial (21:49)

We confirm the spelling of these words, which we have listed in inst/WORDLIST.

## Downstream dependencies

There are no downstream dependencies.
