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
>  Lieth (24:65)
>  SRTM (20:77)
>  WorldClim (20:43)
>  climatological (18:43)
>  covariates (22:61)
>  geospatial (21:49)

We confirm the spelling of these words, which we have listed in inst/WORDLIST.


## Downstream dependencies

There are no downstream dependencies.


## Responses to remarks from CRAN submission

> Add references describing the methods in the package to the DESCRIPTION file
> in the description field.

We have included new references describing the methods in the DESCRIPTION.

> Explain all acronyms in the description text. e.g., SRTM, CHELSA.

We confirm the description of all acronyms in the package description.

> `\dontrun{}` should only be used if the example really can not be executed and
> results in the "# Not run:" as a warning for the user. If they are not 
> executable in ~5 seconds unwrap them, or replace with `\donttest{}`.

We have switched from `\dontrun{}` to `\donttest{}`.

> Check that the user's options, par or working directory are not changed

We have made sure to restore the users's options throughout the package.
