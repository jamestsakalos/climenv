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
>  CHELSA (19:33)
>  Climatologies (18:40)
>  geospatial (21:27)
>  Holdridge (25:29, 28:64)
>  Lieth (23:41)
>  SRTM (20:63)
>  WorldClim (18:26)

We confirm the spelling of these words, which we have listed in inst/WORDLIST.


## Downstream dependencies

There are no downstream dependencies.


## Responses to remarks from CRAN submission - 1

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

We have restored user options to default settings throughout the package.

## Responses to remarks from CRAN submission - 2

> remove examples that are commented out

We have removed them

> lengthy examples (>5 sec) can be wrapped in `/donttest{}`

All examples that are lengthy are wrapped. Where possible we have attempted to
reduce their complexity and timing, however, our package works with big,
global data and, even in simple cases these take time to download.

> only use `on.exit()` within functions.

We have replaced `on.exit()` from examples with appropriate code. 
