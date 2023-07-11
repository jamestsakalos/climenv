# elev() fails gracefully

    Code
      elev(tmp_dir, sea, "GEOdata", quiet = TRUE)
    Warning <simpleWarning>
      Coordinate reference system not specified; assuming EPSG:4326
      Could not download srtm_12_24: HTTP status 404
    Error <simpleError>
      No data downloaded.

# elev()

    Code
      geo_elev <- elev(tmp_dir, island, "GEOdata", quiet = TRUE)
    Warning <simpleWarning>
      Coordinate reference system not specified; assuming EPSG:4326
      Could not download srtm_69_22: HTTP status 404
      Could not download srtm_69_23: HTTP status 404
      Could not download srtm_67_24: HTTP status 404
      Could not download srtm_69_24: HTTP status 404

