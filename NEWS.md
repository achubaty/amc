# amc v1.0.0

- drop support for R <+ 4.0;
- use native pipe instead of `magrittr`;
- removed retiring geospatial dependencies (`rgdal`);
- removed `getOGR()`, which relied on `rgdal`;
- add `sf` and `terra` support;


# amc v0.2.1

- move `notify_slack()` to `SpaDES.project`

# amc v0.2.0

- drop support for R 3.5 (requirement of several dependencies)
- remove deprecated and defunct functions
- add `notify_slack()`

# amc v0.1.1

- add `flink` for creating file (and directory) symlinks
- don't write row numbers in download checksum files
- numerous bug fixes

# amc v0.1.0

- initial version
