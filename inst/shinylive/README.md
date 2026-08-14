# cofad on webR

This entry point builds a static Shinylive version of cofad. Shinylive runs
Shiny for R in the browser through webR, so calculations happen on the user's
device and no Shiny server is required.

From the repository root, run:

```r
install.packages("shinylive")
source("tools/build-shinylive.R")
```

Preview the generated site with:

```r
httpuv::runStaticServer("docs")
```

The build script reports export time and output size. First-load latency also
depends on network speed, browser cache, and the WebAssembly packages required
by the app.
