# 1. Get the absolute path of your current R project
proj_dir <- getwd()
makevars_path <- file.path(proj_dir, "Makevars.project")

# 2. Write the compiler flags directly to a local project Makevars file
# (This uses RPATH, which eliminates the need to mess with LD_LIBRARY_PATH)
writeLines(
  c(
    "LDFLAGS += -Wl,-rpath,/usr/gdal36/lib -Wl,-rpath,/usr/gdal36/lib64 -Wl,-rpath,/usr/geos311/lib -Wl,-rpath,/usr/geos311/lib64 -Wl,-rpath,/usr/proj82/lib -Wl,-rpath,/usr/proj82/lib64",
    "LIBS += -L/usr/gdal36/lib -L/usr/gdal36/lib64 -L/usr/geos311/lib -L/usr/geos311/lib64 -L/usr/proj82/lib -L/usr/proj82/lib64"
  ),
  con = makevars_path
)

# 3. Create/Overwrite the project's local .Renviron file (for FUTURE sessions)
writeLines(
  c(
    "PATH=/usr/gdal36/bin:/usr/geos311/bin:/usr/proj82/bin:${PATH}",
    "PKG_CONFIG_PATH=/usr/gdal36/lib/pkgconfig:/usr/gdal36/lib64/pkgconfig:/usr/geos311/lib64/pkgconfig:/usr/proj82/lib/pkgconfig",
    paste0("R_MAKEVARS_USER=", makevars_path),
    "GIT_SSL_NO_VERIFY=TRUE"
  ),
  con = ".Renviron"
)

# 4. INJECT the variables directly into the CURRENT session (no restart needed!)
Sys.setenv(PATH = paste("/usr/gdal36/bin:/usr/geos311/bin:/usr/proj82/bin", Sys.getenv("PATH"), sep = ":"))
Sys.setenv(PKG_CONFIG_PATH = "/usr/gdal36/lib/pkgconfig:/usr/gdal36/lib64/pkgconfig:/usr/geos311/lib64/pkgconfig:/usr/proj82/lib/pkgconfig")
Sys.setenv(R_MAKEVARS_USER = makevars_path)

# 5. HEAL the active session by explicitly removing the bad LD_LIBRARY_PATH override
Sys.unsetenv("LD_LIBRARY_PATH")

# Verify R_MAKEVARS_USER points to your local project directory
Sys.getenv("R_MAKEVARS_USER")
# EXPECTED: "/your/project/path/Makevars.project"

# Verify the active gdal-config binary path
Sys.which("gdal-config")
# EXPECTED: "/usr/gdal36/bin/gdal-config"

# Verify the active geos-config binary path
Sys.which("geos-config")
# EXPECTED: "/usr/geos311/bin/geos-config"

# Query the system GDAL version that R is currently targeting
system("gdal-config --version")
# EXPECTED: "3.6.4" (or 3.6.x)

# renv::install('terra')
