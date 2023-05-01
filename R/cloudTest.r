#' Test cloud /atlantis RunAtlantis.sh bug
#'
#'

for (ifolder in 1:5) {
  dir.create(here::here(paste0("out",ifolder)))
  
  # create sh script
  fileConn<-file(here::here("RunAtlantiss.sh"),open="w")
  cat("#!/bin/bash\n",file=fileConn,append=T)
  cat("find /app/model -type f | xargs dos2unix\n",file=fileConn,append=T)
  cat(paste0("atlantisMerged -i neus_init.nc 0 -o neus_output.nc -r at_run",ifolder,".prm -f at_force_LINUX.prm -p at_physics.prm -b at_biology.prm -h at_harvest.prm -e at_economics.prm -s neus_groups.csv -q neus_fisheries.csv -t . -d output\n"),file = fileConn,append=T)
  close(fileConn)

  
  runString <- paste0("sudo podman run --rm  --mount \"type=bind,src=/contrib/neus-atlantis/currentVersion/,dst=/app/model\" --mount \"type=bind,src=/home/Andrew.Beet/out",ifolder,",dst=/app/model/output/\" atlantis:6536")
  system(runString)
}
