#!/usr/bin/env bash
set -e

menu() {
	echo "1) tar_make() on local"
	echo "2) tar_make_clustermq() on local"
	echo "3) tar_make() on Apptainer"
	echo "4) tar_make_clustermq() on Apptainer"
	echo "5) Enter in the Apptainer container"
	read -rp "Enter number：" menu_num
  case $menu_num in
  1)
    Rscript run.R 1
    ;;
  2)
    Rscript run.R 100
    ;;
  3)
 		apptainer exec --env RENV_PATHS_CACHE=/home/${USER}/renv \
		--env RENV_PATHS_PREFIX_AUTO=TRUE \
 		radian.sif Rscript run.R 1
    ;;
  4)
 		apptainer exec --env RENV_PATHS_CACHE=/home/${USER}/renv \
		--env RENV_PATHS_PREFIX_AUTO=TRUE \
 		radian.sif Rscript run.R 100
    ;;
  5)
 		apptainer shell --env RENV_PATHS_CACHE=/home/${USER}/renv \
		--env RENV_PATHS_PREFIX_AUTO=TRUE \
 		radian.sif bash
    ;;
	*)
    echo "Type 1-3"
    ;;
  esac
}

menu "$@"
