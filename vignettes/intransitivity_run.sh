#!/usr/bin/env bash
#
# intransitivity_run.sh: run intransitivity_compute.R in batches, each in a fresh R
# process, so no batch's workers inherit another's memory. Every unit's result is
# cached as soon as it finishes, so a rerun resumes where a run stopped.
#
# Usage (from the site root):
#   setsid nohup vignettes/intransitivity_run.sh units > /dev/null 2>&1 &   # build the units, then stop
#   setsid nohup vignettes/intransitivity_run.sh tests > /dev/null 2>&1 &   # test batches, calibration, extras
# Logs: vignettes/intransitivity_data/work/run_<batch>.log
# IRW_CORES (default 4) sets the workers per batch. Before each batch the script
# waits until at least MIN_GB (default 12) of memory is available.

set -euo pipefail
cd "$(dirname "$0")/.."
export IRW_CORES="${IRW_CORES:-4}"
MIN_GB="${MIN_GB:-12}"
log_dir=vignettes/intransitivity_data/work
mkdir -p "$log_dir"

wait_for_memory() {
  while :; do
    avail=$(awk '/MemAvailable/ {print int($2 / 1048576)}' /proc/meminfo)
    [ "$avail" -ge "$MIN_GB" ] && return
    echo "$(date +%T) waiting: ${avail} GB available, need ${MIN_GB}" >> "$log_dir/run.log"
    sleep 300
  done
}

run() {   # run <name> <stage> [family regex]
  wait_for_memory
  echo "$(date '+%F %T') start $1" >> "$log_dir/run.log"
  if ! IRW_STAGE="$2" IRW_FAMILY="${3:-}" nice -n 19 Rscript vignettes/intransitivity_compute.R > "$log_dir/run_$1.log" 2>&1; then
    echo "$(date '+%F %T') FAILED $1 (see run_$1.log)" >> "$log_dir/run.log"; exit 1
  fi
  echo "$(date '+%F %T') done $1" >> "$log_dir/run.log"
}

case "${1:-}" in
  units)
    run units units ;;
  tests)
    run team     tests '^(MLB|NBA|NHL|NFL|Football|US college|Cricket|Rugby)'
    run animal   tests '^Animal'
    run judg     tests '^Judgments'
    run onevone  tests '^(Lichess|Engines|UFC|Quiz)'
    run calib    calib
    wait_for_memory
    echo "$(date '+%F %T') start extras" >> "$log_dir/run.log"
    nice -n 19 Rscript vignettes/intransitivity_extras.R > "$log_dir/run_extras.log" 2>&1 ||
      { echo "$(date '+%F %T') FAILED extras (see run_extras.log)" >> "$log_dir/run.log"; exit 1; }
    nice -n 19 Rscript vignettes/vignette_versions_compute.R > "$log_dir/run_versions.log" 2>&1 ||
      { echo "$(date '+%F %T') FAILED versions (see run_versions.log)" >> "$log_dir/run.log"; exit 1; }
    echo "$(date '+%F %T') all done" >> "$log_dir/run.log" ;;
  *)
    echo "usage: $0 units|tests" >&2; exit 2 ;;
esac
