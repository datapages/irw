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
# waits until at least MIN_GB (default 12) of memory is available, and each batch
# runs under a MAX_GB (default 12) hard memory cap where the platform supports it,
# so a batch that balloons mid-run is killed on its own instead of triggering a
# global OOM. Set MAX_GB=0 to disable the cap.

set -euo pipefail
cd "$(dirname "$0")/.."
export IRW_CORES="${IRW_CORES:-4}"
MIN_GB="${MIN_GB:-12}"
log_dir=vignettes/intransitivity_data/work
mkdir -p "$log_dir"

MAX_GB="${MAX_GB:-12}"

# Run "$@" under a hard memory ceiling so a runaway batch dies by itself. A
# global OOM is far worse than a failed batch: it can take systemd-oomd's
# dbus.service kill with it and end every terminal session on the machine.
# wait_for_memory only gates entry to a batch; this bounds a batch already
# running. Falls back to running the command unwrapped where there is no user
# cgroup delegation (CI containers, other platforms).
cap_available=
if [ "$MAX_GB" != "0" ] && command -v systemd-run > /dev/null 2>&1 &&
   systemd-run --user --scope --collect -q -p MemoryMax=64M -- true > /dev/null 2>&1; then
  cap_available=1
fi

capped_run() {
  if [ -n "$cap_available" ]; then
    systemd-run --user --scope --collect -q \
      -p MemoryAccounting=yes -p MemoryMax="${MAX_GB}G" -p MemorySwapMax=0 -- "$@"
  else
    "$@"
  fi
}

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
  if ! IRW_STAGE="$2" IRW_FAMILY="${3:-}" capped_run nice -n 19 Rscript vignettes/intransitivity_compute.R > "$log_dir/run_$1.log" 2>&1; then
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
    capped_run nice -n 19 Rscript vignettes/intransitivity_extras.R > "$log_dir/run_extras.log" 2>&1 ||
      { echo "$(date '+%F %T') FAILED extras (see run_extras.log)" >> "$log_dir/run.log"; exit 1; }
    capped_run nice -n 19 Rscript vignettes/vignette_versions_compute.R > "$log_dir/run_versions.log" 2>&1 ||
      { echo "$(date '+%F %T') FAILED versions (see run_versions.log)" >> "$log_dir/run.log"; exit 1; }
    echo "$(date '+%F %T') all done" >> "$log_dir/run.log" ;;
  *)
    echo "usage: $0 units|tests" >&2; exit 2 ;;
esac
