#!/usr/bin/env bash
#
# intransitivity_progress.sh: how many analysis units have their LR result, overall
# and by family. Run from the site root, once or under watch:
#   watch -n 60 vignettes/intransitivity_progress.sh
# Units are not equal in cost: a team-season takes seconds, a Lichess month or a
# Kaggle competition 5-10 minutes, so the share of units done runs ahead of the
# share of time.

cd "$(dirname "$0")/.."
export LC_ALL=C
work=${IRW_WORK:-vignettes/intransitivity_data/work}
keys=$work/unit_keys.tsv
if [ ! -f "$work/units.rds" ]; then echo "no units.rds yet (the units stage is still running)"; exit 0; fi
# one line per unit: cache key, family (rebuilt whenever units.rds is newer)
if [ ! -f "$keys" ] || [ "$work/units.rds" -nt "$keys" ]; then
  Rscript -e 'w <- commandArgs(TRUE)[1]; m <- readRDS(file.path(w, "units.rds"))$meta
    k <- paste0(gsub("[^A-Za-z0-9]+", "_", m$unit), ".rds")
    write.table(data.frame(k, m$family), file.path(w, "unit_keys.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)' "$work" > /dev/null 2>&1
fi

bar() {   # bar <done> <total> <label>
  local w=40 n=$(( $1 * 40 / ($2 > 0 ? $2 : 1) ))
  printf "%-38s [%-${w}s] %5d / %-5d %3d%%\n" "$3" "$(printf '%*s' "$n" '' | tr ' ' '#')" "$1" "$2" $(( $1 * 100 / ($2 > 0 ? $2 : 1) ))
}

ls "$work/lr" 2> /dev/null | sort > "$work/.lr_done"
total=$(wc -l < "$keys")
done_n=$(cut -f1 "$keys" | sort | comm -12 - "$work/.lr_done" | wc -l)
bar "$done_n" "$total" "ALL UNITS"
echo
cut -f2 "$keys" | sort -u | while IFS= read -r fam; do
  t=$(awk -F'\t' -v f="$fam" '$2 == f' "$keys" | wc -l)
  d=$(awk -F'\t' -v f="$fam" '$2 == f {print $1}' "$keys" | sort | comm -12 - "$work/.lr_done" | wc -l)
  bar "$d" "$t" "$fam"
done
echo
tail -n 3 "$work/run.log" 2> /dev/null || true
