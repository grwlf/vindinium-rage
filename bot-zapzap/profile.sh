
unalias gc
gc() { git commit --date="`date -R -d 08:00`" "$@"; }
