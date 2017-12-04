
unalias gc
gc() { git commit --date="`date -R -d 08:00`" "$@"; }

export PYTHONPATH=py:gen-py:$PYTHONPATH
