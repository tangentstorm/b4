# Shared helpers for imp/test-*. ROOT is the repository root.

ensure_tanco() {
  if [ -n "${TANCO:-}" ]; then
    return 0
  fi
  tanco="$ROOT/.venv/bin/tanco"
  if [ ! -x "$tanco" ]; then
    echo "Installing tanco into .venv"
    if [ ! -x "$ROOT/.venv/bin/python" ]; then
      python3 -m venv --without-pip "$ROOT/.venv"
    fi
    if ! "$ROOT/.venv/bin/python" -m pip --version >/dev/null 2>&1; then
      curl -fsSL https://bootstrap.pypa.io/get-pip.py | "$ROOT/.venv/bin/python"
    fi
    "$ROOT/.venv/bin/python" -m pip install tanco
  fi
  TANCO=$tanco
  export TANCO
}

# tanco 0.4 exits 0 when a test fails, so require its success line.
# Usage: tanco_run ORG [-- cmd args... | -c "shell command"]
tanco_run() {
  org=$1
  shift
  log=$(mktemp)
  echo "== $org =="
  "$TANCO" run -t "$org" "$@" | tee "$log"
  if grep -q '^All .* tests passed\.$' "$log"; then
    rm -f "$log"
  else
    rm -f "$log"
    return 1
  fi
}
