#!/usr/bin/env bash

set -euo pipefail

readonly REMOTE_USER="johannes"
readonly REMOTE_HOST="method.hsw.tu-chemnitz.de"
readonly REMOTE="${REMOTE_USER}@${REMOTE_HOST}"
readonly REMOTE_CONFIG="/etc/shinyproxy/application.yml"

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_DIR=$(cd -- "${SCRIPT_DIR}/.." && pwd)
LOCAL_CONFIG=${1:-"${REPO_DIR}/application.yml"}
REMOTE_TMP="/tmp/application.yml.upload.${REMOTE_USER}.$$"

if [[ ! -s "${LOCAL_CONFIG}" ]]; then
  echo "Local configuration is missing or empty: ${LOCAL_CONFIG}" >&2
  exit 1
fi

cleanup() {
  ssh "${REMOTE}" "rm -f -- '${REMOTE_TMP}'" >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "Uploading ${LOCAL_CONFIG} to a temporary file on ${REMOTE_HOST}..."
scp "${LOCAL_CONFIG}" "${REMOTE}:${REMOTE_TMP}"

echo "Installing ${REMOTE_CONFIG} (sudo may prompt on the server)..."
ssh -t "${REMOTE}" "
  set -eu
  backup='${REMOTE_CONFIG}.bak.'\$(date -u +%Y%m%dT%H%M%SZ)
  if sudo test -e '${REMOTE_CONFIG}'; then
    sudo cp -a -- '${REMOTE_CONFIG}' \"\${backup}\"
    sudo cp -- '${REMOTE_TMP}' '${REMOTE_CONFIG}'
    echo \"Saved server backup: \${backup}\"
  else
    sudo install -o root -g root -m 0644 '${REMOTE_TMP}' '${REMOTE_CONFIG}'
  fi
  rm -f -- '${REMOTE_TMP}'
"

echo "Uploaded ${LOCAL_CONFIG} to ${REMOTE}:${REMOTE_CONFIG}"
echo "ShinyProxy was not restarted. Restart it separately after validation."
