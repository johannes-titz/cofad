#!/usr/bin/env bash

set -euo pipefail

readonly REMOTE_USER="johannes"
readonly REMOTE_HOST="method.hsw.tu-chemnitz.de"
readonly REMOTE="${REMOTE_USER}@${REMOTE_HOST}"
readonly REMOTE_CONFIG="/etc/shinyproxy/application.yml"

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
REPO_DIR=$(cd -- "${SCRIPT_DIR}/.." && pwd)
LOCAL_CONFIG=${1:-"${REPO_DIR}/application.yml"}
LOCAL_TMP=$(mktemp "${LOCAL_CONFIG}.download.XXXXXX")
REMOTE_TMP="/tmp/application.yml.download.${REMOTE_USER}.$$"

cleanup() {
  rm -f -- "${LOCAL_TMP}"
  ssh "${REMOTE}" "rm -f -- '${REMOTE_TMP}'" >/dev/null 2>&1 || true
}
trap cleanup EXIT

echo "Preparing ${REMOTE_CONFIG} for download (sudo may prompt on the server)..."
ssh -t "${REMOTE}" \
  "sudo install -o '${REMOTE_USER}' -m 0600 '${REMOTE_CONFIG}' '${REMOTE_TMP}'"

scp "${REMOTE}:${REMOTE_TMP}" "${LOCAL_TMP}"

if [[ ! -s "${LOCAL_TMP}" ]]; then
  echo "Downloaded configuration is empty; refusing to replace ${LOCAL_CONFIG}." >&2
  exit 1
fi

if [[ -f "${LOCAL_CONFIG}" ]] && cmp -s "${LOCAL_CONFIG}" "${LOCAL_TMP}"; then
  echo "No changes: ${LOCAL_CONFIG} already matches ${REMOTE_CONFIG}."
  exit 0
fi

if [[ -f "${LOCAL_CONFIG}" ]]; then
  LOCAL_BACKUP="${LOCAL_CONFIG}.bak.$(date -u +%Y%m%dT%H%M%SZ)"
  cp -p -- "${LOCAL_CONFIG}" "${LOCAL_BACKUP}"
  echo "Saved local backup: ${LOCAL_BACKUP}"
fi

mv -- "${LOCAL_TMP}" "${LOCAL_CONFIG}"
chmod 0600 "${LOCAL_CONFIG}"
echo "Downloaded ${REMOTE}:${REMOTE_CONFIG} to ${LOCAL_CONFIG}"
