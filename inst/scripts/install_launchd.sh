#!/bin/bash
# =============================================================================
# Install SISCORAR Daily Update launchd agent
#
# This script installs the launchd agent for running daily GRIB updates.
# Run from the rsiscorar package directory.
# =============================================================================

set -e

PLIST_NAME="com.siscorar.daily-update.plist"
PLIST_SOURCE="inst/launchd/${PLIST_NAME}"
PLIST_DEST="${HOME}/Library/LaunchAgents/${PLIST_NAME}"
LOG_FILE="${HOME}/Library/Logs/siscorar-daily-update.log"

echo "SISCORAR Daily Update - launchd Installation"
echo "============================================="
echo

# Check we're in the right directory
if [ ! -f "${PLIST_SOURCE}" ]; then
    echo "Error: Cannot find ${PLIST_SOURCE}"
    echo "Please run this script from the rsiscorar package directory."
    exit 1
fi

# Create LaunchAgents directory if needed
mkdir -p "${HOME}/Library/LaunchAgents"

# Unload existing agent if present
if launchctl list | grep -q "com.siscorar.daily-update"; then
    echo "Unloading existing agent..."
    launchctl unload "${PLIST_DEST}" 2>/dev/null || true
fi

# Copy plist to LaunchAgents
echo "Installing plist to ${PLIST_DEST}..."
cp "${PLIST_SOURCE}" "${PLIST_DEST}"

# Ensure log directory exists
mkdir -p "$(dirname "${LOG_FILE}")"

# Load the agent
echo "Loading launchd agent..."
launchctl load "${PLIST_DEST}"

# Verify installation
echo
echo "Verification:"
if launchctl list | grep -q "com.siscorar.daily-update"; then
    echo "  [OK] Agent is loaded"
else
    echo "  [WARN] Agent may not be loaded correctly"
fi

echo
echo "Installation complete!"
echo
echo "The daily update will run at 6:00 AM each day."
echo "If the computer is asleep at that time, it will run when woken."
echo
echo "Useful commands:"
echo "  View logs:      tail -f ${LOG_FILE}"
echo "  Run now:        launchctl start com.siscorar.daily-update"
echo "  Check status:   launchctl list | grep siscorar"
echo "  Uninstall:      launchctl unload ${PLIST_DEST} && rm ${PLIST_DEST}"
echo
