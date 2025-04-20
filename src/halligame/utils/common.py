"""
Functions used by various modules.
Michael Daniels, 2025-04-20
"""

import subprocess

from psutil import process_iter


def ensure_epmd():
    """Ensure that the Erlang port-mapper daemon is running"""
    epmd_running = False
    for proc in process_iter(["pid", "name"]):
        if proc.info["name"] == "epmd":
            epmd_running = True
            break
    if not epmd_running:
        subprocess.Popen(["epmd", "-daemon"])


def whoami():
    """Returns the current user's username."""
    return (
        subprocess.run(["whoami"], capture_output=True).stdout.decode().strip()
    )
