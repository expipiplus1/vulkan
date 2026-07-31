#!/usr/bin/env bash
# Syncval watchdog over the multi-queue headless example.
#
# QFOT mistakes surface as validation hazards or as deadlocks (a missed
# release/acquire half wedges the device), so the run is killed after
# $WATCHDOG seconds (default 300) and the timeout reported as a failure.
# Point VULKAN_SDK elsewhere to use another layer build.
set -u

here=$(cd "$(dirname "$0")" && pwd)
sdk=${VULKAN_SDK:-$HOME/VulkanSDK}
bin=$(cd "$here" && stack path --local-install-root)/bin/visibility-buffer
watchdog=${WATCHDOG:-300}
log=$(mktemp)
trap 'rm -f "$log"' EXIT

# A missing layer would pass vacuously: refuse to run without it.
if [ ! -f "$sdk/share/vulkan/explicit_layer.d/VkLayer_khronos_validation.json" ]; then
  echo "no validation layer under $sdk (set VULKAN_SDK)"
  exit 2
fi
if [ ! -x "$bin" ]; then
  echo "no binary at $bin (stack build vulkan-examples:exe:visibility-buffer)"
  exit 2
fi

# -k: a run wedged inside the driver ignores TERM; only KILL gets it out.
VK_LAYER_PATH="$sdk/share/vulkan/explicit_layer.d" \
LD_LIBRARY_PATH="$sdk/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
VK_INSTANCE_LAYERS=VK_LAYER_KHRONOS_validation \
VK_LAYER_ENABLES=VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT \
timeout -k 10 "$watchdog" "$bin" --headless >"$log" 2>&1
status=$?

if [ "$status" -eq 124 ] || [ "$status" -eq 137 ]; then
  echo "WATCHDOG: run wedged after ${watchdog}s; tail:"
  tail -20 "$log"
  exit 1
fi
if [ "$status" -ne 0 ]; then
  echo "run failed with status $status; tail:"
  tail -20 "$log"
  exit "$status"
fi
hazards=$(grep -E 'SYNC-HAZARD|VUID|[Vv]alidation [Ee]rror' "$log" || true)
if [ -n "$hazards" ]; then
  echo "validation messages:"
  printf '%s\n' "$hazards" | sed -E 's/(MessageID[^|]*).*/\1/' | sort | uniq -c
  exit 1
fi
if ! grep -q "All visibility-buffer checks passed" "$log"; then
  echo "checks line missing; tail:"
  tail -20 "$log"
  exit 1
fi
echo "syncval clean: $(grep -c '^' "$log") log lines, checks passed"
