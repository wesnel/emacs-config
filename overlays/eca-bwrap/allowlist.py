"""Allowlist and request log for the bubblewrap backend.

Emits the same JSONL shape as the Gondolin backend, so both tiers produce
one comparable audit trail. A blocked request is logged without a matching
response, which is how the Gondolin side records a refusal too.
"""

import json
import os
import time

from mitmproxy import http

# Empty means nothing is reachable, matching Gondolin.
ALLOWED = [h for h in os.environ.get("ECA_SANDBOX_ALLOW_HOSTS", "").split(",") if h]
LOG = os.environ.get("ECA_SANDBOX_LOG") or None


def _record(entry):
    if not LOG:
        return

    entry["at"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())

    try:
        with open(LOG, "a") as handle:
            handle.write(json.dumps(entry) + "\n")
    except OSError:
        # Observability is best-effort: a failed write must not take the
        # session down.
        pass


def _allowed(host):
    return any(host == entry or host.endswith("." + entry) for entry in ALLOWED)


def _refuse(flow):
    flow.metadata["eca_blocked"] = True
    flow.response = http.Response.make(403, b"blocked by eca-sandbox allowlist\n")


def http_connect(flow: http.HTTPFlow) -> None:
    # Refused before the tunnel opens, so a blocked HTTPS host is never
    # contacted. An allowed tunnel is logged per request inside it instead.
    if not _allowed(flow.request.pretty_host):
        _record(
            {
                "dir": "request",
                "method": "CONNECT",
                "url": f"{flow.request.pretty_host}:{flow.request.port}",
            }
        )
        _refuse(flow)


def request(flow: http.HTTPFlow) -> None:
    _record(
        {
            "dir": "request",
            "method": flow.request.method,
            "url": flow.request.pretty_url,
        }
    )

    if not _allowed(flow.request.pretty_host):
        _refuse(flow)


def response(flow: http.HTTPFlow) -> None:
    if flow.metadata.get("eca_blocked"):
        return

    _record(
        {
            "dir": "response",
            "status": flow.response.status_code,
            "url": flow.request.pretty_url,
        }
    )
