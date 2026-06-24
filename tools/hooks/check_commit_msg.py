#!/usr/bin/env python3
"""commit-msg hook: forbid AI ``Co-Authored-By`` trailers (SportsDataverse-wide policy).

The human author is the sole attributable contributor on SDV commits/PRs, so any
``Co-Authored-By:`` trailer naming an AI tool is rejected. Reads the commit message file
(``argv[1]``); a non-zero exit blocks the commit.

Conventional-commit formatting is intentionally NOT enforced here — the automated
scraper/processor commits (``WNBA Play-by-Play and Schedules update (Start: ... End: ...)``)
are not conventional and must continue to pass. (sdv-py's variant additionally enforces
Conventional Commits.)
"""

from __future__ import annotations

import re
import sys

AI = re.compile(
    r"co-authored-by:.*\b(claude|copilot|cursor|gpt|chatgpt|gemini|anthropic|openai)\b",
    re.I,
)


def main() -> int:
    path = sys.argv[1] if len(sys.argv) > 1 else ".git/COMMIT_EDITMSG"
    try:
        with open(path, encoding="utf-8") as fh:
            msg = fh.read()
    except OSError as exc:
        print(f"commit-msg hook: cannot read {path}: {exc}")
        return 0  # never block on an infra failure

    bad = [ln.strip() for ln in msg.splitlines() if AI.search(ln)]
    if bad:
        print("commit-msg blocked: AI co-author trailer is forbidden (SportsDataverse policy):")
        for ln in bad:
            print("  - " + repr(ln))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
