#!/usr/bin/env python3
"""Compare local uv.lock with upstream uv.lock for breaking changes."""
from __future__ import annotations
import sys, argparse, tomllib
from pathlib import Path

try:
    from packaging.version import Version
except Exception:
    Version = None

def load_lock(path: Path) -> dict:
    if not path.exists():
        raise FileNotFoundError(path)
    data = tomllib.loads(path.read_text(encoding="utf-8"))
    return {e["name"]: e["version"] for e in data.get("package", []) or [] if e.get("name") and e.get("version")}

def safe_parse(v: str):
    if Version:
        try: return Version(v)
        except: pass
    parts = []
    for s in str(v).split("."):
        try: parts.append(int(s))
        except: parts.append(s)
    return tuple(parts)

def major_bump(old: str, new: str) -> bool:
    if Version:
        try:
            o, n = Version(old).release or (), Version(new).release or ()
            return len(o) and len(n) and n[0] > o[0]
        except: pass
    try: return int(str(new).split(".")[0]) > int(str(old).split(".")[0])
    except: return False

def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("local", nargs="?", default="uv.lock")
    p.add_argument("upstream", nargs="?", default=".tmp_upstream_lock/uv.lock")
    args = p.parse_args(argv)
    try:
        up = load_lock(Path(args.upstream))
    except FileNotFoundError as e:
        print(f"ERROR: {e}"); return 1
    except Exception as e:
        print(f"ERROR: {e}"); return 1
    try:
        local = load_lock(Path(args.local)) if Path(args.local).exists() else {}
    except Exception as e:
        print(f"ERROR: {e}"); return 1
    added, upgraded, downgraded = [], [], []
    for name, upv in sorted(up.items()):
        lv = local.get(name)
        if lv is None: added.append((name, upv))
        elif upv != lv:
            try:
                if safe_parse(upv) > safe_parse(lv): upgraded.append((name, lv, upv))
                else: downgraded.append((name, lv, upv))
            except: upgraded.append((name, lv, upv))
    if not (added or upgraded or downgraded):
        print("No differences detected."); return 0
    if added:
        print("New packages:")
        for n, v in added: print(f"  + {n}: {v}")
    if upgraded:
        print("\nUpgraded:")
        for n, l, u in upgraded:
            print(f"  ↑ {n}: {l} -> {u}{' (MAJOR)' if major_bump(l, u) else ''}")
    if downgraded:
        print("\nDowngraded:")
        for n, l, u in downgraded: print(f"  ↓ {n}: {l} -> {u}")
    should_fail = bool(added or downgraded)
    if not should_fail:
        for _, l, u in upgraded:
            if major_bump(l, u): should_fail = True; break
    if should_fail:
        print("\n⚠️ Breaking changes detected."); return 2
    print("\n✅ All changes are non-breaking."); return 0

if __name__ == "__main__":
    raise SystemExit(main())
