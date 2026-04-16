#!/usr/bin/env python3
"""
Automatically sync dependencies from subproject pyproject.toml to root.
Ensures dependency changes in subprojects automatically propagate.

Usage:
    python scripts/sync_dependencies.py [--check]
    --check: Only verify sync, exit 1 if out of sync (for CI/pre-commit)
"""
from __future__ import annotations
import sys
import argparse
from pathlib import Path

try:
    import tomli
    import tomli_w
except ImportError:
    print("❌ Missing: pip install tomli tomli-w")
    sys.exit(1)


def load_toml(path: Path) -> dict:
    with open(path, "rb") as f:
        return tomli.load(f)


def save_toml(path: Path, data: dict):
    with open(path, "wb") as f:
        tomli_w.dump(data, f)


def sync_dependencies(source: Path, target: Path, check_only: bool = False) -> bool:
    """Sync deps from source to target. Returns True if already in sync."""
    source_data = load_toml(source)
    target_data = load_toml(target)

    changes_needed = False

    if "project" in source_data:
        for key in ["dependencies", "optional-dependencies"]:
            if key in source_data["project"]:
                if target_data.get("project", {}).get(key) != source_data["project"][key]:
                    changes_needed = True
                    if not check_only:
                        target_data.setdefault("project", {})[key] = source_data["project"][key]
                        print(f"✓ Synced [project.{key}]")

    if "tool" in source_data and "uv" in source_data["tool"]:
        if "extra-build-dependencies" in source_data["tool"]["uv"]:
            src_deps = source_data["tool"]["uv"]["extra-build-dependencies"]
            tgt_deps = target_data.get("tool", {}).get("uv", {}).get("extra-build-dependencies", {})
            if src_deps != tgt_deps:
                changes_needed = True
                if not check_only:
                    target_data.setdefault("tool", {}).setdefault("uv", {})["extra-build-dependencies"] = src_deps
                    print("✓ Synced [tool.uv.extra-build-dependencies]")

    if check_only:
        if changes_needed:
            print(f"❌ OUT OF SYNC: {source} vs {target}")
            return False
        print("✓ Dependencies in sync")
        return True

    if changes_needed:
        save_toml(target, target_data)
        print(f"\n✅ Synced: {source} → {target}")
        return False

    print("✓ No changes needed")
    return True


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="Check only, don't modify")
    parser.add_argument("--source", type=Path, default=Path("khoj-source/pyproject.toml"))
    parser.add_argument("--target", type=Path, default=Path("pyproject.toml"))
    args = parser.parse_args()

    if Path.cwd().name == "scripts":
        import os
        os.chdir("..")

    source = args.source.resolve()
    target = args.target.resolve()

    if not source.exists() or not target.exists():
        print(f"❌ File not found: {source if not source.exists() else target}")
        sys.exit(1)

    try:
        in_sync = sync_dependencies(source, target, check_only=args.check)
        sys.exit(0 if in_sync else 1)
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
