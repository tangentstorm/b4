#!/usr/bin/env python3
"""Run the shared B4 suite plus explicit Bend contracts through tanco.

Normalize the shared document's mixed TEST headlines / legacy #+name blocks
so tanco 0.4 actually runs both. Bend tests with the same name replace the
legacy expectation (strict address faults); every replacement is reported.
"""
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parent.parent


def blocks(path):
    tests = {}
    name = None
    body = None
    for line in path.read_text().splitlines():
        if body is not None:
            if line.lower().startswith("#+end_src"):
                if name:
                    if name in tests:
                        raise ValueError(f"duplicate test {name} in {path}")
                    tests[name] = "\n".join(body)
                body = None
                name = None
            elif not line.lstrip().startswith(("=", ":")):
                # Legacy v0.1 title/description lines are not expected output.
                body.append(line)
        elif line.startswith("*"):
            match = re.search(r"\bTEST\s+(\S+)", line)
            name = match[1] if match else None
        elif line.lower().startswith("#+name:"):
            name = line.split(":", 1)[1].strip()
        elif line.lower().startswith("#+begin_src b4a"):
            body = []
    if body is not None:
        raise ValueError(f"unterminated block in {path}")
    return tests


def main():
    shared = blocks(ROOT / "b4-tests.org")
    extra = blocks(ROOT / "bend/tests.org")
    overrides = shared.keys() & extra.keys()
    expected = {"op.jm", "op.cl", "op.hp.forward-wrap", "op.hp.backward-oob"}
    if overrides != expected:
        raise ValueError(f"unexpected compatibility overrides: {overrides ^ expected}")
    print(f"Shared suite: {len(shared)} tests; strict-address replacements: "
          + ", ".join(sorted(overrides)), flush=True)
    tests = shared | extra
    text = "#+title: B4 Bend acceptance tests\n#+tanco-format: 0.2\n\n"
    for name, body in tests.items():
        # Legacy io./q is not a legal tanco 0.2 name.
        name = name.replace("/", "-")
        text += f"* TEST {name}\n#+begin_src b4a\n{body}\n#+end_src\n\n"
    with tempfile.TemporaryDirectory(prefix="b4-bend-tests-") as tmp:
        suite = Path(tmp) / "tests.org"
        suite.write_text(text)
        env = os.environ.copy()
        env.setdefault("TANCO_SDB_PATH", str(Path(tmp) / "tanco.sdb"))
        command = [env.get("TANCO", "tanco"), "run", "-t", str(suite),
                   "--", str(ROOT / "bend/b4i")]
        result = subprocess.run(command, cwd=ROOT, env=env, text=True,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        print(result.stdout, end="")
        # tanco 0.4's CLI returns zero even after some test failures.
        passed = re.search(r"^All (\d+) tests passed\.$", result.stdout, re.M)
        return 0 if result.returncode == 0 and passed and int(passed[1]) == len(tests) else 1


if __name__ == "__main__":
    sys.exit(main())
