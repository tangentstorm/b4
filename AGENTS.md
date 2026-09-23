B4 is a small forth-like virtual machine that
runs atop many different software stacks.

We use 'tanco' to test. See the various test-xxx scripts
in the top level directory.

Current focus is implementing the machine in bend.

When using Bend:

- run `bend guide` to learn it
- use `LAWS.bend` to keep important rules
- run `bend PROOF.bend` before committing
- parallelize the code whenever possible

