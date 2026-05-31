# Git hooks

Tracked git hooks for this repo. They are **not** active until you point git's
`core.hooksPath` at this directory (a one-time, per-clone step):

```sh
git config core.hooksPath tools/git-hooks
```

## `pre-push` — bump the version before pushing

Blocks a `git push` when the pushed commits change tracked files but the
`Version:` field in `DESCRIPTION` is **unchanged from what is on the remote**.
This encodes the convention: *update the version number before every push.*

- New branches (no remote tip) and pushes with no file changes are allowed.
- Standard escape hatch when you really mean it: `git push --no-verify`.

For the dev series, bump the fourth component (`0.0.0.9002` -> `0.0.0.9003`).
