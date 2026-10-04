# Publishing this repository to GitHub

Target repository:

- HTTPS: https://github.com/srudkin12/Red-Wall
- SSH: `git@github.com:srudkin12/Red-Wall.git`
- default branch: `main`

The SSH key itself is **not** stored in this repository. Authentication is
performed by the user's local SSH agent/configuration.

Before publishing, run:

```bash
python3 scripts/static_repo_audit.py
bash scripts/prepublication_check.sh
```

Then publish with:

```bash
bash scripts/publish_to_github.sh
```

The publication script is deliberately conservative. It will:

1. refuse to continue if the static repository audit fails;
2. initialise Git locally if necessary;
3. create or rename the current branch to `main`;
4. configure `origin` as `git@github.com:srudkin12/Red-Wall.git`;
5. refuse to overwrite a non-empty remote repository;
6. stage only files permitted by `.gitignore`;
7. create the initial commit if required;
8. push `main` to GitHub.

If the remote repository already contains an initial GitHub-generated README,
licence or `.gitignore`, remove those files from the remote repository first or
reconcile them manually rather than forcing the push.

## SSH check

On the Linux workstation, the following command should identify the account
associated with the configured SSH key:

```bash
ssh -T git@github.com
```

GitHub normally prints an authentication-success message but does not provide
shell access, so a non-zero exit status from that command alone is not evidence
of failure.

Do **not** paste or upload the private SSH key into ChatGPT or the repository.
