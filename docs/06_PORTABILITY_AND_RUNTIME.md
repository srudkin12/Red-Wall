# Portability and Runtime

## No absolute paths

Framework source must not contain machine-specific project paths.

Use environment variables or application configuration.

Recommended:

```bash
export TDABM_PORTABILITY_ROOT="/path/to/TDABM_Portability"
export TDABM_FRAMEWORK_ROOT="$TDABM_PORTABILITY_ROOT/framework"
```

## Workers

Worker counts are application/runtime configuration. Do not encode a workstation
or server core count in generic source.

## Dependency check

Run:

```bash
Rscript --vanilla tools/install_dependencies.R
```

only if package installation is required.

## Clean self-test

Run:

```bash
bash setup_tdabm_clean.sh
```

before activating an application on a new machine.

## Immutability

Application outputs belong outside `framework/`. A production runner should
hash immutable inputs and accepted canonical topology before and after execution.

A shell wrapper must fail closed: a failed stage cannot be followed by a PASS
metadata record merely because a later command returned exit code zero.
