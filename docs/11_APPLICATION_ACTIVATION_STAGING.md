# Application Activation Staging

Application activation ZIPs, checksum sidecars and installer scripts are
installation media, not clean framework source. Keep them in the directory
containing `TDABM_Portability/`, not inside the framework root.

The clean baseline must validate before a real application is installed.
Synthetic self-tests must write only to temporary/external directories and the
complete self-test must leave the clean manifest unchanged.
