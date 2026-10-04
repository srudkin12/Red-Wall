# Canonical Topology Freeze and Recolouring

## Construction

After a valid explicit user-radius approval contract, including an explicitly justified radius below, inside or above the non-binding diagnostic review region:

1. sort observations deterministically by the stable ID;
2. construct Ball Mapper once;
3. normalize raw C++ landmark positions from zero-based to R one-based;
4. validate membership coverage and landmark indices;
5. freeze the R object and point-order sidecar;
6. export landmarks, memberships, edges and topology fingerprint;
7. hash all accepted artifacts.

## Recolouring

A recolouring may change only the colouring vector associated with balls.

It must not change:

- radius;
- point order;
- landmarks;
- memberships;
- edges;
- topology fingerprint.

Plotting and paper-output modules are read-only consumers of the frozen
topology.
