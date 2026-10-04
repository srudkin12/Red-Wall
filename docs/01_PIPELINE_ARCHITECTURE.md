# TDABM Portable Pipeline Architecture

## Separation of responsibilities

### Framework
Reusable analytical functions, validation, topology construction boundaries,
robustness machinery, evidence generation and provenance.

### Application adapter
Source recovery, variable definitions, sample construction, labels, colour
variables, exclusions, user radius record and application outputs.

### Paper conversation
Substantive interpretation, literature, journal positioning, captions and
claims. It consumes accepted analytical evidence rather than rebuilding it.

## Current stage sequence

### A — source/input freeze
Reconstruct and freeze the declared analytical input.

### B — topology-only radius diagnostics
Generate non-binding topology evidence across a broad radius range and repeated
landmark orders. User approval remains external.

### C — canonical topology
Freeze one approved topology exactly once and make downstream topology
read-only.

### D — evidence/robustness
Generate canonical paper evidence, radius sensitivity and landmark-order
interpretation robustness without modifying the accepted topology.

## Boundary rule

Each stage consumes accepted outputs from the previous stage. No downstream
stage is allowed to repair an upstream analytical choice silently.
