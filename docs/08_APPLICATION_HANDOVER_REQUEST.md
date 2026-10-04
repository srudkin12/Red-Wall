# Application Handover Request

The root-level file:

`APPLICATION_HANDOVER_REQUEST.md`

is the standard outbound request to send to any conversation that owns a new
application, dataset, historical TDABM example, or paper-specific analysis.

It is intentionally generic. Do not replace it with an old application-specific
request unless that request is required for historical recovery.

The application conversation should normally return:

1. a Markdown response describing the verified application contract; and
2. an evidence/source ZIP plus SHA-256 sidecar.

The portability conversation then uses those materials to create an
application adapter and decide whether execution begins at P1.4-A or can resume
from an accepted current artifact.
