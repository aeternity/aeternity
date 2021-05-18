* Introduced a new HTTP endpoint: `/dry-run`. It is part of the `external`
  interface and should be prefered over the existing `debug` endpoint. It
  comes with some protections for the node: all transactions/calls provided
  are limited to a total amount of `gas` that they can consume. There is a new
  setting in the config where the node operator can change this according to
  their needs, the default value is 6 000 000 gas. The new endpoint is
  disabled by default and can be enabled via the new API group `dry-run`.
