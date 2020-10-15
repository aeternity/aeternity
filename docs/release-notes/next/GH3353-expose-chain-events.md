* Added support for reporting chain events during contract evaluation. These events
  are published internally as 'tx_events' (available e.g. for plugins), and can also
  be fetched via the HTTP 'dry-run' method. The events are presented as 'dummy'
  transactions, unsigned, reflecting the details of the respective events.
