* The config setting `chain:db_direct_access: true` is made the new default.
  This will provide much greater consistency protection. To be enabled, the database
  must be on the new format (column families). If the db hasn't yet been migrated to,
  or created from scratch in the new format, `db_direct_access` will NOT be enabled,
  and a warning message will be produced, prompting for this problem to be addressed.
