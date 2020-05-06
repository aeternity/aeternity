* Bugfix: Under specific circumstances the store of a contract could be damaged
    by updating values in the wrong place. A transaction experiencing this would
    not be valid for peers which are not affected by this bug. The fix ensures the store
    update is correct.

    This bug affects nodes running v5.5.x, therefore we advise to upgrade these
    nodes to prevent any issues.
