* AENS pointers are now limited, this is enforced when updating a name:
  - No duplicate pointer keys.
  - Pointer keys are not longer than 256 bytes.
  - A name can not have more than 32 pointers.
  When a name is updated, or looked up, inside a Sophia contract keys
  that are no longer valid are automatically removed.
