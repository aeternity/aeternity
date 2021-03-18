* Improved garbage collector for all Fate contracts form Iris
* Fate contracts of different versions can call eachother (Fate1 can call Fate2 and vice-versa)
* Opcode availability and behaviour depends on VM version of the contract (Fate2 opcodes are available both when Fate2 contract is called directly and when called by another (possibly Fate1) contract)