import { AeSdk, Node, MemoryAccount } from '@aeternity/aepp-sdk';

const ignoreVersion = process.env.FORCE_COMPATIBILITY === "true";

console.log("Starting JS SDK smoke test");
const node = new Node("http://localhost:3013", { ignoreVersion });

const sdk = new AeSdk({
    nodes: [{ name: "ae_sdk_smoke_test", instance: node }],
    accounts: [
        new MemoryAccount("e6a91d633c77cf5771329d3354b3bcef1bc5e032c43d70b6d35af923ce1eb74dcea7ade470c9f99d9d4e400880a86f1d49bb444b62f11a9ebb64bbcfeb73fef3"),
    ],
});

const balance = await sdk.getBalance(sdk.address);
if (balance !== "10000000000000000000000000000000") {
    throw new Error(`Unexpected balance: ${balance}`);
};

await sdk.spend(1e18, MemoryAccount.generate().address);
