import { AeSdk, Node, MemoryAccount } from '@aeternity/aepp-sdk';

const ignoreVersion = process.env.FORCE_COMPATIBILITY === "true";

console.log("Starting JS SDK smoke test");
const node = new Node("http://localhost:3013", { ignoreVersion });

const sdk = new AeSdk({
    nodes: [{ name: "ae_sdk_smoke_test", instance: node }],
    accounts: [
        new MemoryAccount("sk_2kav6y7UyuhycCYgo8UepzjwgBQDa6e72jkJoRnSu6zR8UUzJx"),
    ],
});

const balance = await sdk.getBalance(sdk.address);
if (balance !== "10000000000000000000000000000000") {
    throw new Error(`Unexpected balance: ${balance}`);
};

console.log("JS SDK smoke test passed");
