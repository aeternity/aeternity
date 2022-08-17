const { AeSdk, Node, MemoryAccount } = require('@aeternity/aepp-sdk')
const ignoreVersion = process.env.FORCE_COMPATIBILITY || true

const f = async () => {
    console.log("Starting JS SDK smoke test")
    const node = new Node("http://localhost:3013", { ignoreVersion });

    const sdk = new AeSdk({
        nodes: [{name: "ae_sdk_smoke_test", instance: node}]
    });
    await sdk.addAccount(
        new MemoryAccount({
            keypair: {
                publicKey: "ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi",
                secretKey: "e6a91d633c77cf5771329d3354b3bcef1bc5e032c43d70b6d35af923ce1eb74dcea7ade470c9f99d9d4e400880a86f1d49bb444b62f11a9ebb64bbcfeb73fef3",
            }
        })
    );

    const balance = await sdk.getBalance("ak_2a1j2Mk9YSmC1gioUq4PWRm3bsv887MbuRVwyv4KaUGoR1eiKi")
    console.log("Patron has " + balance + " aettos")
    if(balance === "10000000000000000000000000000000") {
        console.log("JS SDK smoke test passed");
        process.exit()
    } else {
        console.log("JS SDK smoke test failed");
        process.exit(1);
    }
}
f()
