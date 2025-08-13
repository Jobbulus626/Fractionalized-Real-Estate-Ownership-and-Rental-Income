// import { Clarinet, Tx, Chain, Account, types } from 'https://deno.land/x/clarinet@v1.0.0/index.ts';
// import { assertEquals } from 'https://deno.land/std@0.90.0/testing/asserts.ts';

// Clarinet.test({
//     name: "Can create listing and place bid",
//     async fn(chain: Chain, accounts: Map<string, Account>) {
//         const deployer = accounts.get('deployer')!;
//         const user1 = accounts.get('wallet_1')!;
//         const user2 = accounts.get('wallet_2')!;
        
//         let block = chain.mineBlock([
//             Tx.contractCall('rental', 'create-property', [
//                 types.ascii("Test Property"),
//                 types.uint(100),
//                 types.uint(1000)
//             ], deployer.address),
            
//             Tx.contractCall('rental', 'mint-share', [
//                 types.uint(1)
//             ], user1.address),
            
//             Tx.contractCall('marketplace', 'create-listing', [
//                 types.uint(1),
//                 types.uint(1),
//                 types.uint(500),
//                 types.uint(100)
//             ], user1.address),
            
//             Tx.contractCall('marketplace', 'place-bid', [
//                 types.uint(1),
//                 types.uint(450),
//                 types.uint(50)
//             ], user2.address)
//         ]);
        
//         assertEquals(block.receipts.length, 4);
//         block.receipts.forEach(receipt => receipt.result.expectOk());
//     }
// });
