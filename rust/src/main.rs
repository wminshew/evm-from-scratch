/**
 * EVM From Scratch
 * Rust template
 *
 * To work on EVM From Scratch in Rust:
 *
 * - Install Rust: https://www.rust-lang.org/tools/install
 * - Edit `rust/lib.rs`
 * - Run `cd rust && cargo run` to run the tests
 *
 * Hint: most people who were trying to learn Rust and EVM at the same
 * gave up and switched to JavaScript, Python, or Go. If you are new
 * to Rust, implement EVM in another programming language first.
 */
use ethereum_types::U256;
use evm::{evm, Block, Code, Log, State, Tx};
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Evmtest {
    name: String,
    hint: String,

    #[serde(default)]
    block: Block,
    #[serde(default)]
    tx: Tx,
    #[serde(default)]
    state: State,

    code: Code,
    expect: Expect,
}

#[derive(Debug, Deserialize)]
struct Expect {
    success: bool,
    #[serde(default)]
    stack: Vec<String>,
    #[serde(default)]
    logs: Vec<Log>,
    #[serde(rename = "return")]
    #[serde(default)]
    ret: String,
}

fn main() {
    let text = std::fs::read_to_string("../evm.json").unwrap();
    let mut data: Vec<Evmtest> = serde_json::from_str(&text).unwrap();

    let total = data.len();

    for (index, test) in data.iter_mut().enumerate() {
        println!("Test {} of {}: {}", index + 1, total, test.name);

        let code: Vec<u8> = hex::decode(&test.code.bin).unwrap();

        let result = evm(&code, &test.tx, &test.block, &mut test.state, true);

        let expected_stack: Vec<U256> = test
            .expect
            .stack
            .iter()
            .map(|v| U256::from_str_radix(v, 16).unwrap())
            .collect();

        let matching = result.success == test.expect.success
            && result.stack == expected_stack
            && result.logs == test.expect.logs
            && result.ret == test.expect.ret;

        if !matching {
            println!("Instructions: \n{}\n", test.code.asm.as_ref().unwrap());

            println!("Expected success: {:?}", test.expect.success);
            println!("Expected return: {:?}", test.expect.ret);
            println!("Expected stack: [");
            for v in expected_stack {
                println!("  {:#X},", v);
            }
            println!("]\n");

            println!("Actual success: {:?}", result.success);
            println!("Expected return: {:?}", result.ret);
            println!("Actual stack: [");
            for v in result.stack {
                println!("  {:#X},", v);
            }
            println!("]\n");

            println!("\nHint: {}\n", test.hint);
            println!("Progress: {}/{}\n\n", index, total);
            panic!("Test failed");
        }
        println!("PASS");
    }
    println!("Congratulations!");
}
