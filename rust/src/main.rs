#![feature(let_chains)]

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
use evm::{evm, Block, Log, State, Tx};
use primitive_types::U256;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Evmtest {
    name: String,
    hint: String,

    block: Option<Block>,
    tx: Option<Tx>,
    state: Option<State>,

    code: Code,
    expect: Expect,
}

#[derive(Debug, Deserialize)]
struct Code {
    asm: String,
    bin: String,
}

#[derive(Debug, Deserialize)]
struct Expect {
    stack: Option<Vec<String>>,
    success: bool,
    logs: Option<Vec<Log>>,
    #[serde(rename = "return")]
    ret: Option<String>,
}

fn main() {
    let text = std::fs::read_to_string("../evm.json").unwrap();
    let mut data: Vec<Evmtest> = serde_json::from_str(&text).unwrap();

    let total = data.len();

    for (index, test) in data.iter_mut().enumerate() {
        println!("Test {} of {}: {}", index + 1, total, test.name);

        let code: Vec<u8> = hex::decode(&test.code.bin).unwrap();

        let result = evm(
            &code,
            test.tx.as_ref(),
            test.block.as_ref(),
            test.state.as_mut(),
            true,
        );

        let expected_stack: Vec<U256> = test
            .expect
            .stack
            .as_ref()
            .and_then(|s| s.iter().map(|v| U256::from_str_radix(v, 16).ok()).collect())
            .unwrap_or_else(|| Vec::new());

        let mut matching = result.stack == expected_stack;

        matching = matching && result.success == test.expect.success;

        if matching && let Some(expected_logs) = test.expect.logs.as_ref() {
            for ( i, expected_log ) in expected_logs.into_iter().enumerate() {
                let log = &result.logs[i];
                if expected_log.address != log.address
                    || expected_log.data != log.data
                    || expected_log.topics != log.topics {
                        matching = false;
                        break;
                    }
            }
        }

        if matching && let Some(ret) = test.expect.ret.as_ref() {
            matching = result.ret == *ret;
        }

        if !matching {
            println!("Instructions: \n{}\n", test.code.asm);

            println!("Expected success: {:?}", test.expect.success);
            if let Some(ret) = test.expect.ret.as_ref() {
                println!("Expected return: {:?}", ret);
            }
            println!("Expected stack: [");
            for v in expected_stack {
                println!("  {:#X},", v);
            }
            println!("]\n");

            println!("Actual success: {:?}", result.success);
            if test.expect.ret.is_some() {
                println!("Expected return: {:?}", result.ret);
            }
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
