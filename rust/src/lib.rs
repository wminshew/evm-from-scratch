#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(string_remove_matches)]

// use serde::de;
// use serde::{Deserialize, Deserializer};
use serde::Deserialize;
use sha3::{Digest, Keccak256};
use std::cmp;
use std::collections::HashMap;
use std::ops::{Add, BitAnd, BitOr, BitXor, Not, Shl, Shr};

use primitive_types::U256;

// TODO: have different representations in main vs lib? strings for json vs .. U256 etc?
// TODO: add encapsulation for state, memory etc

#[derive(Debug, Deserialize)]
pub struct Block {
    basefee: Option<String>,
    coinbase: Option<String>,
    timestamp: Option<String>,
    number: Option<String>,
    difficulty: Option<String>,
    gaslimit: Option<String>,
    chainid: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct Tx {
    // #[serde(deserialize_with = "from_hex")]
    // to: Option<U256>,
    to: Option<String>,
    from: Option<String>,
    origin: Option<String>,
    gasprice: Option<String>,
    value: Option<String>,
    data: Option<String>,
}

// fn from_hex<'de, D>(deserializer: D) -> Result<Option<U256>, D::Error>
// where
//     D: Deserializer<'de>,
// {
//     println!("ahh im deserializiing");
//     let s: &str = Deserialize::deserialize(deserializer)?;
//     println!("s: {}", s);
//     U256::from_str_radix(s, 16)
//         .map(|u| Some(u))
//         .map_err(de::Error::custom)
//     // if let Ok(s) = Deserialize::deserialize(deserializer) {
//     //     println!("{}", s);
//     //     U256::from_str_radix(s, 16)
//     //         .map(|u| Some(u))
//     //         .map_err(de::Error::custom)
//     // } else {
//     //     println!("err");
//     //     Ok(None)
//     // }
// }

// TODO: type alias vs single field struct?

// #[derive(Debug, Deserialize)]
pub type State = HashMap<String, StateInfo>;

#[derive(Debug, Clone, Deserialize)]
pub struct StateInfo {
    balance: Option<String>,
    nonce: Option<String>,
    code: Option<Code>,
    storage: Option<Storage>,
}

type Storage = HashMap<String, String>;

#[derive(Debug, Clone, Deserialize)]
struct Code {
    // asm: Option<String>,
    bin: String,
}

pub struct EvmResult {
    pub logs: Vec<Log>,
    pub stack: Vec<U256>,
    pub success: bool,
    pub ret: String,
}

#[derive(Debug, Deserialize)]
pub struct Log {
    pub address: String,
    pub data: String,
    pub topics: Vec<String>,
}

// TODO: use opcode enum?
// TODO: use pre processing in main

pub fn evm(
    _code: impl AsRef<[u8]>,
    tx: Option<&Tx>,
    block: Option<&Block>,
    state: Option<&mut State>,
    writeable: bool,
) -> EvmResult {
    let mut stack: Vec<U256> = Vec::new();
    let mut pc = 0;
    let mut memory: Vec<u8> = Vec::new();
    let mut success = true;
    let mut logs: Vec<Log> = Vec::new();
    let mut ret = "".to_string();
    let mut last_return_data: Vec<u8> = Vec::new();

    let code = _code.as_ref();

    let mut init_state = HashMap::new();
    let mut state = match state {
        Some(state) => state,
        None => &mut init_state,
    };

    while pc < code.len() {
        let opcode = code[pc];
        pc += 1;

        match opcode {
            // STOP
            0x00 => {
                break;
            }
            // ADD
            0x01 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.overflowing_add(b).0);
            }
            // MUL
            0x02 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.overflowing_mul(b).0);
            }
            // SUB
            0x03 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.overflowing_sub(b).0);
            }
            // DIV
            0x04 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.checked_div(b).unwrap_or( U256::zero() ));
            }
            // SDIV
            // TODO: cleaner way to do this?
            0x05 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() {
                    stack.push(U256::zero());
                    continue;
                }

                // let a_neg = a.bit(255);
                // let b_neg = b.bit(255);
                // let neg = a_neg ^ b_neg;
                // let a = if a_neg { a.not() + 1 } else { a };
                // let b = if b_neg { b.not() + 1 } else { b };
                // let result = a / b;
                // let result = if neg { result.not() + 1 } else { result };
                // stack.push(result);

                // TODO: use fn for twos complement + neg bool
                let ( a_neg, a_twos ) = ( a.bit(255), a.not().add(1) );
                let ( b_neg, b_twos ) = ( b.bit(255), b.not().add(1) );
                let result = if a_neg { a_twos } else { a } / if b_neg { b_twos } else { b };
                stack.push(if a_neg ^ b_neg { result.not() + 1 } else { result });
            }
            // MOD
            0x06 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(if b.is_zero() { U256::zero() } else { a % b});
            }
            // SMOD
            // TODO: cleaner way to do this?
            0x07 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() | (b == U256::MAX) {
                    stack.push(U256::zero());
                    continue;
                }

                let a_neg = a.bit(255);
                let b_neg = b.bit(255);
                let neg = a_neg | b_neg;
                let a = if a_neg { a.not() + 1 } else { a };
                let b = if b_neg { b.not() + 1 } else { b };
                let result = a % b;
                let result = if neg { result.not() + 1 } else { result };
                stack.push(result);
            }
            // ADDMOD
            0x08 if let ( Some(a), Some(b), Some(n) ) = ( stack.pop(), stack.pop(), stack.pop() ) => {
                stack.push(if n.is_zero() { U256::zero() } else { a.overflowing_add(b).0 % n});
            }
            // MULMOD
            0x09 if let ( Some(a), Some(b), Some(n) ) = ( stack.pop(), stack.pop(), stack.pop() ) => {
                stack.push(if n.is_zero() { U256::zero() } else { ( a.full_mul(b) % n ).try_into().unwrap() });
            }
            // EXP
            0x0a if let ( Some(a), Some(exp) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.overflowing_pow(exp).0);
            }
            // SIGNEXTEND
            0x0b if let ( Some(b), Some(x) ) = ( stack.pop(), stack.pop() ) => {
                let idx = (b.as_usize() + 1) * 8;
                stack.push(if x.bit(idx - 1) { U256::MAX.shl(idx).bitor(x) } else { x });
            }
            // LT
            0x10 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(U256::from(a.lt(&b) as u32));
            }
            // GT
            0x11 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(U256::from(a.gt(&b) as u32));
            }
            // SLT
            0x12 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(U256::from(if a.bit(255) ^ b.bit(255) {
                    a.bit(255)
                } else {
                    a.lt(&b)
                } as u32));
            }
            // SGT
            0x13 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(U256::from(if a.bit(255) ^ b.bit(255) {
                    b.bit(255)
                } else {
                    a.gt(&b)
                } as u32));
            }
            // EQ
            0x14 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(U256::from(a.eq(&b) as u32));
            }
            // ISZERO
            0x15 if let Some(a) = stack.pop() => {
                stack.push(U256::from(a.is_zero() as u32));
            }
            // AND
            0x16 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.bitand(b));
            }
            // OR
            0x17 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.bitor(b));
            }
            // XOR
            0x18 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(a.bitxor(b));
            }
            // NOT
            0x19 if let Some(a) = stack.pop() => {
                stack.push(a.not());
            }
            // BYTE
            0x1a if let ( Some(i), Some(x) ) = ( stack.pop(), stack.pop() ) => {
                // TODO: use byte; so as_usize
                stack.push(
                    if let Ok(i) = usize::try_from(i) && i < 32 {
                        U256::from(<[u8; 32]>::from(x)[i])
                    } else {
                        U256::zero()
                    }
                );
            }
            // SHL
            0x1B if let ( Some(shift), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(value.shl(shift));
            }
            // SHR
            0x1C if let ( Some(shift), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(value.shr(shift));
            }
            // SAR
            0x1D if let ( Some(shift), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(
                    if value.bit(255) {
                        if shift.gt( &U256::from(255) ) {
                            U256::MAX
                        } else {
                            value.shr(shift).bitor(U256::MAX.shl(U256::from(255) - shift ))
                        }
                    } else {
                        value.shr(shift)
                    }
                );
            }
            // SHA3
            0x20 if let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                stack.push(U256::from(Keccak256::digest(&memory[offset..offset+size]).as_slice()));
            }
            // ADDRESS
            0x30 => {
                let to = tx.unwrap().to.as_ref().unwrap();
                stack.push(U256::from_str_radix(to, 16).unwrap());
            }
            // BALANCE
            0x31 if let Some(address) = stack.pop() => {
                stack.push(
                    // state
                    //     .and_then(|s| s.get(&format!("0x{:x}", address)))
                    state.get(&format!("0x{:x}", address))
                        .and_then(|addr| addr.balance.as_ref())
                        .and_then(|bal| U256::from_str_radix(bal, 16).ok())
                        .unwrap_or_else(|| U256::zero())
                )
            }
            // ORIGIN
            0x32 => {
                let origin = tx.unwrap().origin.as_ref().unwrap();
                stack.push(U256::from_str_radix(origin, 16).unwrap());
            }
            // CALLER
            0x33 => {
                let from = tx.unwrap().from.as_ref().unwrap();
                stack.push(U256::from_str_radix(from, 16).unwrap());
            }
            // CALLVALUE
            0x34 => {
                let value = tx.unwrap().value.as_ref().unwrap();
                stack.push(U256::from_str_radix(value, 16).unwrap());
            }
            // CALLDATALOAD
            0x35 if let Some(i) = stack.pop() => {
                println!("stack {:?}", stack);
                println!("i {:?}", i);
                let i = i.as_usize() * 2; // each char is 4-bits, a byte is 8-bits
                let data = tx.unwrap().data.as_ref().unwrap();
                let data_str_radix = format!("{:0<64}", &data[i..cmp::min(i+64,data.len())]);
                stack.push(U256::from_str_radix(&data_str_radix, 16).unwrap());
            }
            // CALLDATASIZE
            0x36 => {
                stack.push(
                    tx
                        .and_then(|t| t.data.as_ref())
                        .and_then(|d| Some(U256::from(d.len() / 2)))
                        .unwrap_or_else(|| U256::zero())
                );
            }
            // CALLDATACOPY
            0x37 if let ( Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                if let Some(cd) = tx
                    .and_then(|t| t.data.as_ref())
                    // each char is 4-bits, a byte is 8-bits
                    .and_then(|d| hex::decode(&d[offset*2..(offset+size)*2]).ok()) {
                        let dest_offset = dest_offset.as_usize();
                        let end = dest_offset + size;
                        resize(&mut memory, end);
                        memory.splice(dest_offset..end, cd);
                    }
            }
            // CODESIZE
            0x38 => {
                stack.push(
                    U256::from(code.len())
                );
            }
            // CODECOPY
            0x39 if let ( Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                let dest_offset = dest_offset.as_usize();

                let mut new_mem = vec![0; size];
                let copy_size = cmp::min(size, code.len() - offset);
                new_mem[..copy_size].copy_from_slice(&code[offset..offset+copy_size]);

                let end = dest_offset + size;
                resize(&mut memory, end);
                memory[dest_offset..end].copy_from_slice(&new_mem[..]);
            }
            // GASPRICE
            0x3a => {
                let gasprice = tx.unwrap().gasprice.as_ref().unwrap();
                stack.push(U256::from_str_radix(gasprice, 16).unwrap());
            }
            // EXTCODESIZE
            0x3b if let Some(address) = stack.pop() => {
                stack.push(
                    // state
                    //     .and_then(|s| s.get(&format!("0x{:x}", address)))
                    state.get(&format!("0x{:x}", address))
                        .and_then(|addr| addr.code.as_ref())
                        .map(|c| U256::from(c.bin.len() / 2))
                        .unwrap_or_else(|| U256::zero())
                )
            }
            // EXTCODECOPY
            0x3c if let ( Some(address), Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                let dest_offset = dest_offset.as_usize();

                // if let Some( ext_code ) = state
                //     .and_then(|s| s.get(&format!("0x{:x}", address)))
                if let Some( ext_code ) = state.get(&format!("0x{:x}", address))
                    .and_then(|addr| addr.code.as_ref())
                    .and_then(|c| hex::decode(&c.bin).ok()) {
                        let mut new_mem = vec![0; size];
                        let copy_size = cmp::min(size, ext_code.len() - offset);
                        new_mem[..copy_size].copy_from_slice(&ext_code[offset..offset+copy_size]);

                        let end = dest_offset + size;
                        resize(&mut memory, end);
                        memory[dest_offset..end].copy_from_slice(&new_mem[..]);
                    }
            }
            // RETURNDATASIZE
            0x3d => {
                stack.push(U256::from(last_return_data.len()));
            }
            // RETURNDATACOPY
            0x3e if let ( Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop()) => {
                let dest_offset = dest_offset.as_usize();
                let offset = offset.as_usize();
                let size = size.as_usize();

                let mut new_mem = vec![0; size];
                let copy_size = cmp::min(size, last_return_data.len() - offset);
                new_mem[..copy_size].copy_from_slice(&last_return_data[offset..offset+copy_size]);

                let end = dest_offset + size;
                resize(&mut memory, end);
                memory[dest_offset..end].copy_from_slice(&new_mem[..]);
            }
            // EXTCODEHASH
            0x3f if let Some(address) = stack.pop() => {
                stack.push(
                    // state
                    //     .and_then(|s| s.get(&format!("0x{:x}", address)))
                    state.get(&format!("0x{:x}", address))
                        .and_then(|addr| addr.code.as_ref())
                        .map(|c| U256::from(Keccak256::digest(&hex::decode(&c.bin).unwrap()).as_slice()))
                        .unwrap_or_else(|| U256::zero())
                )
            }
            // BLOCKHASH
            0x40 if let Some(_block_number) = stack.pop() => {
                stack.push(U256::zero());
            }
            // COINBASE
            0x41 => {
                let coinbase = block.unwrap().coinbase.as_ref().unwrap();
                stack.push(U256::from_str_radix(coinbase, 16).unwrap());
            }
            // TIMESTAMP
            0x42 => {
                let timestamp = block.unwrap().timestamp.as_ref().unwrap();
                stack.push(U256::from_str_radix(timestamp, 16).unwrap());
            }
            // NUMBER
            0x43 => {
                let number = block.unwrap().number.as_ref().unwrap();
                stack.push(U256::from_str_radix(number, 16).unwrap());
            }
            // DIFFICULTY
            0x44 => {
                let difficulty = block.unwrap().difficulty.as_ref().unwrap();
                stack.push(U256::from_str_radix(difficulty, 16).unwrap());
            }
            // GASLIMIT
            0x45 => {
                let gaslimit = block.unwrap().gaslimit.as_ref().unwrap();
                stack.push(U256::from_str_radix(gaslimit, 16).unwrap());
            }
            // CHAINID
            0x46 => {
                let chainid = block.unwrap().chainid.as_ref().unwrap();
                stack.push(U256::from_str_radix(chainid, 16).unwrap());
            }
            // SELFBALANCE
            0x47 => {
                let address = tx.unwrap().to.as_ref().unwrap();
                stack.push(
                    // state
                    //     .and_then(|s| s.get(address))
                    state.get(address)
                        .and_then(|addr| addr.balance.as_ref())
                        .and_then(|bal| U256::from_str_radix(bal, 16).ok())
                        .unwrap_or_else(|| U256::zero())
                )
            }
            // BASEFEE
            0x48 => {
                let basefee = block.unwrap().basefee.as_ref().unwrap();
                stack.push(U256::from_str_radix(basefee, 16).unwrap());
            }
            // POP
            0x50 => {
                stack.pop();
            }
            // MLOAD
            0x51 if let Some(offset) = stack.pop() => {
                let offset = offset.as_usize();
                let end = offset + 31;
                resize(&mut memory, end);
                stack.push(U256::from(&memory[offset..=end]));
            }
            // MSTORE
            0x52 if let ( Some(offset), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                let offset = offset.as_usize();
                let end = offset + 31;
                resize(&mut memory, end);
                memory.splice(offset..=end, <[u8; 32]>::from(value));
            }
            // MSTORE8
            0x53 if let ( Some(offset), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                let offset = offset.as_usize();
                resize(&mut memory, offset);
                memory[offset] = value.byte(0);
            }
            // SLOAD
            0x54 if let Some(key) = stack.pop() => {
                let address = tx.and_then(|t| t.to.as_ref()).map(|t| t.clone()).unwrap_or_else(|| "0x0".to_string());
                stack.push(
                    // state
                    //     .and_then(|s| s.get(&address))
                    state.get(&address)
                        .and_then(|s| s.storage.as_ref())
                        .and_then(|s| s.get(&format!("0x{:x}", key)))
                        .and_then(|s| U256::from_str_radix(s, 16).ok())
                        .unwrap_or_else(|| U256::zero())
                );
            }
            // SSTORE
            0x55 if writeable && let ( Some(key), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                let address = tx.and_then(|t| t.to.as_ref()).map(|t| t.clone()).unwrap_or_else(|| "0x0".to_string());

                let entry = state.entry(address).or_insert(StateInfo {
                    balance: None,
                    nonce: None,
                    code: None,
                    storage: None,
                });
                if entry.storage.is_none() {
                    entry.storage = Some(HashMap::new());
                    // *entry = StateInfo {
                    //     storage: Some(HashMap::new()),
                    //     ..*entry
                    // }
                }

                entry.storage.as_mut().unwrap().insert(format!("0x{:x}", key), format!("0x{:x}", value));
            }
            // JUMP
            0x56 if let Some(counter) = stack.pop() => {
                pc = counter.as_usize();
                if invalid_pc(pc, code) {
                    success = false;
                    break;
                }
            }
            // JUMPI
            0x57 if let ( Some(counter), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() {
                    continue;
                }

                pc = counter.as_usize();
                if invalid_pc(pc, code) {
                    success = false;
                    break;
                }
            }
            // PC
            0x58 => {
                stack.push(U256::from(pc - 1));
            }
            // MSIZE
            0x59 => {
                stack.push(U256::from(memory.len()));
            }
            // GAS
            0x5a => {
                stack.push(U256::MAX);
            }
            // JUMPDEST
            0x5b => {}
            // PUSHn
            0x60..=0x7f => {
                let push_size = opcode as usize - 0x60 + 1;
                stack.push(U256::from(&code[pc..pc + push_size]));
                pc += push_size;
            }
            // DUPn
            0x80..=0x8f => {
                let dup_dist = opcode as usize - 0x80 + 1;
                stack.push(stack[stack.len() - dup_dist]);
            }
            // SWAPn
            0x90..=0x9f => {
                let swap_dist = opcode as usize - 0x90 + 2;
                let len = stack.len();
                stack.swap(len - 1, len - swap_dist);
            }
            // LOGn
            0xa0..=0xa4 if writeable && let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                let address = tx.unwrap().to.as_ref().unwrap().clone();

                let offset = offset.as_usize();
                let size = size.as_usize();
                // TODO: resize memory to multiple of 32?
                // let end = offset + 31;
                // resize(&mut memory, end);
                let data = hex::encode(memory[offset..offset+size].to_vec());

                let log_dist = opcode as usize - 0xa0;
                let mut topics = Vec::new();
                for _ in 0..log_dist {
                    topics.push(format!("0x{:x}", stack.pop().unwrap()));
                }

                logs.push(Log {
                    address,
                    data,
                    topics,
                })
            }
            // CREATE
            0xf0 if writeable && let (
                Some(value),
                Some(offset),
                Some(size)
            ) = ( stack.pop(), stack.pop(), stack.pop() ) => {
                // prep call

                let offset = offset.as_usize();
                let size = size.as_usize();

                // TODO: resize memory to multiple of 32?
                // let end = offset + 31;
                // resize(&mut memory, end);

                // TODO: address = keccak256(rlp([sender_address,sender_nonce]))[12:]
                let address = format!("0x{}", &hex::encode(
                    Keccak256::digest(&memory[offset..offset+size]).as_slice()
                )[24..]);

                let init_code = &memory[offset..offset+size];

                let tx = tx.map(|t| t.clone()).unwrap_or_else(|| Tx {
                    to: None,
                    from: None,
                    data: None,
                    value: None,
                    gasprice: None,
                    origin: None,
                });
                let tx = Tx {
                    to: Some(address.clone()),
                    from: tx.to.as_ref().map(|t| t.clone()),
                    // data: Some(init_code.clone()),
                    data: Some(hex::encode(&init_code)),
                    value: Some(format!("0x{:x}", value)),
                    ..tx
                };

                // call
                let result = evm(
                    init_code,
                    Some(&tx),
                    block,
                    Some( &mut state ),
                    true,
                );

                last_return_data = hex::decode(result.ret.clone()).unwrap();

                if result.success {
                    stack.push(U256::from_str_radix(&address[..], 16).unwrap());
                    // TODO: def not how you should be setting balance..
                    state.insert(address, StateInfo {
                        balance: Some(format!("0x{:x}", value)),
                        nonce: Some("0".to_string()),
                        code: Some(Code {
                            bin: result.ret.clone(),
                        }),
                        storage: None,
                    });
                } else {
                    stack.push(U256::zero());
                }
            }
            // CALL
            0xf1 if writeable && let (
                Some(_gas),
                Some(address),
                Some(value),
                Some(args_offset),
                Some(args_size),
                Some(ret_offset),
                Some(ret_size) ) = ( stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop() ) => {
                // prep call

                let args_size = args_size.as_usize();
                let args_offset = args_offset.as_usize();
                let end = args_offset + args_size;

                let tx = tx.map(|t| t.clone()).unwrap_or_else(|| Tx {
                    to: None,
                    from: None,
                    data: None,
                    value: None,
                    gasprice: None,
                    origin: None,
                });
                let tx = Tx {
                    to: Some(format!("0x{:x}", address)),
                    from: tx.to.as_ref().map(|t| t.clone()),
                    data: Some(hex::encode(&memory[args_offset..end])),
                    value: Some(format!("0x{:x}", value)),
                    ..tx
                };

                let _code = state.get(&format!("0x{:x}", address))
                    .and_then(|addr| addr.code.as_ref())
                    .and_then(|c| hex::decode(&c.bin).ok())
                    .unwrap();

                // call
                let result = evm(
                    _code,
                    Some(&tx),
                    block,
                    Some( &mut state ),
                    true,
                );

                // store result to mem
                last_return_data = hex::decode(result.ret).unwrap();

                let ret_size = ret_size.as_usize();
                let ret_offset = ret_offset.as_usize();
                let end = ret_offset + ret_size;

                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                resize(&mut memory, end);
                memory[ret_offset..end].copy_from_slice(&new_mem[..]);

                stack.push(U256::from(result.success as u8))
            }
            // RETURN
            0xf3 if let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                // TODO: resize memory to multiple of 32?
                // let end = offset + 31;
                // resize(&mut memory, end);
                ret = hex::encode(memory[offset..offset+size].to_vec());
                break;
            }
            // DELEGATECALL
            0xf4 if let (
                Some(_gas),
                Some(address),
                Some(args_offset),
                Some(args_size),
                Some(ret_offset),
                Some(ret_size) ) = ( stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop() ) => {
                // prep call
                let args_size = args_size.as_usize();
                let args_offset = args_offset.as_usize();
                let end = args_offset + args_size;

                let tx = tx.map(|t| t.clone()).unwrap_or_else(|| Tx {
                    to: None,
                    from: None,
                    data: None,
                    value: None,
                    gasprice: None,
                    origin: None,
                });
                let tx = Tx {
                    data: Some(hex::encode(&memory[args_offset..end])),
                    ..tx
                };

                let _code = state.get(&format!("0x{:x}", address))
                    .and_then(|addr| addr.code.as_ref())
                    .and_then(|c| hex::decode(&c.bin).ok())
                    .unwrap();

                // call
                let result = evm(
                    _code,
                    Some(&tx),
                    block,
                    Some(&mut state),
                    true,
                );

                // store result to mem
                last_return_data = hex::decode(result.ret).unwrap();

                let ret_size = ret_size.as_usize();
                let ret_offset = ret_offset.as_usize();
                let end = ret_offset + ret_size;

                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                resize(&mut memory, end);
                memory[ret_offset..end].copy_from_slice(&new_mem[..]);

                stack.push(U256::from(result.success as u8))
            }
            // STATICCALL
            0xfa if let (
                Some(_gas),
                Some(address),
                Some(args_offset),
                Some(args_size),
                Some(ret_offset),
                Some(ret_size) ) = ( stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop(), stack.pop() ) => {
                // prep call
                let args_size = args_size.as_usize();
                let args_offset = args_offset.as_usize();
                let end = args_offset + args_size;

                let tx = tx.map(|t| t.clone()).unwrap_or_else(|| Tx {
                    to: None,
                    from: None,
                    data: None,
                    value: None,
                    gasprice: None,
                    origin: None,
                });
                let tx = Tx {
                    to: Some(format!("0x{:x}", address)),
                    from: tx.to.as_ref().map(|t| t.clone()),
                    data: Some(hex::encode(&memory[args_offset..end])),
                    ..tx
                };

                let _code = state.get(&format!("0x{:x}", address))
                    .and_then(|addr| addr.code.as_ref())
                    .and_then(|c| hex::decode(&c.bin).ok())
                    .unwrap();

                // call
                let result = evm(
                    _code,
                    Some(&tx),
                    block,
                    Some(&mut state),
                    false,
                );

                // store result to mem
                last_return_data = hex::decode(result.ret).unwrap();

                let ret_size = ret_size.as_usize();
                let ret_offset = ret_offset.as_usize();
                let end = ret_offset + ret_size;

                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                resize(&mut memory, end);
                memory[ret_offset..end].copy_from_slice(&new_mem[..]);

                stack.push(U256::from(result.success as u8))
            }
            // REVERT
            0xfd if let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                let offset = offset.as_usize();
                let size = size.as_usize();
                // TODO: resize memory to multiple of 32?
                // let end = offset + 31;
                // resize(&mut memory, end);
                ret = hex::encode(memory[offset..offset+size].to_vec());
                success = false;
                break;
            }
            // INVALID
            0xfe => {
                success = false;
                break;
            }
            // SELFDESTRUCT
            0xff if let Some(to_address) = stack.pop() => {
                let my_address = tx.unwrap().to.as_ref().unwrap().clone();
                let my_state_info = state.get(&my_address).unwrap().clone();
                let my_balance = U256::from_str_radix(my_state_info.balance.as_ref().unwrap(), 16).unwrap();

                let old_to_balance = state.get(&format!("0x{:x}", &to_address))
                    .and_then(|si| si.balance.as_ref())
                    .and_then(|bal| U256::from_str_radix(bal, 16).ok())
                    .unwrap_or_else(|| U256::zero());
                let new_to_balance = old_to_balance.add(&my_balance);
                let new_to_balance_str = format!("0x{:x}", new_to_balance);

                state.entry(format!("0x{:x}", &to_address))
                    .and_modify(|e| e.balance = Some(new_to_balance_str.clone()))
                    .or_insert(StateInfo {
                        balance: Some(new_to_balance_str),
                        nonce: None,
                        code: None,
                        storage: None,
                    });

                state.insert(my_address.clone(), StateInfo {
                    balance: Some("0x0".to_string()),
                    code: None,
                    storage: None,
                    ..my_state_info
                } );
            }
            _ => {
                // revert on non-match (e.g. trying to write in STATICCALL)
                success = false;
                break;
            }
        };
    }

    stack.reverse();

    return EvmResult {
        logs,
        stack,
        success,
        ret,
    };
}

// TODO: use encapsulation / special type w custom impls for pc/memory ?

fn invalid_pc(pc: usize, code: &[u8]) -> bool {
    // in-bounds
    pc >= code.len()
    // JUMPDEST
        || code[pc] != 0x5b
    // non-push-data JUMPDEST
        || (1..=cmp::min(16, pc)).any(|i| code[pc - i] >= 0x60 + i as u8 - 1 && code[pc - i] < 0x80 )
}

fn resize(memory: &mut Vec<u8>, end: usize) -> () {
    if end > memory.len() {
        memory.resize(((end + 31) / 32) * 32, 0);
    }
}
