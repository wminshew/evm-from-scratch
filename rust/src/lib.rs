#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(string_remove_matches)]

use ethereum_types::U256;
use sha3::{Digest, Keccak256};
use std::cmp;
use std::ops::{Add, BitAnd, BitOr, BitXor, Not, Shl, Shr};

pub mod block;
pub use crate::block::Block;

pub mod code;
pub use crate::code::Code;

pub mod log;
pub use crate::log::Log;

mod memory;
pub use crate::memory::Memory;

pub mod state;
pub use crate::state::State;

pub mod tx;
pub use crate::tx::Tx;

mod pc;
use crate::pc::PC;

// future improvements
// TODO: use ethereum_types::Address type
// TODO: use opcode enum

#[derive(Debug)]
pub struct EvmResult {
    pub logs: Vec<Log>,
    pub stack: Vec<U256>,
    pub success: bool,
    pub ret: String,
}

pub fn evm(
    _code: impl AsRef<[u8]>,
    tx: &Tx,
    block: &Block,
    state: &mut State,
    writeable: bool,
) -> EvmResult {
    let mut stack: Vec<U256> = Vec::new();
    let mut memory = Memory::new();
    let mut success = true;
    let mut logs: Vec<Log> = Vec::new();
    let mut ret = "".to_string();
    let mut last_return_data: Vec<u8> = Vec::new();

    let code = _code.as_ref();
    let mut pc = PC::new(code);

    while let Some(opcode) = pc.next() {
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
            0x05 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() {
                    stack.push(U256::zero());
                    continue;
                }

                // prolly could improve using abs
                let ( a_neg, a_twos ) = twos_complement(&a);
                let ( b_neg, b_twos ) = twos_complement(&b);
                let result = if a_neg { a_twos } else { a } / if b_neg { b_twos } else { b };
                stack.push(if a_neg ^ b_neg { result.not() + 1 } else { result });
            }
            // MOD
            0x06 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                stack.push(if b.is_zero() { U256::zero() } else { a % b});
            }
            // SMOD
            0x07 if let ( Some(a), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() | (b == U256::MAX) {
                    stack.push(U256::zero());
                    continue;
                }

                let ( a_neg, a_twos ) = twos_complement(&a);
                let ( b_neg, b_twos ) = twos_complement(&b);
                let result = if a_neg { a_twos } else { a } % if b_neg { b_twos } else { b };
                stack.push(if a_neg | b_neg { result.not() + 1 } else { result });
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
                let i = i.as_usize();
                stack.push(U256::from(if i < 32 { x.byte(31 - i) } else { 0 }));
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
                stack.push(U256::from(Keccak256::digest(memory.load(offset.as_usize(), size.as_usize())).as_slice()));
            }
            // ADDRESS
            0x30 => {
                stack.push(tx.to);
            }
            // BALANCE
            0x31 if let Some(address) = stack.pop() => {
                stack.push(state.get_balance(&address));
            }
            // ORIGIN
            0x32 => {
                stack.push(tx.origin);
            }
            // CALLER
            0x33 => {
                stack.push(tx.from);
            }
            // CALLVALUE
            0x34 => {
                stack.push(tx.value);
            }
            // CALLDATALOAD
            0x35 if let Some(i) = stack.pop() => {
                let i = i.as_usize() * 2; // each char is 4-bits, a byte is 8-bits
                stack.push(U256::from_str_radix(&format!("{:0<64}", &tx.data[i..cmp::min(i+64,tx.data.len())]), 16).unwrap());
            }
            // CALLDATASIZE
            0x36 => {
                stack.push(U256::from(tx.data.len() / 2));
            }
            // CALLDATACOPY
            0x37 if let ( Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize() * 2;  // each char is 4-bits, a byte is 8-bits
                let size = size.as_usize() * 2;  // each char is 4-bits, a byte is 8-bits
                if let Ok(data) = hex::decode(&tx.data[offset..offset+size]) {
                    memory.store(dest_offset.as_usize(), &data);
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

                let mut new_mem = vec![0; size];
                let copy_size = cmp::min(size, code.len() - offset);
                new_mem[..copy_size].copy_from_slice(&code[offset..offset+copy_size]);

                memory.store(dest_offset.as_usize(), &new_mem);
            }
            // GASPRICE
            0x3a => {
                stack.push(tx.gasprice);
            }
            // EXTCODESIZE
            0x3b if let Some(address) = stack.pop() => {
                stack.push(
                    U256::from(state.get_code(&address).len())
                )
            }
            // EXTCODECOPY
            0x3c if let ( Some(address), Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize();
                let size = size.as_usize();

                let ext_code = state.get_code(&address);

                let mut new_mem = vec![0; size];
                let copy_size = cmp::min(size, ext_code.len() - offset);
                new_mem[..copy_size].copy_from_slice(&ext_code[offset..offset+copy_size]);

                memory.store(dest_offset.as_usize(), &new_mem);
            }
            // RETURNDATASIZE
            0x3d => {
                stack.push(U256::from(last_return_data.len()));
            }
            // RETURNDATACOPY
            0x3e if let ( Some(dest_offset), Some(offset), Some(size) ) = (stack.pop(), stack.pop(), stack.pop()) => {
                let offset = offset.as_usize();
                let size = size.as_usize();

                let mut new_mem = vec![0; size];
                let copy_size = cmp::min(size, last_return_data.len() - offset);
                new_mem[..copy_size].copy_from_slice(&last_return_data[offset..offset+copy_size]);

                memory.store(dest_offset.as_usize(), &new_mem);
            }
            // EXTCODEHASH
            0x3f if let Some(address) = stack.pop() => {
                let ext_code = state.get_code(&address);
                stack.push(
                    if ext_code.is_empty() {
                        U256::zero()
                    } else {
                        U256::from(Keccak256::digest(ext_code).as_slice())
                    }
                );
            }
            // BLOCKHASH
            0x40 if let Some(_block_number) = stack.pop() => {
                stack.push(U256::zero());
            }
            // COINBASE
            0x41 => {
                stack.push(block.coinbase);
            }
            // TIMESTAMP
            0x42 => {
                stack.push(block.timestamp);
            }
            // NUMBER
            0x43 => {
                stack.push(block.number);
            }
            // DIFFICULTY
            0x44 => {
                stack.push(block.difficulty);
            }
            // GASLIMIT
            0x45 => {
                stack.push(block.gaslimit);
            }
            // CHAINID
            0x46 => {
                stack.push(block.chainid);
            }
            // SELFBALANCE
            0x47 => {
                stack.push(state.get_balance(&tx.to));
            }
            // BASEFEE
            0x48 => {
                stack.push(block.basefee);
            }
            // POP
            0x50 => {
                stack.pop();
            }
            // MLOAD
            0x51 if let Some(offset) = stack.pop() => {
                stack.push(U256::from(memory.load(offset.as_usize(), 32)));
            }
            // MSTORE
            0x52 if let ( Some(offset), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                memory.store(offset.as_usize(), &<[u8; 32]>::from(value));
            }
            // MSTORE8
            0x53 if let ( Some(offset), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                memory.store(offset.as_usize(), &[value.byte(0)]);
            }
            // SLOAD
            0x54 if let Some(key) = stack.pop() => {
                stack.push(state.load(&tx.to, &key));
            }
            // SSTORE
            0x55 if writeable && let ( Some(key), Some(value) ) = ( stack.pop(), stack.pop() ) => {
                state.store(&tx.to, &key, &value);
            }
            // JUMP
            0x56 if let Some(counter) = stack.pop() => {
                if !pc.jump(counter.as_usize()) {
                    success = false;
                    break;
                }
            }
            // JUMPI
            0x57 if let ( Some(counter), Some(b) ) = ( stack.pop(), stack.pop() ) => {
                if b.is_zero() {
                    continue;
                } else if !pc.jump(counter.as_usize()) {
                    success = false;
                    break;
                }
            }
            // PC
            0x58 => {
                stack.push(pc.current());
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
                stack.push(pc.push(push_size));
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
                let address = format!("0x{:x}", &tx.to);

                let data = hex::encode(memory.load(offset.as_usize(), size.as_usize()));

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
                // TODO: look at rlp
                // address = keccak256(rlp([sender_address,sender_nonce]))[12:]
                let mut new_mem = [0u8; 64];
                new_mem[..32].copy_from_slice(&<[u8; 32]>::from(tx.to));
                new_mem[32..].copy_from_slice(&<[u8; 32]>::from(state.get_nonce(&tx.to)));
                let address = U256::try_from(&Keccak256::digest(new_mem).as_slice()[24..]).unwrap();

                let init_code = memory.load(offset.as_usize(), size.as_usize());

                let tx = Tx {
                    to: address.to_owned(),
                    from: tx.to.to_owned(),
                    data: hex::encode(init_code),
                    value,
                    ..tx.to_owned()
                };

                // call
                let result = evm(
                    init_code,
                    &tx,
                    block,
                    state,
                    true,
                );

                last_return_data = hex::decode(&result.ret).unwrap();

                if result.success {
                    state.deploy(&address, &value, &result.ret);
                    stack.push(address);
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

                let tx = &Tx {
                    to: address.to_owned(),
                    from: tx.to.to_owned(),
                    value,
                    data: hex::encode(memory.load(args_offset.as_usize(), args_size.as_usize())),
                    ..tx.to_owned()
                };

                let _code = state.get_code(&address);

                // call
                let result = evm(
                    _code,
                    tx,
                    block,
                    state,
                    true,
                );

                // store result to mem
                last_return_data = hex::decode(&result.ret).unwrap();

                let ret_size = ret_size.as_usize();
                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                memory.store(ret_offset.as_usize(), &new_mem);
                stack.push(U256::from(result.success as u8));
            }
            // RETURN
            0xf3 if let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                ret = hex::encode(memory.load(offset.as_usize(), size.as_usize()));
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

                let tx = &Tx {
                    data: hex::encode(memory.load(args_offset.as_usize(), args_size.as_usize())),
                    ..tx.to_owned()
                };

                let _code = state.get_code(&address);

                // call
                let result = evm(
                    _code,
                    tx,
                    block,
                    state,
                    true,
                );

                // store result to mem
                last_return_data = hex::decode(result.ret).unwrap();

                let ret_size = ret_size.as_usize();
                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                memory.store(ret_offset.as_usize(), &new_mem);
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
                let tx = &Tx {
                    to: address.to_owned(),
                    from: tx.to.to_owned(),
                    data: hex::encode(memory.load(args_offset.as_usize(), args_size.as_usize())),
                    ..tx.to_owned()
                };

                let _code = state.get_code(&address);

                // call
                let result = evm(
                    _code,
                    tx,
                    block,
                    state,
                    false,
                );

                // store result to mem
                last_return_data = hex::decode(&result.ret).unwrap();

                let ret_size = ret_size.as_usize();

                let mut new_mem = vec![0; ret_size];
                let copy_size = cmp::min(ret_size, last_return_data.len());
                new_mem[..copy_size].copy_from_slice(&last_return_data[..copy_size]);

                memory.store(ret_offset.as_usize(), &new_mem);
                stack.push(U256::from(result.success as u8))
            }
            // REVERT
            0xfd if let ( Some(offset), Some(size) ) = ( stack.pop(), stack.pop() ) => {
                ret = hex::encode(memory.load(offset.as_usize(), size.as_usize()));
                success = false;
                break;
            }
            // INVALID
            0xfe => {
                success = false;
                break;
            }
            // SELFDESTRUCT
            0xff if let Some(send_balance_to) = stack.pop() => {
                state.self_destruct(&tx.to, &send_balance_to);
            }
            _ => {
                // revert on non-match (e.g. trying to write in STATICCALL)
                success = false;
                break;
            }
        };
    }

    stack.reverse();

    EvmResult {
        logs,
        stack,
        success,
        ret,
    }
}

fn twos_complement(a: &U256) -> (bool, U256) {
    (a.bit(255), a.not().add(1))
}
