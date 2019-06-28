extern crate num;
#[macro_use]
extern crate num_derive;
extern crate byteorder;

mod bytecode;
mod interpreter;

use byteorder::{BigEndian, ByteOrder, LittleEndian};
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::time::Instant;

use bytecode::{dissasemble_chunk, Chunk, Op, Word};
use interpreter::Interpreter;

fn main() -> io::Result<()> {
    let mut f = File::open("fib.osb")?;
    let mut buffer = Vec::new();
    f.read_to_end(&mut buffer)?;

    let mut chunk_from_file = Chunk::new();
    for i in (0..buffer.len()).step_by(8) {
        chunk_from_file.push(BigEndian::read_u64(&buffer[i..]) as usize);
    }

    let mut chunk = Chunk::new();

    // Entry Point
    chunk.push(Op::Closure as Word);
    chunk.push(0x0006);
    chunk.push(Op::Const as Word);
    chunk.push(30);
    chunk.push(Op::Apply as Word);
    chunk.push(Op::Stop as Word);

    // Fib
    // If arg is 0, return 0
    chunk.push(Op::Access as Word);
    chunk.push(0);
    chunk.push(Op::Const as Word);
    chunk.push(0);
    chunk.push(Op::Equal as Word);
    chunk.push(Op::Jump as Word);
    chunk.push(0x0010);
    chunk.push(Op::Const as Word);
    chunk.push(0);
    chunk.push(Op::Return as Word);
    // If arg is 1, return r1
    chunk.push(Op::Access as Word);
    chunk.push(0);
    chunk.push(Op::Const as Word);
    chunk.push(1);
    chunk.push(Op::Equal as Word);
    chunk.push(Op::Jump as Word);
    chunk.push(0x001A);
    chunk.push(Op::Const as Word);
    chunk.push(1);
    chunk.push(Op::Return as Word);
    // Call fib with arg sub 1
    chunk.push(Op::Closure as Word);
    chunk.push(0x0006);
    chunk.push(Op::Access as Word);
    chunk.push(0);
    chunk.push(Op::Const as Word);
    chunk.push(1);
    chunk.push(Op::Sub as Word);
    chunk.push(Op::Apply as Word);
    // Call fib with arg sub 2
    chunk.push(Op::Closure as Word);
    chunk.push(0x0006);
    chunk.push(Op::Access as Word);
    chunk.push(0);
    chunk.push(Op::Const as Word);
    chunk.push(2);
    chunk.push(Op::Sub as Word);
    chunk.push(Op::Apply as Word);
    // Add and return
    chunk.push(Op::Add as Word);
    chunk.push(Op::Return as Word);

    assert_eq!(chunk.len(), chunk_from_file.len());
    assert_eq!(chunk, chunk_from_file);

    if std::env::args().len() == 2 {
        let args: Vec<String> = std::env::args().collect();
        let debug = args[1] == "debug";

        println!("======== Run ========");
        let mut vm = Interpreter::new(chunk);

        let now = Instant::now();
        if debug {
            vm.execute_all_debug();
        } else {
            vm.execute_all();
        }
        let duration = now.elapsed();
        println!("Duration: {:?}", duration);

        println!("======= Stack =======");
        for value in vm.stack {
            println!("{:?}", value);
        }

        if vm.env.len() > 0 {
            println!("======== Env ========");
            for (key, value) in vm.env {
                println!("{:?}\t{:?}", key, value);
            }
        }
        println!("=====================");
    } else {
        println!("======= Chunk =======");
        dissasemble_chunk(&chunk);
        println!("=====================");
    }
    Ok(())
}
