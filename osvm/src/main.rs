extern crate num;
#[macro_use]
extern crate num_derive;

mod bytecode;
mod interpreter;

use bytecode::{dissasemble_chunk, Chunk, Op, Word};
use interpreter::Interpreter;

fn main() {
    let mut chunk = Chunk::new();

    // Entry Point
    chunk.push(Op::Closure as Word);
    chunk.push(0x0006);
    chunk.push(Op::Const as Word);
    chunk.push(20);
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

    if std::env::args().len() == 2 {
        let args: Vec<String> = std::env::args().collect();
        let debug = args[1] == "debug";

        println!("======== Run ========");
        let mut vm = Interpreter::new(chunk);
        vm.execute_all(debug);

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
}
