use std::fmt::{self};

#[repr(usize)]
#[derive(FromPrimitive, Debug)]
pub enum Op {
    Return,
    Equal,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Apply,
    Stop,
    Closure,
    Jump,
    Const,
    Access,
}

fn num_args(op: Op) -> usize {
    match op {
        Op::Return => 0,
        Op::Add => 0,
        Op::Sub => 0,
        Op::Div => 0,
        Op::Mul => 0,
        Op::Mod => 0,
        Op::Apply => 0,
        Op::Equal => 0,
        Op::Stop => 0,
        Op::Const => 1,
        Op::Access => 1,
        Op::Closure => 1,
        Op::Jump => 1,
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

pub type Word = usize;

pub type Chunk = Vec<Word>;

pub fn dissasemble_chunk(chunk: &Chunk) {
    let mut offset = 0;
    while offset < chunk.len() {
        offset = dissasemble_next(chunk, offset)
    }
}

fn dissasemble_next(chunk: &Chunk, offset: usize) -> usize {
    match num::FromPrimitive::from_usize(chunk[offset]) {
        Some(op) => dissasemble_args(op, chunk, offset),
        None => panic!("unknown instruction {}", chunk[offset]),
    }
}

fn dissasemble_args(op: Op, chunk: &Chunk, offset: usize) -> usize {
    print!("{:04X?}\t{}", offset, op.to_string().to_uppercase());
    let args = num_args(op);
    let mut i = 1;
    while i <= args {
        print!("\t{:04X?}", chunk[offset + i]);
        i += 1;
    }
    println!("");
    return offset + args + 1;
}
