extern crate num;
#[macro_use]
extern crate num_derive;

use std::collections::HashMap;
use std::fmt::{self};

#[repr(u64)]
#[derive(FromPrimitive, Debug)]
enum OpCode {
    Const,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Access,
    Closure,
    Apply,
    Return,
    Stop,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

type Env = HashMap<u64, Value>;

#[derive(Debug, Clone)]
struct Closure {
    code: usize,
    env: Env,
}

#[derive(Debug, Clone)]
enum Value {
    Const(u64),
    Closure(Box<Closure>),
    Env(Box<Env>),
    IP(usize),
}

type Chunk = Vec<u64>;

struct Interpreter {
    ip: usize,
    stack: Vec<Value>,
    env: Env,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            ip: 0,
            stack: Vec::new(),
            env: HashMap::new(),
        }
    }

    fn run(&mut self, chunk: &Chunk) {
        loop {
            match num::FromPrimitive::from_u64(chunk[self.ip]) {
                Some(OpCode::Return) => {
                    let result = self.stack.pop().unwrap();
                    let env = match self.stack.pop().unwrap() {
                        Value::Env(env) => env,
                        _ => panic!("expected closure"),
                    };
                    let ip = match self.stack.pop().unwrap() {
                        Value::IP(ip) => ip,
                        _ => panic!("expected closure"),
                    };
                    self.env = *env;
                    self.ip = ip;
                    self.stack.push(result);
                    continue;
                }
                Some(OpCode::Add) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(one + two));
                    self.ip += 1;
                }
                Some(OpCode::Sub) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(one - two));
                    self.ip += 1;
                }
                Some(OpCode::Mul) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(one * two));
                    self.ip += 1;
                }
                Some(OpCode::Div) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(one / two));
                    self.ip += 1;
                }
                Some(OpCode::Mod) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(one % two));
                    self.ip += 1;
                }
                Some(OpCode::Apply) => {
                    let arg = self.stack.pop().unwrap();
                    let closure = match self.stack.pop().unwrap() {
                        Value::Closure(closure) => closure,
                        _ => panic!("expected closure"),
                    };
                    self.stack.push(Value::IP(self.ip + 1));
                    self.stack.push(Value::Env(Box::new(self.env.clone())));
                    self.ip = closure.code;
                    self.env = closure.env.clone();
                    self.stack.push(arg);
                }
                Some(OpCode::Stop) => return,
                Some(OpCode::Const) => {
                    self.stack.push(Value::Const(chunk[self.ip + 1]));
                    self.ip += 2;
                }
                Some(OpCode::Access) => {
                    let val = self.env.get(&chunk[self.ip + 1]).unwrap();
                    self.stack.push(val.clone());
                    self.ip += 2;
                }
                Some(OpCode::Closure) => {
                    let closure = Box::new(Closure {
                        code: chunk[self.ip + 1] as usize,
                        env: self.env.clone(),
                    });
                    self.stack.push(Value::Closure(closure));
                    self.ip += 2;
                }
                _ => unimplemented!(),
            }
        }
    }
}

fn dissasemble_chunk(chunk: &Chunk) {
    let mut offset = 0;
    while offset < chunk.len() {
        offset = dissasemble_intruction(chunk, offset)
    }
}

fn dissasemble_intruction(chunk: &Chunk, offset: usize) -> usize {
    let inst = num::FromPrimitive::from_u64(chunk[offset]);
    match inst {
        Some(i @ OpCode::Return) => simple_instruction(i, offset),
        Some(i @ OpCode::Add) => simple_instruction(i, offset),
        Some(i @ OpCode::Sub) => simple_instruction(i, offset),
        Some(i @ OpCode::Div) => simple_instruction(i, offset),
        Some(i @ OpCode::Mul) => simple_instruction(i, offset),
        Some(i @ OpCode::Mod) => simple_instruction(i, offset),
        Some(i @ OpCode::Apply) => simple_instruction(i, offset),
        Some(i @ OpCode::Stop) => simple_instruction(i, offset),
        Some(i @ OpCode::Const) => one_word_instruction(i, offset, chunk),
        Some(i @ OpCode::Access) => one_word_instruction(i, offset, chunk),
        Some(i @ OpCode::Closure) => one_word_instruction(i, offset, chunk),
        None => panic!("unknown instruction {}", chunk[offset]),
    }
}

fn simple_instruction(op: OpCode, offset: usize) -> usize {
    println!("{:04X?}\t{}", offset, op.to_string());
    return offset + 1;
}

fn one_word_instruction(op: OpCode, offset: usize, chunk: &Chunk) -> usize {
    println!(
        "{:04X?}\t{}\t{:04X?}",
        offset,
        op.to_string(),
        chunk[offset + 1]
    );
    return offset + 2;
}

fn main() {
    let mut chunk = Chunk::new();
    chunk.push(OpCode::Closure as u64);
    chunk.push(9);
    chunk.push(OpCode::Const as u64);
    chunk.push(8);
    chunk.push(OpCode::Apply as u64);
    chunk.push(OpCode::Const as u64);
    chunk.push(10);
    chunk.push(OpCode::Mul as u64);
    chunk.push(OpCode::Stop as u64);

    chunk.push(OpCode::Const as u64);
    chunk.push(3);
    chunk.push(OpCode::Add as u64);
    chunk.push(OpCode::Const as u64);
    chunk.push(2);
    chunk.push(OpCode::Const as u64);
    chunk.push(8);
    chunk.push(OpCode::Add as u64);
    chunk.push(OpCode::Add as u64);
    chunk.push(OpCode::Return as u64);
    dissasemble_chunk(&chunk);
    let mut vm = Interpreter::new();
    vm.run(&chunk);
    println!("result: {:?}", vm.stack.pop());
}
