extern crate num;
#[macro_use]
extern crate num_derive;

use std::collections::HashMap;

#[repr(u8)]
#[derive(FromPrimitive)]
enum OpCode {
    Const,
    Add,
    Access,
    Closure,
    Apply,
    Return,
    Stop,
}

type Env = HashMap<u8, Value>;

#[derive(Debug, Clone)]
struct Closure {
    code: usize,
    env: Env,
}

#[derive(Debug, Clone)]
enum Value {
    Const(u8),
    Closure(Box<Closure>),
    Env(Box<Env>),
    IP(usize),
}

type Chunk = Vec<u8>;

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
            match num::FromPrimitive::from_u8(chunk[self.ip]) {
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
                Some(OpCode::Apply) => {
                    // TODO: let arg = self.stack.pop().unwrap();
                    let closure = match self.stack.pop().unwrap() {
                        Value::Closure(closure) => closure,
                        _ => panic!("expected closure"),
                    };
                    self.stack.push(Value::IP(self.ip + 1));
                    self.stack.push(Value::Env(Box::new(self.env.clone())));
                    self.ip = closure.code;
                    self.env = closure.env.clone();
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

fn dissasembleChunk(chunk: &Chunk) {
    let mut offset = 0;
    while offset < chunk.len() {
        offset = dissasembleIntruction(chunk, offset)
    }
}

fn dissasembleIntruction(chunk: &Chunk, offset: usize) -> usize {
    let instruction = chunk[offset];
    match num::FromPrimitive::from_u8(instruction) {
        Some(OpCode::Return) => simpleInstruction("RETURN", offset),
        Some(OpCode::Add) => simpleInstruction("ADD", offset),
        Some(OpCode::Apply) => simpleInstruction("APPLY", offset),
        Some(OpCode::Stop) => simpleInstruction("STOP", offset),
        Some(OpCode::Const) => oneWordInstruction("CONST", chunk[offset + 1], offset),
        Some(OpCode::Access) => oneWordInstruction("ACCESS", chunk[offset + 1], offset),
        Some(OpCode::Closure) => oneWordInstruction("CLOSURE", chunk[offset + 1], offset),
        None => panic!("unknown instruction {}", instruction),
    }
}

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{:x?}\t{}", offset, name);
    return offset + 1;
}

fn oneWordInstruction(name: &str, word: u8, offset: usize) -> usize {
    println!("{:x?}\t{} {}", offset, name, word);
    return offset + 2;
}

fn main() {
    let mut chunk = Chunk::new();
    chunk.push(OpCode::Closure as u8);
    chunk.push(7 as u8);
    chunk.push(OpCode::Apply as u8);
    chunk.push(OpCode::Const as u8);
    chunk.push(10 as u8);
    chunk.push(OpCode::Add as u8);
    chunk.push(OpCode::Stop as u8);

    chunk.push(OpCode::Const as u8);
    chunk.push(7 as u8);
    chunk.push(OpCode::Const as u8);
    chunk.push(3 as u8);
    chunk.push(OpCode::Add as u8);
    chunk.push(OpCode::Const as u8);
    chunk.push(2 as u8);
    chunk.push(OpCode::Const as u8);
    chunk.push(8 as u8);
    chunk.push(OpCode::Add as u8);
    chunk.push(OpCode::Add as u8);
    chunk.push(OpCode::Return as u8);
    dissasembleChunk(&chunk);
    let mut vm = Interpreter::new();
    vm.run(&chunk);
    println!("result: {:?}", vm.stack.pop());
}
