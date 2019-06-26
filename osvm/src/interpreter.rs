use std::collections::HashMap;
use std::convert::TryInto;

use super::bytecode::{Chunk, Op, Word};

pub type Env = HashMap<Word, Value>;

#[derive(Debug, Clone)]
pub struct Closure {
    code: u64,
    env: Env,
}

#[derive(Debug, Clone)]
pub enum Value {
    Const(u64),
    Closure(Box<Closure>),
    Env(Box<Env>),
    IP(usize),
}

pub struct Interpreter {
    ip: usize,
    pub stack: Vec<Value>,
    pub env: Env,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            ip: 0,
            stack: Vec::new(),
            env: HashMap::new(),
        }
    }

    pub fn run(&mut self, chunk: &Chunk) {
        loop {
            // println!("{:04X?}\t{:?}", self.ip, self.stack.last());
            match num::FromPrimitive::from_u64(chunk[self.ip]) {
                Some(Op::Return) => {
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
                Some(Op::Add) => {
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
                Some(Op::Sub) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack.push(Value::Const(two - one));
                    self.ip += 1;
                }
                Some(Op::Mul) => {
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
                Some(Op::Div) => {
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
                Some(Op::Mod) => {
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
                Some(Op::Equal) => {
                    let one = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    let two = match self.stack.pop().unwrap() {
                        Value::Const(two) => two,
                        _ => panic!("expected const"),
                    };
                    self.stack
                        .push(Value::Const(if one == two { 1 } else { 0 }));
                    self.ip += 1;
                }
                Some(Op::Apply) => {
                    let arg = self.stack.pop().unwrap();
                    let closure = match self.stack.pop().unwrap() {
                        Value::Closure(closure) => closure,
                        _ => panic!("expected closure"),
                    };
                    self.stack.push(Value::IP(self.ip + 1));
                    self.stack.push(Value::Env(Box::new(self.env.clone())));
                    self.ip = (closure.code as usize).try_into().unwrap();
                    self.env = closure.env.clone();
                    self.env.insert(0, arg);
                }
                Some(Op::Stop) => return,
                Some(Op::Const) => {
                    self.stack.push(Value::Const(chunk[self.ip + 1]));
                    self.ip += 2;
                }
                Some(Op::Access) => {
                    let val = self.env.get(&chunk[self.ip + 1]).unwrap();
                    self.stack.push(val.clone());
                    self.ip += 2;
                }
                Some(Op::Closure) => {
                    let closure = Box::new(Closure {
                        code: chunk[self.ip + 1],
                        env: self.env.clone(),
                    });
                    self.stack.push(Value::Closure(closure));
                    self.ip += 2;
                }
                Some(Op::Jump) => {
                    let arg = match self.stack.pop().unwrap() {
                        Value::Const(one) => one,
                        _ => panic!("expected const"),
                    };
                    if arg == 0 {
                        self.ip = (chunk[self.ip + 1] as usize).try_into().unwrap();
                    } else {
                        self.ip += 2;
                    }
                }
                None => panic!("unknown instruction {}", chunk[self.ip]),
            }
        }
    }
}
