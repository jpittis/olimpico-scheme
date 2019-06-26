use std::collections::HashMap;

use super::bytecode::{Chunk, Op, Word};

pub type Env = HashMap<Word, Value>;

#[derive(Debug, Clone)]
pub struct Closure {
    code: usize,
    env: Env,
}

#[derive(Debug, Clone)]
pub enum Value {
    Const(Word),
    Closure(Box<Closure>),
    Env(Box<Env>),
    IP(Word),
}

pub struct Interpreter {
    ip: usize,
    pub env: Env,
    pub stack: Vec<Value>,
    chunk: Chunk,
}

impl Interpreter {
    pub fn new(chunk: Chunk) -> Interpreter {
        Interpreter {
            ip: 0,
            stack: Vec::new(),
            env: HashMap::new(),
            chunk: chunk,
        }
    }

    pub fn execute_all(&mut self, debug: bool) {
        loop {
            if debug {
                println!("{:04X?}\t{:?}", self.ip, self.stack.last());
            }
            if self.execute_next() {
                return;
            }
        }
    }

    fn execute_next(&mut self) -> bool {
        match num::FromPrimitive::from_usize(self.chunk[self.ip]) {
            Some(op) => self.execute_op(op),
            None => panic!("unknown instruction {}", self.chunk[self.ip]),
        }
    }

    fn execute_op(&mut self, op: Op) -> bool {
        match op {
            Op::Return => {
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
            }
            Op::Add => {
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
            Op::Sub => {
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
            Op::Mul => {
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
            Op::Div => {
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
            Op::Mod => {
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
            Op::Equal => {
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
            Op::Apply => {
                let arg = self.stack.pop().unwrap();
                let closure = match self.stack.pop().unwrap() {
                    Value::Closure(closure) => closure,
                    _ => panic!("expected closure"),
                };
                self.stack.push(Value::IP(self.ip + 1));
                self.stack.push(Value::Env(Box::new(self.env.clone())));
                self.ip = closure.code;
                self.env = closure.env.clone();
                self.env.insert(0, arg);
            }
            Op::Stop => return true,
            Op::Const => {
                self.stack.push(Value::Const(self.chunk[self.ip + 1]));
                self.ip += 2;
            }
            Op::Access => {
                let val = self.env.get(&self.chunk[self.ip + 1]).unwrap();
                self.stack.push(val.clone());
                self.ip += 2;
            }
            Op::Closure => {
                let closure = Box::new(Closure {
                    code: self.chunk[self.ip + 1],
                    env: self.env.clone(),
                });
                self.stack.push(Value::Closure(closure));
                self.ip += 2;
            }
            Op::Jump => {
                let arg = match self.stack.pop().unwrap() {
                    Value::Const(one) => one,
                    _ => panic!("expected const"),
                };
                if arg == 0 {
                    self.ip = self.chunk[self.ip + 1];
                } else {
                    self.ip += 2;
                }
            }
        }

        false
    }
}
