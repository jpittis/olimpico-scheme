extern crate num;
#[macro_use]
extern crate num_derive;

#[repr(u8)]
#[derive(FromPrimitive)]
enum OpCode {
    Return = 1,
}

type Chunk = Vec<u8>;

fn dissasembleChunk(chunk: &Chunk) {
    let mut offset = 0;
    while offset < chunk.len() {
        offset = dissasembleIntruction(chunk, offset)
    }
}

fn dissasembleIntruction(chunk: &Chunk, offset: usize) -> usize {
    let instruction = chunk[offset];
    match num::FromPrimitive::from_u8(instruction) {
        Some(OpCode::Return) => simpleInstruction("Return", offset),
        _ => simpleInstruction("Unknown", offset),
    }
}

fn simpleInstruction(name: &str, offset: usize) -> usize {
    println!("{}\n", name);
    return offset + 1;
}

fn main() {
    let mut chunk = Chunk::new();
    chunk.push(OpCode::Return as u8);
    dissasembleChunk(&chunk)
}
