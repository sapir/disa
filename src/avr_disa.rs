use avr_insn::*;


pub struct AvrDisassembler<'a> {
    addr: u32,
    input: &'a [u16],
}

impl<'a> AvrDisassembler<'a> {
    pub fn new(addr: u32, input: &'a [u16]) -> Self {
        AvrDisassembler { addr, input }
    }
}

impl<'a> Iterator for AvrDisassembler<'a> {
    type Item = (u32, AvrInsn);

    fn next(&mut self) -> Option<(u32, AvrInsn)> {
        AvrInsn::decode(self.input).map(|(rest, insn)| {
            self.input = rest;
            let ret = (self.addr, insn);
            self.addr += insn.byte_size() as u32;
            ret
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.input.len()))
    }
}
