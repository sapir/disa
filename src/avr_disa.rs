use avr_insn::*;


pub struct AvrDisassembler<'a> {
    input: &'a [u16],
}

impl<'a> AvrDisassembler<'a> {
    pub fn new(input: &'a [u16]) -> Self {
        AvrDisassembler { input }
    }
}

impl<'a> Iterator for AvrDisassembler<'a> {
    type Item = AvrInsn;

    fn next(&mut self) -> Option<AvrInsn> {
        AvrInsn::decode(self.input).map(|(rest, insn)| {
            self.input = rest;
            insn
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(self.input.len()))
    }
}
